# Candidate localities are the evidence panel: places elsewhere in a dictionary
# whose name resembles the one being worked on. The provider is an option, so
# the application never names a dictionary and can be run against any of them,
# or against none.
#
# The default provider returns nothing. That is not a placeholder: it is the
# state a real dictionary produces for a locality it does not know, and it is
# the state the application must remain usable in, because most of Central
# Africa is not in any gazetteer. See `gazetteer_provider()` for the RAINBIO
# implementation.

#' Columns every candidate provider must return
#'
#' Fixing the contract here means a dictionary can be plugged in later without
#' touching the application: only the provider changes.
#'
#' There is deliberately no `coordinate_uncertainty_m`. A candidate's
#' coordinate is the median of the records filed under that name, not a
#' georeference somebody made; attaching an uncertainty to it would dress a
#' summary statistic up as a measurement. `n_distinct_coords` and `span_deg`
#' state what is actually known -- and where `n_distinct_coords` is 1 or 2,
#' nothing about the spread is known at all.
#'
#' @return A zero-row tibble with the required columns and types.
#'
#' @export
candidates_empty <- function() {
  tibble::tibble(
    candidate_id = character(),
    locality_verbatim = character(),
    country = character(),
    decimal_latitude = numeric(),
    decimal_longitude = numeric(),
    n_records = integer(),
    n_records_georef = integer(),
    n_distinct_coords = integer(),
    span_deg = numeric(),
    n_name_variants = integer(),
    is_georeferenced = logical(),
    coverage = numeric(),
    overlap = numeric(),
    n_matched = integer(),
    source = character()
  )
}

#' A provider that knows no localities
#'
#' The default. Returns no candidates for any query.
#'
#' @return A function with the candidate provider signature.
#'
#' @export
candidates_none <- function() {
  function(locality_key, verbatim = NULL, limit = 10L) candidates_empty()
}

#' Query the configured locality dictionary
#'
#' Looks up places resembling `locality_key` in whatever dictionary is
#' configured. The provider is read from the `georefapp.candidates` option, so
#' a session can point the application at a different dictionary without any
#' change to the application itself.
#'
#' @param locality_key Normalised locality key, from [normalise_locality()].
#' @param verbatim Verbatim locality string, for providers that match on the
#'   original spelling.
#' @param limit Maximum number of candidates to return.
#'
#' @return A tibble shaped like [candidates_empty()], best match first.
#'
#' @export
candidates_query <- function(locality_key, verbatim = NULL, limit = 10L) {
  provider <- getOption("georefapp.candidates", default = candidates_none())
  if (!is.function(provider)) return(candidates_empty())
  out <- tryCatch(
    provider(locality_key = locality_key, verbatim = verbatim, limit = limit),
    error = function(e) {
      # A dictionary that is unreachable must not stop the georeferencing loop;
      # working without candidates is the normal state, not a failure.
      warning("Candidate provider failed: ", conditionMessage(e), call. = FALSE)
      candidates_empty()
    }
  )
  if (!is.data.frame(out)) return(candidates_empty())
  out
}

#' Name of the dictionary snapshot in use
#'
#' Stamped onto every decision so that a georeference records what it was
#' decided against, not merely when.
#'
#' @return A single string, or `NA_character_` when no dictionary is configured.
#'
#' @export
candidates_snapshot <- function() {
  as_chr1(getOption("georefapp.candidates_snapshot", default = NA_character_))
}

#' Combine several candidate providers into one
#'
#' Each dictionary is searched separately and its candidates are listed after
#' those of the providers before it. Results are stacked, not merged and
#' re-ranked: coverage is measured against each dictionary's own vocabulary, so
#' a 100% match in a dictionary of eighty islands and a 100% match in RAINBIO
#' are not the same claim, and sorting them together would pretend they were.
#' The `source` column says which dictionary each candidate came from.
#'
#' A provider that fails is reported and skipped; the others still answer, as
#' the georeferencing loop must never depend on every dictionary being
#' reachable.
#'
#' @param ... Functions with the candidate provider signature, in the order
#'   their candidates should be listed.
#'
#' @return A function with the candidate provider signature. The providers are
#'   attached as the `providers` attribute.
#'
#' @examples
#' \dontrun{
#' p <- candidates_combine(gazetteer_provider("rainbio-gazetteer.sqlite"),
#'                         gazetteer_provider("mandango1982-iles.sqlite"))
#' options(georefapp.candidates = p)
#' }
#'
#' @export
candidates_combine <- function(...) {
  providers <- Filter(is.function, list(...))
  f <- function(locality_key, verbatim = NULL, limit = 10L) {
    parts <- lapply(providers, function(p) {
      tryCatch(
        p(locality_key = locality_key, verbatim = verbatim, limit = limit),
        error = function(e) {
          warning("Candidate provider failed: ", conditionMessage(e), call. = FALSE)
          candidates_empty()
        }
      )
    })
    parts <- Filter(is.data.frame, parts)
    # The tokens a query was reduced to drive the panel's "not searched on"
    # note. They are the first dictionary's, the one listed first.
    query <- if (length(parts)) attr(parts[[1]], "query") else NULL
    found <- Filter(function(x) nrow(x) > 0, parts)
    out <- if (length(found)) {
      dplyr::bind_rows(lapply(found, function(x) x[, names(candidates_empty())]))
    } else {
      candidates_empty()
    }
    attr(out, "query") <- query
    out
  }
  attr(f, "providers") <- providers
  f
}
