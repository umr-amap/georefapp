# Key given to records whose locality text is blank. They are real records and
# belong in the output, but there is nothing to georeference, so they are kept
# out of the working list rather than presented as a task with no content. The
# parentheses make it unreachable by normalisation, which emits only lowercase
# alphanumerics and spaces, so no real locality can collide with it.
no_locality_key <- "(none)"

# Leading qualifiers that hedge a locality without changing which place is
# meant. Stripped so that "env. Yangambi" and "Yangambi" become one unit of
# work. Anything that displaces the locality ("5 km N of ...") is deliberately
# NOT stripped, because it does change the place.
vague_prefixes <- c(
  "env", "envs", "environs", "environs de", "ca", "circa", "approx",
  "approximately", "about", "pres de", "pres", "near", "nr", "vers",
  "autour de", "aux environs de", "region de", "region of", "around"
)

#' Normalise a locality string into a grouping key
#'
#' Produces the key used to decide that two records refer to the same place and
#' may therefore share one georeferencing decision. The transformation is
#' deliberately conservative: it removes noise (case, accents, punctuation,
#' hedging prefixes) but never removes anything that displaces the locality,
#' such as a distance or a bearing.
#'
#' The verbatim string is always kept alongside the key and is what gets
#' exported as `verbatimLocality`; the key is an internal grouping device only.
#'
#' @param x Character vector of locality descriptions.
#'
#' @return A character vector of normalised keys, the same length as `x`.
#'   Empty or missing input yields `NA_character_`.
#'
#' @examples
#' normalise_locality(c("Env. Yangambi", "yangambi", "5 km N of Yangambi"))
#'
#' @export
normalise_locality <- function(x) {
  x <- as.character(x)
  # Punctuation becomes space before prefix stripping, so that "env." and "env"
  # collapse to the same token. Shared with the tokeniser, which has to split on
  # exactly the same boundaries for a key and its tokens to agree.
  out <- normalise_chars(x)
  pattern <- paste0("^(", paste(vague_prefixes, collapse = "|"), ")[[:space:]]+")
  # Applied repeatedly: "environs de ca yangambi" needs more than one pass.
  repeat {
    stripped <- sub(pattern, "", out)
    if (identical(stripped, out)) break
    out <- stripped
  }
  out[is.na(x) | !nzchar(out)] <- NA_character_
  out
}
