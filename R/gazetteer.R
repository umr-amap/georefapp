# The gazetteer snapshot: a local, read-only copy of the RAINBIO locality
# dictionary with its inverted index built in.
#
# Searching happens against this file rather than against PostgreSQL. The
# dictionary changes rarely, the app queries it once per locality decided, and
# a georeferencing session should not stall because a remote database is slow
# or unreachable. Holding the snapshot locally also makes a decision auditable:
# `gazetteer_snapshot` on every decision names the exact file the candidates
# came from, so what was on screen can be reconstructed later.

gazetteer_schema <- c(
  "CREATE TABLE IF NOT EXISTS gaz_meta (
     key   TEXT PRIMARY KEY,
     value TEXT
   )",
  "CREATE TABLE IF NOT EXISTS gaz_localities (
     locality_id       INTEGER PRIMARY KEY,
     locality_verbatim TEXT NOT NULL,
     locality_key      TEXT,
     country           TEXT,
     n_records         INTEGER,
     n_records_georef  INTEGER,
     n_distinct_coords INTEGER,
     decimal_latitude  REAL,
     decimal_longitude REAL,
     span_deg          REAL,
     n_name_variants   INTEGER,
     is_georeferenced  INTEGER NOT NULL,
     token_weight      REAL NOT NULL
   )",
  "CREATE TABLE IF NOT EXISTS gaz_tokens (
     token   TEXT PRIMARY KEY,
     df      INTEGER NOT NULL,
     idf     REAL NOT NULL,
     indexed INTEGER NOT NULL
   )",
  "CREATE TABLE IF NOT EXISTS gaz_postings (
     token       TEXT NOT NULL,
     locality_id INTEGER NOT NULL
   )"
)

gazetteer_indexes <- c(
  "CREATE INDEX IF NOT EXISTS idx_gaz_post_token ON gaz_postings (token)",
  "CREATE INDEX IF NOT EXISTS idx_gaz_loc_geo ON gaz_localities (is_georeferenced)"
)

# Columns pulled from rainbio_gazetteer_localities. Requested by name so that a
# widening of the view upstream cannot silently change what the snapshot holds.
gazetteer_source_columns <- c(
  "locality_id", "locality_verbatim", "locality_key", "n_records",
  "n_records_georef", "n_distinct_coords", "decimal_latitude",
  "decimal_longitude", "span_deg", "n_name_variants", "is_georeferenced"
)

#' Default name for a snapshot file
#'
#' Used when a directory is given instead of a file name.
#' @noRd
gazetteer_default_name <- "rainbio-gazetteer.sqlite"

#' Resolve a snapshot destination to a file name
#'
#' `file` follows the base R convention and means a file, but "path" is a
#' natural thing to call it and a directory is a natural thing to pass. Rather
#' than refuse, a directory gets the default file name inside it -- and says so,
#' because a file appearing under a name the caller did not choose should not be
#' silent.
#'
#' @param file A file name, or a directory to write the default name into.
#'
#' @return A file name.
#' @noRd
gazetteer_file <- function(file) {
  file <- as.character(file)[1]
  # A trailing separator means a directory was intended even if it does not
  # exist yet; dirname() would otherwise silently strip the last component and
  # write the snapshot one level up under that component's name.
  trailing <- grepl("[/\\\\]$", file)
  if (dir.exists(file) || trailing) {
    file <- file.path(sub("[/\\\\]+$", "", file), gazetteer_default_name)
    message("Writing to ", file)
  }
  file
}

#' Build a local gazetteer snapshot
#'
#' Reads the locality dictionary once, builds the inverted index over it, and
#' writes both to a SQLite file that the application then uses read-only.
#'
#' Ungeoreferenced localities are included deliberately. A similar name that
#' carries no coordinates is still evidence -- usually that the place is known
#' under a spelling somebody else has already had to interpret -- and hiding it
#' would leave the user believing the dictionary knows nothing about the place.
#' `is_georeferenced` separates the two; only georeferenced localities can be
#' drawn.
#'
#' @param src Either a DBI connection to `plots_transects`, or a data frame
#'   already shaped like [gazetteer_source_columns].
#' @param file Snapshot file to write. A directory is accepted and gets
#'   `rainbio-gazetteer.sqlite` inside it.
#' @param snapshot_id Identifier stamped onto every decision made against this
#'   snapshot. Defaults to the file name plus the build date.
#' @param table Source table or view, when `src` is a connection.
#' @param df_max_frac Tokens present in more than this fraction of the corpus
#'   are not posted; see [build_token_index()].
#' @param overwrite Replace an existing snapshot at `file`.
#'
#' @return The snapshot file, invisibly.
#'
#' @export
gazetteer_snapshot_build <- function(src,
                                     file,
                                     snapshot_id = NULL,
                                     table = "rainbio_gazetteer_localities",
                                     df_max_frac = 0.05,
                                     overwrite = FALSE) {
  # Resolved and checked before the read, not after: fetching and indexing
  # 360,000 localities takes half a minute, and a bad destination should not
  # cost it.
  file <- gazetteer_file(file)
  if (!dir.exists(dirname(file))) {
    stop("No such directory: ", dirname(file), call. = FALSE)
  }
  if (file.exists(file) && !overwrite) {
    stop("A snapshot already exists at ", file, ". Pass overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  loc <- if (is.data.frame(src)) {
    tibble::as_tibble(src)
  } else {
    message("Reading ", table, " ...")
    tibble::as_tibble(DBI::dbGetQuery(src, sprintf(
      "SELECT %s FROM %s", paste(gazetteer_source_columns, collapse = ", "), table
    )))
  }
  missing <- setdiff(gazetteer_source_columns, names(loc))
  if (length(missing)) {
    stop("Source is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  # Not carried by the view. Kept in the schema because a homonym is far easier
  # to dismiss when the country is on screen, and it can be filled from
  # rainbio_gazetteer_occurrences without changing anything downstream.
  if (!"country" %in% names(loc)) loc$country <- NA_character_

  loc$locality_id <- as.integer(loc$locality_id)
  loc$is_georeferenced <- as.integer(as.logical(loc$is_georeferenced))

  # A dictionary entry with no name cannot be searched for and cannot be shown.
  # RAINBIO holds a handful, left behind by records whose locality field was
  # blank; they are dropped rather than carried as unmatchable rows.
  nameless <- is.na(loc$locality_verbatim) | !nzchar(trimws(loc$locality_verbatim))
  if (any(nameless)) {
    message("Dropping ", sum(nameless), " localities with no name.")
    loc <- loc[!nameless, , drop = FALSE]
  }

  message("Indexing ", format(nrow(loc), big.mark = " "), " localities ...")
  idx <- build_token_index(loc$locality_id, loc$locality_verbatim,
                           df_max_frac = df_max_frac)
  loc$token_weight <- idx$weights

  if (file.exists(file) && overwrite) unlink(file)
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (stmt in gazetteer_schema) DBI::dbExecute(con, stmt)

  DBI::dbWriteTable(con, "gaz_localities",
                    as.data.frame(loc[, c(
                      "locality_id", "locality_verbatim", "locality_key", "country",
                      "n_records", "n_records_georef", "n_distinct_coords",
                      "decimal_latitude", "decimal_longitude", "span_deg",
                      "n_name_variants", "is_georeferenced", "token_weight")]),
                    append = TRUE)
  DBI::dbWriteTable(con, "gaz_tokens", idx$tokens, append = TRUE)

  message("Writing ", format(nrow(idx$postings), big.mark = " "), " postings ...")
  DBI::dbWriteTable(con, "gaz_postings", idx$postings, append = TRUE)
  for (stmt in gazetteer_indexes) DBI::dbExecute(con, stmt)

  id <- snapshot_id %||% paste0(tools::file_path_sans_ext(basename(file)), "-",
                                format(Sys.Date(), "%Y%m%d"))
  meta <- data.frame(
    key = c("snapshot_id", "built_at", "source", "n_localities", "n_georeferenced",
            "n_tokens", "n_indexed_tokens", "n_postings", "df_max_frac", "app_version"),
    value = as.character(c(
      id, iso_now(), if (is.data.frame(src)) "data frame" else table,
      nrow(loc), sum(loc$is_georeferenced == 1L), nrow(idx$tokens),
      sum(idx$tokens$indexed == 1L), nrow(idx$postings), df_max_frac, app_version()
    )),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "gaz_meta", meta, append = TRUE)

  message("Snapshot written: ", file)
  invisible(file)
}

#' Open a gazetteer snapshot
#'
#' @param file Path to a file written by [gazetteer_snapshot_build()].
#'
#' @return An open DBI connection. Close it with [gazetteer_close()].
#'
#' @export
gazetteer_open <- function(file) {
  if (!file.exists(file)) stop("No gazetteer snapshot at ", file, call. = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), file, flags = RSQLite::SQLITE_RO)
  if (!DBI::dbExistsTable(con, "gaz_postings")) {
    DBI::dbDisconnect(con)
    stop(file, " is not a gazetteer snapshot.", call. = FALSE)
  }
  con
}

#' @rdname gazetteer_open
#'
#' @param con Connection from [gazetteer_open()].
#'
#' @export
gazetteer_close <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  invisible(TRUE)
}

#' What a snapshot holds
#'
#' @param con Connection from [gazetteer_open()].
#'
#' @return A named character vector of metadata.
#'
#' @export
gazetteer_info <- function(con) {
  m <- DBI::dbGetQuery(con, "SELECT key, value FROM gaz_meta")
  stats::setNames(m$value, m$key)
}

#' Search a gazetteer snapshot for similar locality names
#'
#' Scores every locality sharing at least one indexed token with the query, by
#' the summed inverse document frequency of the tokens they share. Two figures
#' are reported because they answer different questions:
#'
#' * **coverage** -- what fraction of the query's weight the candidate accounts
#'   for. A candidate covering the whole query contains everything distinctive
#'   the query said, whatever else it also says. This is the primary ranking
#'   signal, because the question being asked is "where are the localities that
#'   mention this place", not "which localities are worded like this one".
#' * **overlap** -- the weighted Jaccard index, which does penalise a candidate
#'   for saying more than the query did. It breaks ties, so among candidates
#'   that all contain "Odzala" the plain ones come before the paragraph-long
#'   ones.
#'
#' @param con Connection from [gazetteer_open()].
#' @param text Locality string to search for.
#' @param limit Maximum candidates returned.
#' @param min_coverage Discard candidates accounting for less than this
#'   fraction of the query's weight.
#' @param georeferenced_only Return only candidates that can be drawn.
#'
#' @return A tibble shaped like [candidates_empty()], best first. The tokens
#'   the query was reduced to are attached as the `query` attribute.
#'
#' @export
gazetteer_search <- function(con, text, limit = 20L, min_coverage = 0.34,
                             georeferenced_only = FALSE) {
  empty <- candidates_empty()
  toks <- locality_tokens(text)[[1]]
  if (length(toks) == 0) return(empty)

  vocab <- DBI::dbGetQuery(
    con,
    sprintf("SELECT token, idf, indexed FROM gaz_tokens WHERE token IN (%s)",
            paste(rep("?", length(toks)), collapse = ",")),
    params = as.list(toks)
  )
  q <- weigh_query(toks, vocab)
  attr(empty, "query") <- q
  if (length(q$token) == 0 || q$total <= 0) return(empty)

  values <- paste(rep("(?,?)", length(q$token)), collapse = ",")
  params <- c(
    as.list(as.vector(rbind(q$token, as.character(q$weight)))),
    list(min_coverage * q$total, q$total, q$total, as.integer(limit))
  )

  sql <- sprintf("
    WITH q(token, w) AS (VALUES %s),
    m AS (
      SELECT p.locality_id AS locality_id,
             SUM(q.w)      AS shared_w,
             COUNT(*)      AS n_matched
        FROM q JOIN gaz_postings p ON p.token = q.token
       GROUP BY p.locality_id
    )
    SELECT l.locality_id, l.locality_verbatim, l.country,
           l.decimal_latitude, l.decimal_longitude,
           l.n_records, l.n_records_georef, l.n_distinct_coords,
           l.span_deg, l.n_name_variants, l.is_georeferenced,
           m.shared_w, m.n_matched, l.token_weight
      FROM m JOIN gaz_localities l ON l.locality_id = m.locality_id
     WHERE m.shared_w >= ? %s
     ORDER BY ROUND(m.shared_w / ?, 3) DESC,
              m.shared_w / (? + l.token_weight - m.shared_w) DESC,
              l.n_records DESC
     LIMIT ?", values,
    if (isTRUE(georeferenced_only)) "AND l.is_georeferenced = 1" else "")

  res <- DBI::dbGetQuery(con, sql, params = params)
  if (nrow(res) == 0) return(empty)

  # The query weight is a constant, so SQLite is only asked for the sums; the
  # ratios are clearer computed here.
  out <- tibble::tibble(
    candidate_id = as.character(res$locality_id),
    locality_verbatim = res$locality_verbatim,
    country = res$country,
    decimal_latitude = res$decimal_latitude,
    decimal_longitude = res$decimal_longitude,
    n_records = as.integer(res$n_records),
    n_records_georef = as.integer(res$n_records_georef),
    n_distinct_coords = as.integer(res$n_distinct_coords),
    span_deg = res$span_deg,
    n_name_variants = as.integer(res$n_name_variants),
    is_georeferenced = res$is_georeferenced == 1L,
    coverage = res$shared_w / q$total,
    overlap = res$shared_w / (q$total + res$token_weight - res$shared_w),
    n_matched = as.integer(res$n_matched),
    source = "rainbio"
  )
  attr(out, "query") <- q
  out
}

#' A candidate provider backed by a gazetteer snapshot
#'
#' Wraps [gazetteer_search()] in the provider contract and sets
#' `georefapp.candidates_snapshot` from the snapshot's own metadata, so that
#' every decision records which dictionary it was made against.
#'
#' @param file Path to a snapshot file.
#' @param ... Passed to [gazetteer_search()], e.g. `min_coverage`.
#'
#' @return A function with the candidate provider signature. The open
#'   connection is attached as the `con` attribute; close it with
#'   [gazetteer_close()] when the session ends.
#'
#' @examples
#' \dontrun{
#' p <- gazetteer_provider("rainbio-gazetteer.sqlite")
#' options(georefapp.candidates = p)
#' }
#'
#' @export
gazetteer_provider <- function(file, ...) {
  con <- gazetteer_open(file)
  options(georefapp.candidates_snapshot = unname(gazetteer_info(con)["snapshot_id"]))
  f <- function(locality_key, verbatim = NULL, limit = 10L) {
    text <- if (!is.null(verbatim) && !is.na(verbatim) && nzchar(verbatim))
      verbatim else locality_key
    gazetteer_search(con, text, limit = limit, ...)
  }
  attr(f, "con") <- con
  f
}
