# The project store is append-only by discipline: decisions are only ever
# INSERTed. Revising a georeference writes a new row pointing at the one it
# replaces, so the full history of how a locality was interpreted survives in
# the file. Nothing in this package issues UPDATE or DELETE against `decisions`.

store_schema <- c(
  "CREATE TABLE IF NOT EXISTS project_meta (
     key   TEXT PRIMARY KEY,
     value TEXT
   )",
  "CREATE TABLE IF NOT EXISTS records (
     record_id         TEXT PRIMARY KEY,
     locality_key      TEXT NOT NULL,
     verbatim_locality TEXT NOT NULL,
     country           TEXT,
     admin1            TEXT,
     extra             TEXT
   )",
  "CREATE INDEX IF NOT EXISTS idx_records_key ON records (locality_key)",
  "CREATE TABLE IF NOT EXISTS decisions (
     decision_id              TEXT PRIMARY KEY,
     locality_key             TEXT NOT NULL,
     verbatim_locality        TEXT,
     decision_type            TEXT NOT NULL,
     footprint_wkt            TEXT,
     footprint_srs            TEXT,
     decimal_latitude         REAL,
     decimal_longitude        REAL,
     coordinate_uncertainty_m REAL,
     coordinate_precision     REAL,
     point_radius_spatial_fit REAL,
     geodetic_datum           TEXT,
     centre_rule              TEXT,
     georeferenced_by         TEXT,
     georeferenced_date       TEXT,
     georeference_protocol    TEXT,
     georeference_sources     TEXT,
     georeference_remarks     TEXT,
     supersedes               TEXT,
     app_version              TEXT,
     gazetteer_snapshot       TEXT,
     created_at               TEXT NOT NULL
   )",
  "CREATE INDEX IF NOT EXISTS idx_decisions_key ON decisions (locality_key)",
  "CREATE INDEX IF NOT EXISTS idx_decisions_sup ON decisions (supersedes)"
)

decision_columns <- c(
  "decision_id", "locality_key", "verbatim_locality", "decision_type",
  "footprint_wkt", "footprint_srs", "decimal_latitude", "decimal_longitude",
  "coordinate_uncertainty_m", "coordinate_precision", "point_radius_spatial_fit",
  "geodetic_datum", "centre_rule", "georeferenced_by", "georeferenced_date",
  "georeference_protocol", "georeference_sources", "georeference_remarks",
  "supersedes", "app_version", "gazetteer_snapshot", "created_at"
)

#' Open a project store, creating it if needed
#'
#' A project is a single SQLite file holding the imported records, the
#' append-only decision log and the import metadata. The file is the unit of
#' reproducibility: given it alone, the Darwin Core output can be regenerated
#' exactly and every step audited.
#'
#' @param path Path to the project file.
#'
#' @return An open DBI connection. Close it with [store_close()].
#'
#' @export
store_open <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  for (stmt in store_schema) DBI::dbExecute(con, stmt)
  if (is.na(store_meta_get(con, "created_at"))) {
    store_meta_set(con, "created_at", iso_now())
    store_meta_set(con, "app_version", app_version())
  }
  con
}

#' Close a project store
#'
#' @param con Connection from [store_open()].
#'
#' @return `TRUE`, invisibly.
#'
#' @export
store_close <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  invisible(TRUE)
}

#' Read a project metadata value
#'
#' @param con Connection from [store_open()].
#' @param key Metadata key.
#'
#' @return A single string, or `NA_character_` if the key is absent.
#' @noRd
store_meta_get <- function(con, key) {
  res <- DBI::dbGetQuery(con, "SELECT value FROM project_meta WHERE key = ?", params = list(key))
  if (nrow(res) == 0) NA_character_ else res$value[1]
}

#' Write a project metadata value
#'
#' @param con Connection from [store_open()].
#' @param key,value Metadata key and value.
#'
#' @return `TRUE`, invisibly.
#' @noRd
store_meta_set <- function(con, key, value) {
  DBI::dbExecute(
    con,
    "INSERT INTO project_meta (key, value) VALUES (?, ?)
       ON CONFLICT(key) DO UPDATE SET value = excluded.value",
    params = list(key, as.character(value))
  )
  invisible(TRUE)
}

#' Write the imported records into a project
#'
#' Replaces any previously imported records. Decisions are untouched, so
#' re-importing a corrected source table keeps every georeference already made
#' for locality keys that still exist.
#'
#' @param con Connection from [store_open()].
#' @param records A data frame with columns `record_id`, `locality_key`,
#'   `verbatim_locality` and optionally `country`, `admin1`, `extra`.
#'
#' @return The number of rows written, invisibly.
#'
#' @export
store_write_records <- function(con, records) {
  stopifnot(all(c("record_id", "locality_key", "verbatim_locality") %in% names(records)))
  for (col in c("country", "admin1", "extra")) {
    if (is.null(records[[col]])) records[[col]] <- NA_character_
  }
  records <- records[, c("record_id", "locality_key", "verbatim_locality",
                         "country", "admin1", "extra"), drop = FALSE]
  DBI::dbExecute(con, "DELETE FROM records")
  DBI::dbAppendTable(con, "records", as.data.frame(records))
  invisible(nrow(records))
}

#' Current georeference for every locality
#'
#' A decision is current when nothing supersedes it. Expressing "current" as a
#' property of the lineage, rather than as the most recent timestamp, means the
#' log can be reordered or merged without changing which decision counts.
#'
#' @param con Connection from [store_open()].
#'
#' @return A data frame of decisions, one row per locality that has been acted
#'   on.
#'
#' @export
store_current_decisions <- function(con) {
  tibble::as_tibble(DBI::dbGetQuery(
    con,
    "SELECT d.* FROM decisions d
      WHERE NOT EXISTS (SELECT 1 FROM decisions s WHERE s.supersedes = d.decision_id)"
  ))
}

#' The complete decision log
#'
#' Every decision ever recorded, superseded ones included. This is the audit
#' trail, and it is what should accompany a published dataset.
#'
#' @param con Connection from [store_open()].
#'
#' @return A data frame ordered by locality then time.
#'
#' @export
store_decisions <- function(con) {
  tibble::as_tibble(DBI::dbGetQuery(
    con, "SELECT * FROM decisions ORDER BY locality_key, created_at"
  ))
}

#' Record a georeferencing decision
#'
#' Appends one immutable row. If `supersedes` names an earlier decision, that
#' decision stops being current but stays in the file.
#'
#' @param con Connection from [store_open()].
#' @param locality_key Normalised key the decision applies to.
#' @param decision_type One of `"drawn"`, `"adopted"` or `"unresolvable"`.
#' @param metrics Result of [georef_metrics()], or `NULL` for an unresolvable
#'   locality.
#' @param verbatim_locality Representative verbatim string, so that the log
#'   reads on its own.
#' @param georeferenced_by Person accountable for the decision.
#' @param georeference_sources What was consulted to reach it.
#' @param georeference_remarks Free-text reasoning.
#' @param supersedes `decision_id` this one replaces, or `NA`.
#' @param gazetteer_snapshot Identifier of the locality dictionary consulted, or
#'   `NA` when none was available.
#'
#' @return The new `decision_id`, invisibly.
#'
#' @export
store_add_decision <- function(con,
                               locality_key,
                               decision_type = c("drawn", "adopted", "unresolvable"),
                               metrics = NULL,
                               verbatim_locality = NA_character_,
                               georeferenced_by = NA_character_,
                               georeference_sources = NA_character_,
                               georeference_remarks = NA_character_,
                               supersedes = NA_character_,
                               gazetteer_snapshot = NA_character_) {
  decision_type <- match.arg(decision_type)
  id <- new_id()
  now <- iso_now()

  protocol <- if (is.null(metrics)) {
    NA_character_
  } else {
    sprintf(
      "Point-radius from drawn footprint; minimum bounding circle, centre rule '%s'; %s",
      metrics$centre_rule, app_version()
    )
  }

  row <- data.frame(
    decision_id = id,
    locality_key = locality_key,
    verbatim_locality = as_chr1(verbatim_locality),
    decision_type = decision_type,
    footprint_wkt = if (is.null(metrics)) NA_character_ else metrics$footprint_wkt,
    footprint_srs = if (is.null(metrics)) NA_character_ else metrics$footprint_srs,
    decimal_latitude = if (is.null(metrics)) NA_real_ else metrics$decimal_latitude,
    decimal_longitude = if (is.null(metrics)) NA_real_ else metrics$decimal_longitude,
    coordinate_uncertainty_m = if (is.null(metrics)) NA_real_ else metrics$coordinate_uncertainty_m,
    coordinate_precision = if (is.null(metrics)) NA_real_ else metrics$coordinate_precision,
    point_radius_spatial_fit = if (is.null(metrics)) NA_real_ else metrics$point_radius_spatial_fit,
    geodetic_datum = if (is.null(metrics)) NA_character_ else metrics$geodetic_datum,
    centre_rule = if (is.null(metrics)) NA_character_ else metrics$centre_rule,
    georeferenced_by = as_chr1(georeferenced_by),
    georeferenced_date = now,
    georeference_protocol = protocol,
    georeference_sources = as_chr1(georeference_sources),
    georeference_remarks = as_chr1(georeference_remarks),
    supersedes = as_chr1(supersedes),
    app_version = app_version(),
    gazetteer_snapshot = as_chr1(gazetteer_snapshot),
    created_at = now,
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, "decisions", row[, decision_columns, drop = FALSE])
  invisible(id)
}

#' Pick the spelling that stands for a group of locality strings
#'
#' The most frequent spelling, and among equally frequent ones the shortest.
#' Ties are common in small groups, and the shortest form is the one with the
#' hedges and qualifiers stripped, which is what the place is actually called.
#'
#' @param x Character vector of verbatim locality strings.
#'
#' @return A single string.
#' @noRd
representative_verbatim <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  counts <- table(x)
  best <- names(counts)[counts == max(counts)]
  best[order(nchar(best), best)][1]
}

#' The working list of distinct localities
#'
#' The unit of work in this application is the distinct locality string, not the
#' record: a table of three thousand specimens typically holds a few hundred
#' distinct places, each of which needs deciding once.
#'
#' @param con Connection from [store_open()].
#'
#' @return A data frame with one row per locality key: the most frequent
#'   verbatim spelling, the number of records behind it, its status, and the
#'   current decision where one exists.
#'
#' @export
store_localities <- function(con) {
  # Records with no locality text stay in the store and in the Darwin Core
  # output, but there is nothing to draw for them, so they are not offered as
  # work.
  recs <- tibble::as_tibble(DBI::dbGetQuery(
    con, "SELECT * FROM records WHERE locality_key <> ?", params = list(no_locality_key)
  ))
  if (nrow(recs) == 0) {
    return(tibble::tibble(
      locality_key = character(), verbatim_locality = character(),
      n_records = integer(), country = character(),
      decision_id = character(), decision_type = character(),
      decimal_latitude = numeric(), decimal_longitude = numeric(),
      coordinate_uncertainty_m = numeric(), status = character()
    ))
  }

  loc <- dplyr::summarise(
    dplyr::group_by(recs, .data$locality_key),
    verbatim_locality = representative_verbatim(.data$verbatim_locality),
    n_records = dplyr::n(),
    country = as_chr1(stats::na.omit(.data$country)[1]),
    .groups = "drop"
  )

  dec <- store_current_decisions(con)
  if (nrow(dec) > 0) {
    dec <- dec[, c("locality_key", "decision_id", "decision_type",
                   "decimal_latitude", "decimal_longitude",
                   "coordinate_uncertainty_m"), drop = FALSE]
    loc <- dplyr::left_join(loc, dec, by = "locality_key")
  } else {
    loc$decision_id <- NA_character_
    loc$decision_type <- NA_character_
    loc$decimal_latitude <- NA_real_
    loc$decimal_longitude <- NA_real_
    loc$coordinate_uncertainty_m <- NA_real_
  }

  loc$status <- ifelse(
    is.na(loc$decision_type), "pending",
    ifelse(loc$decision_type == "unresolvable", "unresolvable", "done")
  )
  # Ordered by how many records ride on each locality, so the most consequential
  # decisions come first. The order does not depend on status: rows must not
  # jump around as they are decided, or the row a user just selected would move
  # under them.
  dplyr::arrange(loc, dplyr::desc(.data$n_records), .data$verbatim_locality)
}
