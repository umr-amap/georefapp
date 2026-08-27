# Darwin Core terms emitted, in the order GeoPick uses, so that output from the
# two tools is directly comparable and drops into the same downstream pipeline.
dwc_terms <- c(
  "decimalLatitude", "decimalLongitude", "geodeticDatum",
  "coordinateUncertaintyInMeters", "coordinatePrecision",
  "pointRadiusSpatialFit", "footprintWKT", "footprintSRS",
  "georeferencedBy", "georeferencedDate", "georeferenceProtocol",
  "georeferenceSources", "georeferenceRemarks"
)

#' Build the Darwin Core output table
#'
#' Expands the current decisions back out to one row per imported record. This
#' table is derived, never edited: it can be regenerated from the project file
#' at any time, which is what makes a georeferencing session reproducible rather
#' than merely recorded.
#'
#' Records whose locality was marked unresolvable are kept, with empty
#' coordinates and the reason in `georeferenceRemarks`. Dropping them would
#' quietly lose the information that someone looked and could not decide.
#'
#' @param con Connection from [store_open()].
#'
#' @return A tibble with one row per record: the record identifier, the verbatim
#'   locality, the Darwin Core georeference terms, and two `georefapp` columns
#'   tying each row back to the decision it came from.
#'
#' @export
dwc_table <- function(con) {
  recs <- tibble::as_tibble(DBI::dbGetQuery(
    con, "SELECT record_id, locality_key, verbatim_locality, country, admin1 FROM records"
  ))
  if (nrow(recs) == 0) return(dwc_empty())

  # A zero-row query still carries the full column set and its types, so the
  # join below behaves the same whether or not anything has been decided yet.
  dec <- store_current_decisions(con)
  joined <- dplyr::left_join(recs, dec, by = "locality_key", suffix = c("", "_decision"))

  out <- tibble::tibble(
    recordID = joined$record_id,
    verbatimLocality = joined$verbatim_locality,
    country = joined$country,
    decimalLatitude = joined$decimal_latitude,
    decimalLongitude = joined$decimal_longitude,
    geodeticDatum = joined$geodetic_datum,
    coordinateUncertaintyInMeters = joined$coordinate_uncertainty_m,
    coordinatePrecision = joined$coordinate_precision,
    pointRadiusSpatialFit = joined$point_radius_spatial_fit,
    footprintWKT = joined$footprint_wkt,
    footprintSRS = joined$footprint_srs,
    georeferencedBy = joined$georeferenced_by,
    georeferencedDate = joined$georeferenced_date,
    georeferenceProtocol = joined$georeference_protocol,
    georeferenceSources = joined$georeference_sources,
    georeferenceRemarks = joined$georeference_remarks,
    # Not Darwin Core terms, but the thread back to the log. Without these the
    # output cannot be traced to the decision that produced it.
    georefappDecisionID = joined$decision_id,
    georefappDecisionType = joined$decision_type
  )
  out
}

#' Empty Darwin Core table with the right columns
#'
#' @return A zero-row tibble.
#' @noRd
dwc_empty <- function() {
  tibble::tibble(
    recordID = character(), verbatimLocality = character(), country = character(),
    decimalLatitude = numeric(), decimalLongitude = numeric(),
    geodeticDatum = character(), coordinateUncertaintyInMeters = numeric(),
    coordinatePrecision = numeric(), pointRadiusSpatialFit = numeric(),
    footprintWKT = character(), footprintSRS = character(),
    georeferencedBy = character(), georeferencedDate = character(),
    georeferenceProtocol = character(), georeferenceSources = character(),
    georeferenceRemarks = character(), georefappDecisionID = character(),
    georefappDecisionType = character()
  )
}

#' Summarise progress through a project
#'
#' @param con Connection from [store_open()].
#'
#' @return A named list of counts.
#' @noRd
project_progress <- function(con) {
  loc <- store_localities(con)
  list(
    localities = nrow(loc),
    done = sum(loc$status == "done"),
    unresolvable = sum(loc$status == "unresolvable"),
    pending = sum(loc$status == "pending"),
    records = sum(loc$n_records),
    records_done = sum(loc$n_records[loc$status == "done"])
  )
}
