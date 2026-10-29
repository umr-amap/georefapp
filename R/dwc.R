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

#' What each column of the Darwin Core table means
#'
#' One row per column of [dwc_table()], in the same order: whether it is a
#' Darwin Core term or one georefapp adds, the Darwin Core definition, and how
#' georefapp fills it. Written as the `columns` sheet of the Excel export, so
#' the table can be read without this package at hand.
#'
#' @return A tibble with columns `column`, `standard`, `term_iri`,
#'   `definition` and `in_georefapp`.
#'
#' @export
dwc_columns <- function() {
  dwc <- function(column, definition, in_georefapp) {
    tibble::tibble(
      column = column, standard = "Darwin Core",
      term_iri = paste0("http://rs.tdwg.org/dwc/terms/", column),
      definition = definition, in_georefapp = in_georefapp
    )
  }
  own <- function(column, definition, in_georefapp) {
    tibble::tibble(
      column = column, standard = "georefapp", term_iri = NA_character_,
      definition = definition, in_georefapp = in_georefapp
    )
  }
  rbind(
    own("recordID",
        "Identifier of the imported record. Not a Darwin Core term: map it to occurrenceID or catalogNumber, whichever your identifiers are.",
        "The identifier column chosen at import. When none was chosen, or the chosen one was missing or repeated on some rows, row numbers r000001, r000002, ... in the order of the imported file."),
    dwc("verbatimLocality",
        "The original textual description of the place.",
        "The locality text exactly as imported. Records sharing the same text after normalisation share one georeference."),
    dwc("country",
        "The name of the country or major administrative unit in which the location occurs.",
        "Copied from the country column chosen at import; empty when none was chosen. Not used to georeference."),
    dwc("decimalLatitude",
        "The geographic latitude, in decimal degrees, of the geographic center of a location. Positive north of the Equator.",
        "Centre of the smallest circle enclosing the footprint (or a point on the footprint when the coordinate was forced onto it). Empty when the locality is pending or could not be georeferenced."),
    dwc("decimalLongitude",
        "The geographic longitude, in decimal degrees, of the geographic center of a location. Positive east of the Greenwich Meridian.",
        "As decimalLatitude."),
    dwc("geodeticDatum",
        "The ellipsoid, geodetic datum, or spatial reference system upon which the coordinates are based.",
        "Always EPSG:4326 (WGS84)."),
    dwc("coordinateUncertaintyInMeters",
        "The horizontal distance in meters from the given coordinates describing the smallest circle containing the whole of the location.",
        "Radius of the smallest circle enclosing the footprint, measured on the ellipsoid and rounded up to the metre. A bare marker gets the radius set for markers; zero is never written."),
    dwc("coordinatePrecision",
        "A decimal representation of the precision of the coordinates.",
        "Always 0.0000001: coordinates are stored to seven decimal places."),
    dwc("pointRadiusSpatialFit",
        "The ratio of the area of the point-radius to the area of the true spatial representation of the location.",
        "Area of the uncertainty circle divided by the area of the footprint, so 1 or more. Empty when the footprint has no area: a line or a bare marker."),
    dwc("footprintWKT",
        "A Well-Known Text representation of the shape that defines the location.",
        "The shape drawn on the map, or the boundary imported from a GIS file, after markers and circles have been turned into polygons. In the Excel file, a footprint longer than an Excel cell holds (32 767 characters) is replaced by a note: take it from the CSV or the GeoPackage."),
    dwc("footprintSRS",
        "The spatial reference system of the footprintWKT.",
        "Always EPSG:4326 (WGS84)."),
    dwc("georeferencedBy",
        "A list of names of people who determined the georeference.",
        "As typed in Georeferenced by when the decision was saved."),
    dwc("georeferencedDate",
        "The date on which the location was georeferenced.",
        "Date and time the decision was saved, ISO 8601."),
    dwc("georeferenceProtocol",
        "A description or reference to the methods used to determine the spatial footprint, coordinates, and uncertainties.",
        "Generated: whether the footprint was drawn or imported (and from which file and features), that the point-radius method was used with the minimum bounding circle, the centre rule, and the georefapp version."),
    dwc("georeferenceSources",
        "A list of maps, gazetteers, or other resources used to georeference the locality.",
        "As typed in Sources consulted when the decision was saved."),
    dwc("georeferenceRemarks",
        "Notes or comments about the spatial description determination.",
        "As typed in Remarks when the decision was saved; for an unresolvable locality, usually why it could not be georeferenced."),
    own("georefappDecisionID",
        "Identifier of the georeferencing decision this row inherits.",
        "Links the row to the decision log export, which holds every decision including the ones later revised. All records sharing a locality carry the same identifier. Empty while the locality is pending."),
    own("georefappDecisionType",
        "The kind of decision.",
        "drawn: the footprint is an envelope around an uncertain place. area: the footprint is the extent of the locality itself, such as a park. unresolvable: the locality was examined and could not be georeferenced. Empty while pending.")
  )
}

#' Write the Darwin Core table and its column dictionary to Excel
#'
#' Two sheets: `dwc`, the table of [dwc_table()], and `columns`, from
#' [dwc_columns()]. Excel caps a cell at 32 767 characters, and a detailed
#' boundary can exceed that in `footprintWKT`. Such cells are replaced by a
#' note rather than truncated, since a cut WKT is a different shape, or none.
#'
#' @param con Connection from [store_open()].
#' @param file Destination `.xlsx` file. Overwritten.
#'
#' @return `file`, invisibly.
#' @noRd
write_dwc_xlsx <- function(con, file) {
  dat <- dwc_table(con)
  too_long <- !is.na(dat$footprintWKT) & nchar(dat$footprintWKT) > excel_cell_max
  dat$footprintWKT[too_long] <- "(too long for an Excel cell: see the CSV or the GeoPackage)"
  writexl::write_xlsx(list(dwc = dat, columns = dwc_columns()), file)
  invisible(file)
}

excel_cell_max <- 32767L

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
