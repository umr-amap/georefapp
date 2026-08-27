#' Version string used to stamp every decision
#'
#' @return A single string, the installed package version.
#' @noRd
app_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("georefapp")), error = function(e) "unknown")
  paste0("georefapp ", v)
}

#' Current time as an ISO 8601 UTC string
#'
#' Decisions are stamped in UTC so that a project file remains unambiguous when
#' it moves between machines.
#'
#' @return A single string, e.g. `"2026-08-26T14:03:11Z"`.
#' @noRd
iso_now <- function() {
  format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

#' Generate an opaque identifier for a decision
#'
#' @return A single 16-character hexadecimal string.
#' @noRd
new_id <- function() {
  paste(format(as.hexmode(sample.int(65535L, 4L, replace = TRUE)), width = 4L), collapse = "")
}

#' Base leaflet map used throughout the application
#'
#' Three base layers are offered because they fail differently over Central
#' Africa: OSM has the place names, satellite imagery has the rivers and forest
#' edges that locality descriptions actually refer to, and the topographic layer
#' has the relief.
#'
#' @param ... Passed to [leaflet::leaflet()].
#'
#' @return A leaflet map.
#' @noRd
base_map <- function(...) {
  leaflet::leaflet(...) |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") |>
    leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Topographic") |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "Satellite", "Topographic"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::setView(lng = 18, lat = 0, zoom = 4)
}

#' Coerce a value to a length-one string, mapping empty input to NA
#'
#' @param x Value to coerce.
#'
#' @return A single string, or `NA_character_`.
#' @noRd
as_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
}
