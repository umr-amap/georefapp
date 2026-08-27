#' Build a local azimuthal equidistant CRS
#'
#' All metric work is done in an azimuthal equidistant projection centred on the
#' footprint. That projection preserves distance from its origin exactly, which
#' is precisely the quantity a point-radius uncertainty is made of, and it keeps
#' every planar GEOS operation valid at the scale of a single locality.
#'
#' @param lon,lat Origin of the projection, in decimal degrees.
#'
#' @return A proj4 string.
#' @noRd
aeqd_crs <- function(lon, lat) {
  sprintf(
    "+proj=aeqd +lat_0=%.10f +lon_0=%.10f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    lat, lon
  )
}

#' Convert leafpm drawn features into a geometry set
#'
#' leafpm serialises a drawn circle as a GeoJSON Point carrying a `radius`
#' property in metres. The radius is carried out of band, as a `radius`
#' attribute, so that buffering can happen later in a metric projection rather
#' than in degrees.
#'
#' @param features List of GeoJSON features as received from the leafpm Shiny
#'   inputs.
#'
#' @return An `sfc` in EPSG:4326 with a numeric `radius` attribute, one element
#'   per geometry (zero where the feature carries no radius). `NULL` if
#'   `features` is empty.
#' @noRd
draw_features_to_sfc <- function(features) {
  features <- Filter(Negate(is.null), features)
  if (length(features) == 0) return(NULL)
  geoms <- list()
  radii <- numeric(0)
  for (f in features) {
    parsed <- sf::read_sf(jsonlite::toJSON(f, force = TRUE, auto_unbox = TRUE, digits = NA))
    geoms <- c(geoms, list(sf::st_geometry(parsed)))
    r <- f$properties$radius
    radii <- c(radii, if (is.numeric(r) && length(r) == 1 && !is.na(r)) r else 0)
  }
  out <- do.call(c, geoms)
  out <- sf::st_set_crs(out, 4326)
  attr(out, "radius") <- radii
  out
}

#' Test whether geometries are zero-dimensional
#'
#' @param g An `sfc`.
#'
#' @return A logical vector.
#' @noRd
is_point_geom <- function(g) {
  as.character(sf::st_geometry_type(g)) %in% c("POINT", "MULTIPOINT")
}
