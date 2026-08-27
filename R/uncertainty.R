# Buffers approximate a circle with a polygon; 180 segments per quadrant keeps
# the radius error below a millimetre at Central African scales, so the
# reported uncertainty is not quietly shaved by the approximation.
buffer_segments <- 180L

#' Derive a point-radius georeference from a drawn footprint
#'
#' Implements the point-radius method: the reported coordinate is the centre of
#' the smallest circle enclosing the whole footprint, and the reported
#' uncertainty is that circle's radius. Working in a local azimuthal
#' equidistant projection makes both quantities exact in metres.
#'
#' Two centre rules are available. `"mbc"` uses the centre of the minimum
#' bounding circle, which yields the smallest possible uncertainty and is the
#' default. `"inside"` forces the coordinate to lie on the footprint itself,
#' which matters for a concave shape such as a river bend or a coastline, where
#' the bounding-circle centre can fall outside the feature; it necessarily
#' produces a larger radius, since the radius is then measured from a point that
#' is not optimally placed.
#'
#' @param geom An `sfc` or `sf` object holding one or more drawn geometries. A
#'   `radius` attribute, as produced by drawing circles, is honoured.
#' @param point_radius_m Radius in metres applied to any zero-dimensional
#'   geometry that does not already carry its own radius. A bare point with no
#'   radius describes a place with no stated extent, which is rarely honest, so
#'   this is how a marker becomes a usable georeference.
#' @param centre Either `"mbc"` or `"inside"`; see details.
#'
#' @return A list with elements `decimal_latitude`, `decimal_longitude`,
#'   `coordinate_uncertainty_m`, `coordinate_precision`,
#'   `point_radius_spatial_fit`, `footprint_wkt`, `footprint_srs`,
#'   `geodetic_datum` and `centre_rule`. Returns `NULL` for empty input.
#'
#' @examples
#' pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
#' m <- georef_metrics(pt, point_radius_m = 5000)
#' m$coordinate_uncertainty_m
#'
#' @export
georef_metrics <- function(geom, point_radius_m = 0, centre = c("mbc", "inside")) {
  centre <- match.arg(centre)
  if (is.null(geom)) return(NULL)

  radius_attr <- attr(geom, "radius")
  g <- sf::st_geometry(geom)
  if (length(g) == 0) return(NULL)
  if (is.na(sf::st_crs(g))) g <- sf::st_set_crs(g, 4326)
  g <- sf::st_transform(g, 4326)

  if (is.null(radius_attr) || length(radius_attr) != length(g)) {
    radius_attr <- rep(0, length(g))
  }

  # An azimuthal equidistant projection preserves distance from its origin, and
  # nowhere else. The work is therefore done twice. The first pass, centred on
  # the bounding box, is only good enough to decide where the coordinate goes;
  # the second is centred on that coordinate, which puts the reported centre
  # exactly at the projection origin and makes the radius measured from it a
  # true distance rather than an approximation.
  bb <- sf::st_bbox(g)
  crs_bbox <- aeqd_crs(
    lon = mean(c(bb[["xmin"]], bb[["xmax"]])),
    lat = mean(c(bb[["ymin"]], bb[["ymax"]]))
  )
  is_pt <- is_point_geom(g)

  provisional <- build_footprint(g, crs_bbox, radius_attr, is_pt, point_radius_m)
  ctr_provisional <- footprint_centre(provisional, centre)
  ctr_ll <- sf::st_coordinates(sf::st_transform(ctr_provisional, 4326))

  crs_centre <- aeqd_crs(lon = ctr_ll[1, "X"], lat = ctr_ll[1, "Y"])
  footprint <- build_footprint(g, crs_centre, radius_attr, is_pt, point_radius_m)

  # In this projection the reported centre is the origin, so the enclosing
  # radius is simply the largest coordinate magnitude among the footprint's
  # vertices. Rounded to the millimetre first, so floating-point noise cannot
  # inflate a clean 5000 m radius to 5001 m.
  xy <- sf::st_coordinates(footprint)
  radius_m <- round(max(sqrt(xy[, "X"]^2 + xy[, "Y"]^2)), 3)

  fit <- spatial_fit(radius_m, as.numeric(sf::st_area(footprint)))

  fp_ll <- sf::st_transform(footprint, 4326)

  list(
    decimal_latitude = round(unname(ctr_ll[1, "Y"]), 7),
    decimal_longitude = round(unname(ctr_ll[1, "X"]), 7),
    coordinate_uncertainty_m = ceiling(radius_m),
    coordinate_precision = 1e-7,
    point_radius_spatial_fit = fit,
    footprint_wkt = sf::st_as_text(fp_ll),
    footprint_srs = "EPSG:4326",
    geodetic_datum = "EPSG:4326",
    centre_rule = centre
  )
}

#' Project the drawn geometries and buffer whatever needs a radius
#'
#' @param g Geometries in EPSG:4326.
#' @param crs Target projection.
#' @param radius_attr Per-geometry radius carried from a drawn circle.
#' @param is_pt Which geometries are zero-dimensional.
#' @param point_radius_m Radius applied to points that carry none of their own.
#'
#' @return A single unioned geometry in `crs`.
#' @noRd
build_footprint <- function(g, crs, radius_attr, is_pt, point_radius_m) {
  gp <- sf::st_transform(g, crs)
  parts <- lapply(seq_along(gp), function(i) {
    r <- if (radius_attr[i] > 0) radius_attr[i] else if (is_pt[i]) point_radius_m else 0
    if (r > 0) sf::st_buffer(gp[i], dist = r, nQuadSegs = buffer_segments) else gp[i]
  })
  sf::st_union(do.call(c, parts))
}

#' Locate the reported coordinate within a footprint
#'
#' @param footprint A projected geometry.
#' @param centre Either `"mbc"` or `"inside"`.
#'
#' @return A length-one `sfc` point.
#' @noRd
footprint_centre <- function(footprint, centre) {
  if (identical(centre, "inside")) {
    sf::st_point_on_surface(footprint)
  } else {
    sf::st_centroid(sf::st_minimum_bounding_circle(footprint))
  }
}

#' Darwin Core pointRadiusSpatialFit
#'
#' The ratio of the area of the reported circle to the area of the footprint it
#' stands for. Darwin Core defines the value as 1 for an exact match and
#' undefined when the footprint has no area, so a line or an unbuffered point
#' yields `NA`.
#'
#' Darwin Core also defines 0 for a circle that fails to contain the footprint.
#' That cannot arise here: the radius is taken as the largest distance from the
#' reported centre to any footprint vertex, and the farthest point of a polygon
#' or line from any centre is always a vertex, so containment holds by
#' construction under both centre rules.
#'
#' @param radius_m Reported radius in metres.
#' @param area_fp Footprint area in square metres.
#'
#' @return A single number, or `NA_real_` where the value is undefined.
#' @noRd
spatial_fit <- function(radius_m, area_fp) {
  if (isTRUE(area_fp <= 0)) {
    # A point reported as a point with no radius is an exact match; anything
    # else without area has no defined ratio.
    return(if (radius_m == 0) 1 else NA_real_)
  }
  round(pi * radius_m^2 / area_fp, 4)
}
