# A locality can itself be an area: a national park, a plot network, a
# concession. Its footprint is then the answer, not an envelope around an
# uncertain place, and it usually already exists as a GIS file rather than as
# something anyone should trace by hand. This file turns such a file into a
# footprint the rest of the package handles exactly like a drawn one.

#' File extensions accepted as area files
#'
#' A shapefile arrives either zipped or as its component files uploaded
#' together; a KMZ is a zipped KML.
#'
#' @noRd
area_file_extensions <- c(
  ".geojson", ".json", ".kml", ".kmz", ".gpkg", ".zip",
  ".shp", ".shx", ".dbf", ".prj", ".cpg"
)

#' Locate the readable dataset among uploaded files
#'
#' Shiny's `fileInput()` stores each upload under a generated name, which
#' breaks a shapefile: its parts find each other by sharing a base name. The
#' parts are therefore copied back under their original names into one
#' directory. Archives are unpacked the same way.
#'
#' @param paths Paths to the files on disk.
#' @param names Original file names, as the user saw them.
#'
#' @return Path to a single file GDAL can open.
#' @noRd
area_resolve_upload <- function(paths, names = basename(paths)) {
  dir <- tempfile("area-")
  dir.create(dir)
  file.copy(paths, file.path(dir, names))

  for (z in list.files(dir, pattern = "\\.(zip|kmz)$", full.names = TRUE, ignore.case = TRUE)) {
    utils::unzip(z, exdir = file.path(dir, tools::file_path_sans_ext(basename(z))))
  }

  found <- list.files(dir, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.(shp|gpkg|geojson|json|kml)$", ignore.case = TRUE)
  # Resource forks from archives made on a Mac are not data.
  found <- found[!grepl("__MACOSX", found, fixed = TRUE)]
  if (length(found) == 0) {
    stop("No readable spatial file found. Upload a GeoJSON, KML/KMZ, ",
         "GeoPackage, or a shapefile (zipped, or all of its parts together).",
         call. = FALSE)
  }
  if (length(found) > 1) {
    stop("Several spatial files found (", paste(basename(found), collapse = ", "),
         "). Upload one at a time.", call. = FALSE)
  }
  found
}

#' Layers of an area file that can hold polygons
#'
#' @param file Path to a spatial file.
#'
#' @return Character vector of layer names; empty if none holds polygons.
#' @noRd
area_layers <- function(file) {
  ly <- sf::st_layers(file)
  gt <- vapply(ly$geomtype, function(x) paste(x, collapse = " "), character(1))
  # Drivers do not always know the geometry type without reading, and report
  # it as unknown; those layers are kept and judged when read.
  keep <- grepl("polygon", gt, ignore.case = TRUE) | gt %in% c("", "NA", "Unknown (any)", "Geometry")
  ly$name[keep]
}

#' Read the polygon features of an area file
#'
#' @param file Path to a spatial file.
#' @param layer Layer name, or `NULL` for the first layer holding polygons.
#'
#' @return An `sf` object in EPSG:4326 holding only polygonal features, with a
#'   `layer` attribute naming what was read.
#' @noRd
area_read <- function(file, layer = NULL) {
  if (is.null(layer)) {
    layers <- area_layers(file)
    if (length(layers) == 0) stop("The file holds no polygon layer.", call. = FALSE)
    layer <- layers[1]
  }
  x <- sf::st_read(file, layer = layer, quiet = TRUE)
  # GDAL reports a missing CRS two ways: as none at all, or -- in a GeoPackage,
  # which must name one -- as an undefined engineering CRS that nothing can be
  # transformed from.
  crs <- sf::st_crs(x)
  if (is.na(crs) || grepl("^ENGCRS", crs$wkt)) {
    stop("The file declares no coordinate reference system, so its ",
         "coordinates cannot be placed on the map.", call. = FALSE)
  }
  x <- sf::st_zm(x)
  x <- x[as.character(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"), ]
  if (nrow(x) == 0) stop("The layer holds no polygons.", call. = FALSE)
  x <- sf::st_transform(x, 4326)
  attr(x, "layer") <- layer
  x
}

#' Pick the attribute that best names the features of an area file
#'
#' @param x An `sf` object.
#'
#' @return A column name, or `NULL` when there is no text column.
#' @noRd
area_label_column <- function(x) {
  cols <- setdiff(names(x), attr(x, "sf_column"))
  txt <- cols[vapply(sf::st_drop_geometry(x)[cols], is.character, logical(1))]
  if (length(txt) == 0) return(NULL)
  named <- txt[grepl("^(name|nom|nome|label|site)", txt, ignore.case = TRUE)]
  if (length(named)) named[1] else txt[1]
}

#' Turn chosen polygons into one footprint
#'
#' Repairs invalid geometry, unions the features into one, and optionally
#' simplifies it. Simplification is a real choice with a real cost: a boundary
#' of fifty thousand vertices is a megabyte of `footprintWKT`, repeated on every
#' record that inherits it. It is done in a local azimuthal equidistant
#' projection so that the tolerance is in metres, and the tolerance is reported
#' so that it can be written into the protocol.
#'
#' @param geom An `sfc` or `sf` holding polygons.
#' @param tolerance_m Douglas-Peucker tolerance in metres; `0` keeps the
#'   boundary as it is.
#'
#' @return A length-one `sfc` in EPSG:4326 with attributes `n_vertices_in`,
#'   `n_vertices` and `tolerance_m`.
#' @noRd
area_prepare <- function(geom, tolerance_m = 0) {
  g <- sf::st_geometry(geom)
  if (is.na(sf::st_crs(g))) g <- sf::st_set_crs(g, 4326)
  g <- sf::st_transform(sf::st_zm(g), 4326)
  n_in <- nrow(sf::st_coordinates(g))

  # Repaired in the projection, not in degrees. On geographic coordinates sf
  # hands st_make_valid() to s2, which returns a self-intersecting ring
  # unchanged; GEOS on planar coordinates actually splits it.
  bb <- sf::st_bbox(g)
  crs <- aeqd_crs(lon = mean(c(bb[["xmin"]], bb[["xmax"]])),
                  lat = mean(c(bb[["ymin"]], bb[["ymax"]])))
  gp <- sf::st_make_valid(sf::st_transform(g, crs))
  gp <- suppressWarnings(sf::st_collection_extract(gp, "POLYGON"))
  if (length(gp) == 0 || all(sf::st_is_empty(gp))) {
    stop("The selection holds no polygon with an area.", call. = FALSE)
  }
  gp <- sf::st_union(gp)
  tolerance_m <- if (isTRUE(tolerance_m > 0)) tolerance_m else 0
  if (tolerance_m > 0) {
    gp <- sf::st_simplify(gp, preserveTopology = TRUE, dTolerance = tolerance_m)
  }
  out <- sf::st_transform(gp, 4326)

  attr(out, "n_vertices_in") <- n_in
  attr(out, "n_vertices") <- nrow(sf::st_coordinates(out))
  attr(out, "tolerance_m") <- tolerance_m
  out
}

#' Describe where an imported footprint came from
#'
#' The sentence goes into `georeferenceProtocol`, so that someone holding only
#' the Darwin Core table can find the boundary's source and know whether it was
#' altered on the way in.
#'
#' @param file_name Original file name.
#' @param layer Layer read.
#' @param label_column,labels Attribute used to name the chosen features, and
#'   their values; `NULL` when the features had no name.
#' @param prepared Result of [area_prepare()].
#'
#' @return A single string.
#' @noRd
area_origin <- function(file_name, layer, label_column, labels, prepared) {
  what <- sprintf("imported from %s", file_name)
  if (!is.null(layer) && !identical(layer, tools::file_path_sans_ext(file_name))) {
    what <- sprintf("%s, layer '%s'", what, layer)
  }
  if (!is.null(label_column) && length(labels)) {
    what <- sprintf("%s, %s = %s", what, label_column,
                    paste0("'", labels, "'", collapse = " + "))
  }
  tol <- attr(prepared, "tolerance_m")
  if (isTRUE(tol > 0)) {
    what <- sprintf("%s, simplified to %s m (%s to %s vertices)", what,
                    format(tol), format(attr(prepared, "n_vertices_in"), big.mark = ","),
                    format(attr(prepared, "n_vertices"), big.mark = ","))
  }
  what
}
