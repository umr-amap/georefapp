#' Footprints of the current decisions, as GIS layers
#'
#' One feature per current decision that has a footprint, carrying the
#' coordinate, uncertainty and provenance of the decision and the number of
#' records that inherit it. Like [dwc_table()], this is derived from the
#' project file and never stored, so it can be regenerated at any time.
#'
#' Footprints are split by dimension, because a single layer holding both
#' polygons and lines is awkward in most GIS software: `polygons` holds areas,
#' circles and drawn polygons; `lines` holds footprints drawn as lines. A
#' decision whose footprint mixes the two appears in both.
#'
#' @param con Connection from [store_open()].
#'
#' @return A list with elements `polygons` and `lines`, each an `sf` object in
#'   EPSG:4326, possibly with zero rows.
#'
#' @export
dwc_footprints <- function(con) {
  dec <- store_current_decisions(con)
  dec <- dec[!is.na(dec$footprint_wkt), , drop = FALSE]

  n <- DBI::dbGetQuery(
    con, "SELECT locality_key, COUNT(*) AS n_records FROM records GROUP BY locality_key"
  )
  dec$n_records <- n$n_records[match(dec$locality_key, n$locality_key)]

  attrs <- data.frame(
    decisionID = dec$decision_id,
    decisionType = dec$decision_type,
    localityKey = dec$locality_key,
    verbatimLocality = dec$verbatim_locality,
    nRecords = as.integer(dec$n_records),
    decimalLatitude = dec$decimal_latitude,
    decimalLongitude = dec$decimal_longitude,
    coordinateUncertaintyInMeters = dec$coordinate_uncertainty_m,
    pointRadiusSpatialFit = dec$point_radius_spatial_fit,
    georeferencedBy = dec$georeferenced_by,
    georeferencedDate = dec$georeferenced_date,
    georeferenceProtocol = dec$georeference_protocol,
    georeferenceSources = dec$georeference_sources,
    georeferenceRemarks = dec$georeference_remarks,
    stringsAsFactors = FALSE
  )
  geom <- sf::st_as_sfc(dec$footprint_wkt, crs = 4326)

  layer <- function(type, cast_to) {
    parts <- lapply(seq_along(geom), function(i) {
      gt <- as.character(sf::st_geometry_type(geom[i]))
      # st_collection_extract() only sorts collections; a plain geometry of
      # the other dimension is an error there rather than an empty result.
      g <- if (gt == "GEOMETRYCOLLECTION") {
        suppressWarnings(sf::st_collection_extract(geom[i], type))
      } else if (gt %in% c(type, paste0("MULTI", type))) {
        geom[i]
      } else {
        NULL
      }
      if (is.null(g) || length(g) == 0 || all(sf::st_is_empty(g))) return(NULL)
      sf::st_cast(sf::st_union(g), cast_to)
    })
    keep <- which(!vapply(parts, is.null, logical(1)))
    g <- if (length(keep)) do.call(c, parts[keep]) else sf::st_sfc(crs = 4326)
    sf::st_sf(attrs[keep, , drop = FALSE], geometry = sf::st_set_crs(g, 4326))
  }

  list(
    polygons = layer("POLYGON", "MULTIPOLYGON"),
    lines = layer("LINESTRING", "MULTILINESTRING")
  )
}

#' Write footprints to a GeoPackage
#'
#' @param footprints Result of [dwc_footprints()].
#' @param file Destination file. Overwritten.
#'
#' @return The names of the layers written; empty when there was nothing to
#'   write, in which case no file is created.
#' @noRd
write_footprints_gpkg <- function(footprints, file) {
  layers <- Filter(function(x) nrow(x) > 0, footprints)
  if (length(layers) == 0) return(character())
  if (file.exists(file)) unlink(file)
  for (nm in names(layers)) {
    sf::st_write(layers[[nm]], file, layer = nm, driver = "GPKG",
                 append = TRUE, quiet = TRUE)
  }
  names(layers)
}

#' Base name used for a project's exported files
#'
#' @param project Path to the project file.
#' @param what What the file holds, e.g. `"dwc"`.
#' @param ext File extension, without the dot.
#' @param date Date stamped into the name.
#'
#' @return A file name such as `"odzala_dwc_20260916.csv"`.
#' @noRd
export_file_name <- function(project, what, ext, date = Sys.Date()) {
  stem <- if (is.null(project) || is.na(project)) "georef" else
    tools::file_path_sans_ext(basename(project))
  sprintf("%s_%s_%s.%s", stem, what, format(date, "%Y%m%d"), ext)
}

#' Export a project's results
#'
#' Writes everything a finished project produces into one directory:
#'
#' * `<project>_dwc_<date>.csv` -- the Darwin Core table, one row per record.
#'   This is what goes into a database or a publication.
#' * `<project>_log_<date>.csv` -- every decision ever made, superseded ones
#'   included. The audit trail; it should travel with the table.
#' * `<project>_footprints_<date>.gpkg` -- the footprints as GIS layers, see
#'   [dwc_footprints()]. Omitted when nothing has a footprint yet.
#'
#' Exporting is safe at any stage: every decision is already saved in the
#' project file as it is made, and these files are regenerated from it.
#'
#' @param project Path to a project file.
#' @param dir Directory to write into. Defaults to the project's own folder.
#'
#' @return The paths written, invisibly, named `dwc`, `log` and `footprints`.
#'
#' @examples
#' \dontrun{
#' export_project("georef_20260916.sqlite")
#' }
#'
#' @export
export_project <- function(project, dir = dirname(project)) {
  if (!is_project_file(project)) {
    stop("Not a georefapp project: ", project, call. = FALSE)
  }
  con <- store_open(project)
  on.exit(store_close(con), add = TRUE)
  export_project_con(con, project, dir)
}

#' @rdname export_project
#' @param con Open connection to the project.
#' @noRd
export_project_con <- function(con, project, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- c(
    dwc = file.path(dir, export_file_name(project, "dwc", "csv")),
    log = file.path(dir, export_file_name(project, "log", "csv")),
    footprints = file.path(dir, export_file_name(project, "footprints", "gpkg"))
  )
  readr::write_csv(dwc_table(con), paths[["dwc"]], na = "")
  readr::write_csv(store_decisions(con), paths[["log"]], na = "")
  if (length(write_footprints_gpkg(dwc_footprints(con), paths[["footprints"]])) == 0) {
    paths <- paths[names(paths) != "footprints"]
  }
  invisible(stats::setNames(normalizePath(paths, winslash = "/"), names(paths)))
}

#' Test whether a file is a georefapp project
#'
#' A project is a SQLite file holding the `records` and `decisions` tables.
#' The test matters because a gazetteer snapshot is a SQLite file too, and
#' often sits in the same folder.
#'
#' @param path Path to test.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_project_file <- function(path) {
  if (length(path) != 1 || is.na(path) || !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  # The SQLite header is checked first: opening an arbitrary file with RSQLite
  # would create an empty database where the path did not point to one.
  magic <- tryCatch(readBin(path, "raw", 16L), error = function(e) raw())
  if (!identical(rawToChar(magic[magic != as.raw(0)]), "SQLite format 3")) return(FALSE)
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  all(c("records", "decisions") %in% DBI::dbListTables(con))
}

#' Projects found in a directory
#'
#' @param dir Directory to search, not recursively.
#'
#' @return A data frame with `path`, `name`, `modified`, `n_records` and
#'   `n_decisions`, most recently modified first.
#' @noRd
find_projects <- function(dir = getwd()) {
  files <- list.files(dir, pattern = "\\.sqlite$", full.names = TRUE, ignore.case = TRUE)
  files <- files[vapply(files, is_project_file, logical(1))]
  out <- data.frame(
    path = normalizePath(files, winslash = "/", mustWork = FALSE),
    name = basename(files),
    modified = file.mtime(files),
    stringsAsFactors = FALSE
  )
  counts <- lapply(out$path, project_counts)
  out$n_records <- vapply(counts, `[[`, integer(1), "n_records")
  out$n_decisions <- vapply(counts, `[[`, integer(1), "n_decisions")
  out[order(out$modified, decreasing = TRUE), , drop = FALSE]
}

#' Record and decision counts of a project, without modifying it
#'
#' @param path Path to a project file.
#'
#' @return A list with integer `n_records` and `n_decisions`.
#' @noRd
project_counts <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  list(
    n_records = as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM records")$n),
    n_decisions = as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM decisions")$n)
  )
}
