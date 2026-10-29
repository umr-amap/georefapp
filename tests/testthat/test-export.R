local_decided_project <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "odzala.sqlite")
  con <- store_open(path)
  withr::defer(store_close(con), envir = env)
  store_write_records(con, data.frame(
    record_id = c("a", "b", "c", "d"),
    locality_key = c("park", "park", "road", "village"),
    verbatim_locality = c("PN Odzala", "PN Odzala", "Route de Ouesso", "Mbandza"),
    stringsAsFactors = FALSE
  ))
  park <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(14.6, 15, 15, 14.6, 14.6), c(0.4, 0.4, 0.8, 0.8, 0.4)
  ))), crs = 4326)
  road <- sf::st_sfc(sf::st_linestring(cbind(c(16, 16.3), c(1.6, 1.9))), crs = 4326)
  store_add_decision(con, "park", "area", georef_metrics(park))
  store_add_decision(con, "road", "drawn", georef_metrics(road))
  store_add_decision(con, "village", "unresolvable")
  list(con = con, path = path, dir = dir)
}

test_that("footprints split into polygon and line layers with their records", {
  p <- local_decided_project()
  fp <- dwc_footprints(p$con)
  expect_equal(nrow(fp$polygons), 1L)
  expect_equal(nrow(fp$lines), 1L)
  expect_equal(fp$polygons$decisionType, "area")
  expect_equal(fp$polygons$nRecords, 2L)
  expect_equal(as.character(sf::st_geometry_type(fp$lines)), "MULTILINESTRING")
  # The unresolvable locality has nothing to draw and is not a feature.
  expect_false("village" %in% c(fp$polygons$localityKey, fp$lines$localityKey))
})

local_small_circle <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- store_open(path)
  withr::defer(store_close(con), envir = env)
  store_write_records(con, data.frame(
    record_id = "a", locality_key = "spot", verbatim_locality = "Spot",
    stringsAsFactors = FALSE
  ))
  pt <- sf::st_sfc(sf::st_point(c(15.123456, -0.54321)), crs = 4326)
  attr(pt, "radius") <- 50
  list(con = con, metrics = georef_metrics(pt))
}

test_that("a small circle survives the round trip through the store and export", {
  # Written at 7 significant digits, a 50 m circle has neighbouring vertices
  # collapsed into duplicates, which s2 refuses.
  p <- local_small_circle()
  expect_true(all(sf::st_is_valid(sf::st_as_sfc(p$metrics$footprint_wkt, crs = 4326))))
  store_add_decision(p$con, "spot", "drawn", p$metrics)
  expect_equal(nrow(dwc_footprints(p$con)$polygons), 1L)
})

test_that("footprints already saved at low precision still export", {
  p <- local_small_circle()
  m <- p$metrics
  m$footprint_wkt <- sf::st_as_text(sf::st_as_sfc(m$footprint_wkt), digits = 7)
  store_add_decision(p$con, "spot", "drawn", m)
  expect_equal(nrow(dwc_footprints(p$con)$polygons), 1L)
})

test_that("an empty project yields empty layers and no GeoPackage", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- store_open(path)
  withr::defer(store_close(con))
  fp <- dwc_footprints(con)
  expect_equal(nrow(fp$polygons) + nrow(fp$lines), 0L)
  gpkg <- withr::local_tempfile(fileext = ".gpkg")
  expect_length(write_footprints_gpkg(fp, gpkg), 0L)
  expect_false(file.exists(gpkg))
})

test_that("exporting a project writes all four files beside it", {
  p <- local_decided_project()
  paths <- export_project(p$path)
  expect_named(paths, c("dwc", "dwc_xlsx", "log", "footprints"))
  expect_true(all(file.exists(paths)))
  expect_true(all(dirname(paths) == normalizePath(p$dir, winslash = "/")))
  expect_match(basename(paths[["dwc"]]), "^odzala_dwc_\\d{8}\\.csv$")

  dwc <- readr::read_csv(paths[["dwc"]], show_col_types = FALSE)
  expect_equal(nrow(dwc), 4L)
  expect_equal(sort(sf::st_layers(paths[["footprints"]])$name), c("lines", "polygons"))
  expect_equal(nrow(readr::read_csv(paths[["log"]], show_col_types = FALSE)), 3L)
})

test_that("only real projects are recognised, and testing one creates nothing", {
  p <- local_decided_project()
  expect_true(is_project_file(p$path))

  missing <- file.path(p$dir, "nope.sqlite")
  expect_false(is_project_file(missing))
  expect_false(file.exists(missing))

  text <- file.path(p$dir, "text.sqlite")
  writeLines("not a database", text)
  expect_false(is_project_file(text))

  # A gazetteer snapshot is SQLite too, and often sits in the same folder.
  gaz <- file.path(p$dir, "gazetteer.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), gaz)
  DBI::dbWriteTable(con, "gaz_localities", data.frame(x = 1))
  DBI::dbDisconnect(con)
  expect_false(is_project_file(gaz))

  found <- find_projects(p$dir)
  expect_equal(found$name, "odzala.sqlite")
  expect_equal(found$n_records, 4L)
  expect_equal(found$n_decisions, 3L)

  expect_error(export_project(gaz), "Not a georefapp project")
})

test_that("exported file names carry the project name", {
  d <- as.Date("2026-09-16")
  expect_equal(export_file_name("C:/work/odzala.sqlite", "dwc", "csv", d),
               "odzala_dwc_20260916.csv")
  expect_equal(export_file_name(NULL, "log", "csv", d), "georef_log_20260916.csv")
})

test_that("every column of the Darwin Core table is explained, in order", {
  # A column added to dwc_table() without an entry here fails, so the Excel
  # export never ships a column nobody explained.
  cols <- dwc_columns()
  expect_equal(cols$column, names(dwc_empty()))
  expect_false(anyNA(cols$definition) || anyNA(cols$in_georefapp))
  expect_true(all(is.na(cols$term_iri[cols$standard == "georefapp"])))
})

test_that("the Excel export holds the table and the column dictionary", {
  p <- local_decided_project()
  file <- withr::local_tempfile(fileext = ".xlsx")
  write_dwc_xlsx(p$con, file)
  expect_equal(readxl::excel_sheets(file), c("dwc", "columns"))
  dat <- readxl::read_excel(file, "dwc")
  expect_equal(names(dat), names(dwc_empty()))
  expect_equal(nrow(dat), 4L)
  expect_equal(readxl::read_excel(file, "columns")$column, names(dwc_empty()))
})

test_that("a footprint too long for an Excel cell is replaced, not cut", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- store_open(path)
  withr::defer(store_close(con))
  store_write_records(con, data.frame(record_id = "a", locality_key = "big",
                                      verbatim_locality = "Big", stringsAsFactors = FALSE))
  # A polygon of a few thousand vertices: well over 32 767 characters of WKT.
  t <- seq(0, 2 * pi, length.out = 3000)
  ring <- cbind(15 + 0.5 * cos(t), 0.5 * sin(t))
  ring[nrow(ring), ] <- ring[1, ]
  m <- georef_metrics(sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326))
  expect_gt(nchar(m$footprint_wkt), 32767)
  store_add_decision(con, "big", "area", m)

  file <- withr::local_tempfile(fileext = ".xlsx")
  write_dwc_xlsx(con, file)
  dat <- readxl::read_excel(file, "dwc")
  expect_match(dat$footprintWKT, "too long for an Excel cell", fixed = TRUE)
  # The CSV route is untouched.
  expect_equal(dwc_table(con)$footprintWKT, m$footprint_wkt)
})
