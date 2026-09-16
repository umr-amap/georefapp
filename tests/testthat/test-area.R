square <- function(x0, y0, d = 0.1) {
  sf::st_polygon(list(cbind(c(x0, x0 + d, x0 + d, x0, x0), c(y0, y0, y0 + d, y0 + d, y0))))
}

parks <- function() {
  sf::st_sf(
    code = c("A", "B", "C"),
    NAME = c("Odzala", "Lope", NA),
    geometry = sf::st_sfc(square(14.8, 0.6), square(11.6, -0.2), square(9.9, 2.9), crs = 4326)
  )
}

# Mimics fileInput(): every file stored under a meaningless name, with the
# original names given alongside.
as_upload <- function(files) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  stored <- file.path(dir, seq_along(files))
  file.copy(files, stored)
  list(datapath = stored, name = basename(files))
}

test_that("a GeoJSON is read in WGS84 with its polygons and a sensible label", {
  f <- withr::local_tempfile(fileext = ".geojson")
  sf::st_write(sf::st_transform(parks(), 32633), f, quiet = TRUE)
  x <- area_read(area_resolve_upload(f))
  expect_equal(nrow(x), 3L)
  expect_equal(sf::st_crs(x)$epsg, 4326L)
  expect_equal(area_label_column(x), "NAME")
})

test_that("a shapefile survives upload both zipped and as loose parts", {
  dir <- withr::local_tempdir()
  shp <- file.path(dir, "parks.shp")
  sf::st_write(parks(), shp, quiet = TRUE)
  parts <- list.files(dir, pattern = "^parks\\.", full.names = TRUE)

  loose <- as_upload(parts)
  expect_equal(nrow(area_read(area_resolve_upload(loose$datapath, loose$name))), 3L)

  zip <- file.path(withr::local_tempdir(), "parks.zip")
  utils::zip(zip, parts, flags = "-jq")
  zipped <- as_upload(zip)
  path <- area_resolve_upload(zipped$datapath, zipped$name)
  expect_equal(basename(path), "parks.shp")
  expect_equal(nrow(area_read(path)), 3L)
})

test_that("uploads with nothing readable, or too much, are refused", {
  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(area_resolve_upload(txt), "No readable spatial file")

  a <- withr::local_tempfile(fileext = ".geojson")
  b <- withr::local_tempfile(fileext = ".geojson")
  sf::st_write(parks(), a, quiet = TRUE)
  sf::st_write(parks(), b, quiet = TRUE)
  expect_error(area_resolve_upload(c(a, b)), "Several spatial files")
})

test_that("a layer with no polygons, or no CRS, is refused", {
  f <- withr::local_tempfile(fileext = ".gpkg")
  pts <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(14, 0)), crs = 4326))
  sf::st_write(pts, f, layer = "points", quiet = TRUE)
  expect_error(area_read(f, "points"), "no polygons")

  g <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_set_crs(parks(), NA), g, quiet = TRUE)
  expect_error(area_read(g), "no coordinate reference system")
})

test_that("chosen features become one valid footprint", {
  x <- parks()
  p <- area_prepare(x[1:2, ])
  expect_length(p, 1L)
  expect_true(sf::st_is_valid(p))
  expect_equal(as.character(sf::st_geometry_type(p)), "MULTIPOLYGON")
  expect_equal(attr(p, "tolerance_m"), 0)
})

test_that("a self-intersecting boundary is repaired rather than rejected", {
  bowtie <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(14, 14.2, 14, 14.2, 14), c(0, 0.2, 0.2, 0, 0)
  ))), crs = 4326)
  expect_false(sf::st_is_valid(bowtie))
  p <- area_prepare(bowtie)
  expect_true(sf::st_is_valid(p))
  expect_gt(as.numeric(sf::st_area(p)), 0)
})

test_that("simplifying drops vertices but stays within the stated tolerance", {
  a <- seq(0, 2 * pi, length.out = 5001)
  ring <- cbind(14.8 + 0.3 * (1 + 0.2 * sin(40 * a)) * cos(a),
                0.6 + 0.3 * (1 + 0.2 * sin(40 * a)) * sin(a))
  ring[5001, ] <- ring[1, ]
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)

  p <- area_prepare(poly, tolerance_m = 100)
  expect_equal(attr(p, "n_vertices_in"), 5001L)
  expect_lt(attr(p, "n_vertices"), 1000L)

  # The original boundary never strays further than the tolerance from the
  # simplified one, measured in metres.
  crs <- aeqd_crs(14.8, 0.6)
  gap <- sf::st_distance(
    sf::st_cast(sf::st_transform(poly, crs), "POINT"),
    sf::st_cast(sf::st_boundary(sf::st_transform(p, crs)), "MULTILINESTRING")
  )
  expect_lte(max(as.numeric(gap)), 100 * 1.001)
})

test_that("the origin names the file, the features and any simplification", {
  p <- area_prepare(parks()[1, ], tolerance_m = 0)
  expect_equal(
    area_origin("parks.gpkg", "wdpa", "NAME", "Odzala", p),
    "imported from parks.gpkg, layer 'wdpa', NAME = 'Odzala'"
  )
  # A layer named after its file says nothing the file name did not.
  expect_equal(area_origin("parks.shp", "parks", NULL, NULL, p),
               "imported from parks.shp")

  s <- area_prepare(parks()[1:2, ], tolerance_m = 50)
  expect_match(area_origin("parks.shp", "parks", "NAME", c("Odzala", "Lope"), s),
               "NAME = 'Odzala' \\+ 'Lope', simplified to 50 m \\(10 to \\d+ vertices\\)")
})
