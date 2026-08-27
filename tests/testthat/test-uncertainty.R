pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)

test_that("a buffered point reports exactly its radius", {
  m <- georef_metrics(pt, point_radius_m = 5000)
  expect_equal(m$coordinate_uncertainty_m, 5000)
  expect_equal(m$decimal_latitude, 0.77, tolerance = 1e-6)
  expect_equal(m$decimal_longitude, 24.5, tolerance = 1e-6)
  # A circle reported as a circle is an exact match.
  expect_equal(m$point_radius_spatial_fit, 1, tolerance = 1e-3)
})

test_that("a drawn circle's own radius overrides the default", {
  g <- pt
  attr(g, "radius") <- 2500
  m <- georef_metrics(g, point_radius_m = 5000)
  expect_equal(m$coordinate_uncertainty_m, 2500)
})

test_that("a rectangle's spatial fit is the circumscribed-circle ratio", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(cbind(c(24, 24.2, 24.2, 24, 24), c(0.5, 0.5, 0.7, 0.7, 0.5)))),
    crs = 4326
  )
  m <- georef_metrics(poly)
  # For a square, circle area / square area is pi/2.
  expect_equal(m$point_radius_spatial_fit, pi / 2, tolerance = 1e-3)
})

test_that("a line has no defined spatial fit", {
  ln <- sf::st_sfc(sf::st_linestring(cbind(c(24, 24.3), c(0.5, 0.6))), crs = 4326)
  m <- georef_metrics(ln)
  expect_true(is.na(m$point_radius_spatial_fit))
  expect_gt(m$coordinate_uncertainty_m, 0)
})

test_that("forcing the centre onto a concave footprint keeps it inside", {
  # An L-shape: its bounding-circle centre falls outside the polygon.
  ring <- cbind(
    c(24.0, 24.4, 24.4, 24.1, 24.1, 24.0, 24.0),
    c(0.50, 0.50, 0.55, 0.55, 0.90, 0.90, 0.50)
  )
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)

  mbc <- georef_metrics(poly, centre = "mbc")
  inside <- georef_metrics(poly, centre = "inside")

  centre_pt <- function(m) {
    sf::st_sfc(sf::st_point(c(m$decimal_longitude, m$decimal_latitude)), crs = 4326)
  }
  expect_false(as.logical(sf::st_within(centre_pt(mbc), poly, sparse = FALSE)[1, 1]))
  expect_true(as.logical(sf::st_within(centre_pt(inside), poly, sparse = FALSE)[1, 1]))

  # Moving the centre off the optimum can only widen the circle.
  expect_gte(inside$coordinate_uncertainty_m, mbc$coordinate_uncertainty_m)
})

test_that("the reported circle always covers the footprint", {
  # Checked against ellipsoidal distances. s2, which sf uses by default, works
  # on a sphere and reads about 0.09% long at this latitude, which is larger
  # than the tolerance a coverage check deserves.
  withr::local_options(list(sf_use_s2 = NULL))
  old <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  withr::defer(suppressMessages(sf::sf_use_s2(old)))

  poly <- sf::st_sfc(
    sf::st_polygon(list(cbind(c(24, 24.5, 24.3, 24, 24), c(0.5, 0.6, 0.9, 0.8, 0.5)))),
    crs = 4326
  )
  for (rule in c("mbc", "inside")) {
    m <- georef_metrics(poly, centre = rule)
    centre <- sf::st_sfc(
      sf::st_point(c(m$decimal_longitude, m$decimal_latitude)), crs = 4326
    )
    far <- as.numeric(max(sf::st_distance(sf::st_cast(poly, "POINT"), centre)))
    expect_gte(m$coordinate_uncertainty_m, far - 1)
    # Covering is necessary but not sufficient: a wildly inflated radius would
    # also cover, and would be a worse georeference.
    expect_lt(m$coordinate_uncertainty_m, far * 1.001 + 1)
  }
})

test_that("several drawn shapes are enclosed by one circle", {
  two <- c(
    sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326),
    sf::st_sfc(sf::st_point(c(24.6, 0.80)), crs = 4326)
  )
  m <- georef_metrics(two, point_radius_m = 1000)
  # The two buffers are ~12 km apart, so the enclosing radius must exceed the
  # 1 km applied to each on its own.
  expect_gt(m$coordinate_uncertainty_m, 5000)
})

test_that("empty input yields nothing", {
  expect_null(georef_metrics(NULL))
})
