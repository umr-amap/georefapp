example_records <- function() {
  verbatim <- c("Yangambi", "Env. Yangambi", "Yangambi", "Kribi", "Ca. Kribi", "Irangi")
  data.frame(
    record_id = sprintf("r%02d", seq_along(verbatim)),
    locality_key = normalise_locality(verbatim),
    verbatim_locality = verbatim,
    country = c(rep("DRC", 3), rep("Cameroon", 2), "DRC"),
    stringsAsFactors = FALSE
  )
}

local_project <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- store_open(path)
  withr::defer(store_close(con), envir = env)
  store_write_records(con, example_records())
  con
}

test_that("records collapse to distinct localities", {
  con <- local_project()
  loc <- store_localities(con)
  expect_equal(nrow(loc), 3L)
  expect_true(all(loc$status == "pending"))
  # Three spellings of Yangambi ride on one decision.
  expect_equal(loc$n_records[loc$verbatim_locality == "Yangambi"], 3L)
})

test_that("a decision marks its locality done and leaves others alone", {
  con <- local_project()
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  m <- georef_metrics(pt, point_radius_m = 5000)
  store_add_decision(con, "yangambi", "drawn", m, georeferenced_by = "GD")

  loc <- store_localities(con)
  expect_equal(loc$status[loc$locality_key == "yangambi"], "done")
  expect_equal(sum(loc$status == "pending"), 2L)
})

test_that("revising supersedes without losing the earlier decision", {
  con <- local_project()
  pt1 <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  pt2 <- sf::st_sfc(sf::st_point(c(24.6, 0.80)), crs = 4326)

  first <- store_add_decision(con, "yangambi", "drawn",
                              georef_metrics(pt1, point_radius_m = 5000))
  second <- store_add_decision(con, "yangambi", "drawn",
                               georef_metrics(pt2, point_radius_m = 2000),
                               supersedes = first)

  current <- store_current_decisions(con)
  expect_equal(nrow(current), 1L)
  expect_equal(current$decision_id, second)
  expect_equal(current$coordinate_uncertainty_m, 2000)

  # The superseded decision is still in the file: that is the audit trail.
  full <- store_decisions(con)
  expect_equal(nrow(full), 2L)
  expect_true(first %in% full$decision_id)
})

test_that("an unresolvable locality is a decision, not an absence", {
  con <- local_project()
  store_add_decision(con, "kribi", "unresolvable", NULL,
                     georeference_remarks = "Two places of this name.")
  loc <- store_localities(con)
  expect_equal(loc$status[loc$locality_key == "kribi"], "unresolvable")

  dwc <- dwc_table(con)
  kribi <- dwc[dwc$verbatimLocality %in% c("Kribi", "Ca. Kribi"), ]
  expect_equal(nrow(kribi), 2L)
  expect_true(all(is.na(kribi$decimalLatitude)))
  expect_true(all(kribi$georeferenceRemarks == "Two places of this name."))
})

test_that("the Darwin Core table expands one decision to every record", {
  con <- local_project()
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  id <- store_add_decision(con, "yangambi", "drawn",
                           georef_metrics(pt, point_radius_m = 5000),
                           georeferenced_by = "GD", georeference_sources = "OSM")

  dwc <- dwc_table(con)
  expect_equal(nrow(dwc), 6L)

  yang <- dwc[dwc$georefappDecisionID %in% id, ]
  expect_equal(nrow(yang), 3L)
  expect_true(all(yang$coordinateUncertaintyInMeters == 5000))
  expect_true(all(yang$georeferencedBy == "GD"))
  expect_true(all(grepl("point-radius", yang$georeferenceProtocol, ignore.case = TRUE)))
  # Every row can be traced back to the decision that produced it.
  expect_true(all(!is.na(yang$georefappDecisionID)))
})

test_that("the output is regenerable from the file alone", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- store_open(path)
  store_write_records(con, example_records())
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  store_add_decision(con, "yangambi", "drawn", georef_metrics(pt, point_radius_m = 5000))
  first <- dwc_table(con)
  store_close(con)

  reopened <- store_open(path)
  on.exit(store_close(reopened))
  expect_equal(dwc_table(reopened), first)
})

test_that("re-importing keeps decisions for localities that survive", {
  con <- local_project()
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  store_add_decision(con, "yangambi", "drawn", georef_metrics(pt, point_radius_m = 5000))

  extra <- rbind(example_records(), data.frame(
    record_id = "r99", locality_key = normalise_locality("Ipassa"),
    verbatim_locality = "Ipassa", country = "Gabon", stringsAsFactors = FALSE
  ))
  store_write_records(con, extra)

  loc <- store_localities(con)
  expect_equal(loc$status[loc$locality_key == "yangambi"], "done")
  expect_equal(loc$status[loc$locality_key == "ipassa"], "pending")
})

test_that("an area decision says the footprint is the locality itself", {
  con <- local_project()
  park <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(14.6, 15.0, 15.0, 14.6, 14.6), c(0.4, 0.4, 0.8, 0.8, 0.4)
  ))), crs = 4326)
  m <- georef_metrics(park)
  id <- store_add_decision(con, "kribi", "area", m,
                           footprint_origin = "imported from parks.gpkg, NAME = 'Odzala'")

  dec <- store_current_decisions(con)
  expect_equal(dec$decision_type, "area")
  expect_true(startsWith(
    dec$georeference_protocol,
    "Footprint is the extent of the locality itself, not an uncertainty envelope (imported from parks.gpkg, NAME = 'Odzala')"
  ))

  # The shape is the decision, so it must come back out exactly as it went in.
  fp <- store_decision_footprint(con, id)
  expect_equal(sf::st_as_text(fp), m$footprint_wkt)
  dwc <- dwc_table(con)
  kribi <- dwc[dwc$georefappDecisionType %in% "area", ]
  expect_equal(nrow(kribi), 2L)
  expect_true(all(kribi$footprintWKT == m$footprint_wkt))
  loc <- store_localities(con)
  expect_equal(loc$status[loc$locality_key == "kribi"], "done")
})

test_that("an area needs a footprint with an area", {
  con <- local_project()
  ln <- sf::st_sfc(sf::st_linestring(cbind(c(24, 24.3), c(0.5, 0.6))), crs = 4326)
  expect_error(store_add_decision(con, "yangambi", "area", georef_metrics(ln)),
               "footprint with an area")
  expect_error(store_add_decision(con, "yangambi", "area", NULL),
               "footprint with an area")
  expect_equal(nrow(store_decisions(con)), 0L)
})

test_that("the drawn protocol is unchanged, and an imported envelope names its file", {
  con <- local_project()
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  m <- georef_metrics(pt, point_radius_m = 5000)
  store_add_decision(con, "yangambi", "drawn", m)
  store_add_decision(con, "irangi", "drawn", m, footprint_origin = "imported from x.kml")
  dec <- store_current_decisions(con)
  expect_match(dec$georeference_protocol[dec$locality_key == "yangambi"],
               "^Point-radius from drawn footprint; minimum bounding circle")
  expect_match(dec$georeference_protocol[dec$locality_key == "irangi"],
               "^Point-radius from footprint imported from x.kml; minimum bounding circle")
})

test_that("a decision without a footprint has none to show", {
  con <- local_project()
  id <- store_add_decision(con, "irangi", "unresolvable")
  expect_null(store_decision_footprint(con, id))
  expect_null(store_decision_footprint(con, NA_character_))
})
