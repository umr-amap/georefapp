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
