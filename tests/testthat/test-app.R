# Exercises the wiring rather than the arithmetic: that a drawn shape reaches
# the store as a decision, that inheritance happens, and that a bad radius is
# refused.

drawn_circle <- function(edit_id, lng, lat, radius) {
  list(
    type = "Feature",
    properties = list(edit_id = edit_id, radius = radius),
    geometry = list(type = "Point", coordinates = c(lng, lat))
  )
}

drawn_marker <- function(edit_id, lng, lat) {
  list(
    type = "Feature",
    properties = list(edit_id = edit_id),
    geometry = list(type = "Point", coordinates = c(lng, lat))
  )
}

#' Open a project holding the given localities, closed when the caller exits
local_con <- function(verbatim, record_id = NULL, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- store_open(path)
  withr::defer(store_close(con), envir = env)
  store_write_records(con, data.frame(
    record_id = record_id %||% sprintf("r%02d", seq_along(verbatim)),
    locality_key = normalise_locality(verbatim),
    verbatim_locality = verbatim,
    stringsAsFactors = FALSE
  ))
  con
}

test_that("the import module builds a project from the bundled example", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  shiny::testServer(import_server, {
    session$setInputs(use_example = 1)
    session$setInputs(
      col_locality = "locality",
      col_id = "catalog_number",
      col_country = "country",
      col_admin1 = "province"
    )
    recs <- records_r()
    expect_equal(nrow(recs), 24L)
    # Twelve real localities plus the sentinel for the blank one.
    expect_equal(length(unique(recs$locality_key)), 13L)

    session$setInputs(project_name = "test_project", create_btn = 1)
    path <- session$returned()
    expect_true(!is.null(path))
    expect_true(file.exists(path))

    con <- store_open(path)
    withr::defer(store_close(con))
    expect_equal(nrow(store_localities(con)), 12L)
    # The record with no locality text is kept, and is not offered as work.
    expect_equal(nrow(dwc_table(con)), 24L)
  })
})

test_that("drawing and saving writes a decision every sharing record inherits", {
  con <- local_con(c("Yangambi", "Env. Yangambi", "Kribi"), c("a", "b", "c"))

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L)
    expect_equal(current_r()$locality_key, "yangambi")
    expect_equal(current_r()$n_records, 2L)

    session$setInputs(
      radius_m = 1000,
      centre_inside = FALSE,
      by = "G. Dauby",
      sources = "OSM",
      remarks = "Village centre.",
      map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 8000)
    )
    # The circle's own radius wins over the default for a bare point.
    expect_equal(metrics_r()$coordinate_uncertainty_m, 8000)

    session$setInputs(save = 1)

    dec <- store_current_decisions(con)
    expect_equal(nrow(dec), 1L)
    expect_equal(dec$locality_key, "yangambi")
    expect_equal(dec$coordinate_uncertainty_m, 8000)
    expect_equal(dec$georeferenced_by, "G. Dauby")
    expect_equal(dec$georeference_remarks, "Village centre.")
    expect_true(is.na(dec$supersedes))

    dwc <- dwc_table(con)
    yang <- dwc[dwc$recordID %in% c("a", "b"), ]
    expect_equal(nrow(yang), 2L)
    expect_true(all(yang$coordinateUncertaintyInMeters == 8000))
    expect_true(is.na(dwc$decimalLatitude[dwc$recordID == "c"]))
  })
})

test_that("saving clears the drawing and moves to the next pending locality", {
  con <- local_con(c("Yangambi", "Yangambi", "Kribi"))

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
    session$setInputs(map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 5000))
    expect_false(is.null(metrics_r()))

    session$setInputs(save = 1)
    # Nothing is left drawn for the next locality to inherit by accident.
    expect_null(metrics_r())
    expect_equal(store_localities(con)$status[1], "done")
  })
})

test_that("a zero radius is refused rather than silently recorded", {
  con <- local_con("Yangambi")

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 0)
    session$setInputs(map_draw_new_feature = drawn_marker(1, 24.4667, 0.8167))
    expect_equal(metrics_r()$coordinate_uncertainty_m, 0)

    session$setInputs(save = 1)
    expect_equal(nrow(store_decisions(con)), 0L)
  })
})

test_that("marking a locality unresolvable is recorded as a decision", {
  con <- local_con("Kribi")

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(
      localities__reactable__selected = 1L,
      remarks = "Two places of this name; label gives no province."
    )
    session$setInputs(unresolvable = 1)

    dec <- store_current_decisions(con)
    expect_equal(nrow(dec), 1L)
    expect_equal(dec$decision_type, "unresolvable")
    expect_true(is.na(dec$decimal_latitude))
    expect_match(dec$georeference_remarks, "Two places")
  })
})

test_that("saving again supersedes the previous decision", {
  con <- local_con("Yangambi")
  first <- store_add_decision(
    con, "yangambi", "drawn",
    georef_metrics(sf::st_sfc(sf::st_point(c(24.0, 0.5)), crs = 4326), point_radius_m = 20000)
  )

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
    session$setInputs(map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 3000))
    session$setInputs(save = 1)

    current <- store_current_decisions(con)
    expect_equal(nrow(current), 1L)
    expect_equal(current$coordinate_uncertainty_m, 3000)
    expect_equal(current$supersedes, first)
    # The earlier interpretation is still on file.
    expect_equal(nrow(store_decisions(con)), 2L)
  })
})

test_that("the export module derives its table from the store", {
  con <- local_con(c("Yangambi", "Yangambi"))
  store_add_decision(
    con, "yangambi", "drawn",
    georef_metrics(sf::st_sfc(sf::st_point(c(24.4667, 0.8167)), crs = 4326),
                   point_radius_m = 5000)
  )

  shiny::testServer(
    export_server,
    args = list(con_r = shiny::reactive(con), refresh_r = shiny::reactive(1)),
    {
      dat <- dwc_r()
      expect_equal(nrow(dat), 2L)
      expect_true(all(dat$coordinateUncertaintyInMeters == 5000))
      expect_true(all(c("footprintWKT", "georeferenceProtocol") %in% names(dat)))
    }
  )
})

test_that("no dictionary means every locality reports no equivalent", {
  cand <- candidates_query("yangambi", "Yangambi")
  expect_s3_class(cand, "data.frame")
  expect_equal(nrow(cand), 0L)
  expect_named(cand, names(candidates_empty()))
})

test_that("a broken dictionary does not stop the georeferencing loop", {
  withr::local_options(list(
    georefapp.candidates = function(locality_key, verbatim = NULL, limit = 10L) {
      stop("dictionary unreachable")
    }
  ))
  expect_warning(cand <- candidates_query("yangambi"), "unreachable")
  expect_equal(nrow(cand), 0L)
})

test_that("the evidence panel lists ungeoreferenced candidates without offering to plot them", {
  cand <- candidates_empty()
  cand <- rbind(cand, tibble::tibble(
    candidate_id = c("1", "2"),
    locality_verbatim = c("Parc National d'Odzala, village Mbandza",
                          "Layon Mbandza"),
    country = NA_character_,
    decimal_latitude = c(0.62, NA), decimal_longitude = c(14.81, NA),
    n_records = c(12L, 1L), n_records_georef = c(12L, 0L),
    n_distinct_coords = c(4L, NA), span_deg = c(0.05, NA),
    n_name_variants = 1L, is_georeferenced = c(TRUE, FALSE),
    coverage = c(1, 1), overlap = c(0.4, 0.2), n_matched = 1L,
    source = "rainbio"))

  html <- as.character(candidate_panel(cand, "ws-focus"))
  expect_match(html, "village Mbandza")
  expect_match(html, "Layon Mbandza")
  expect_match(html, "not georeferenced")
  # Only the one with coordinates is clickable.
  expect_equal(lengths(regmatches(html, gregexpr("Shiny.setInputValue", html)))[[1]], 1L)

  # And only that one reaches the map.
  m <- add_candidates(base_map(), cand)
  drawn <- Filter(function(c) identical(c$method, "addCircleMarkers"), m$x$calls)
  expect_length(drawn, 1L)
  expect_equal(drawn[[1]]$args[[1]], 0.62)
})

test_that("with no provider the panel says so rather than reporting no match", {
  withr::local_options(georefapp.candidates = NULL)
  expect_match(as.character(candidate_panel(candidates_empty(), "x")),
               "No locality dictionary is configured")
})

test_that("stop_on_close is off unless asked for", {
  # app.R deploys georef_server() to shinyapps.io, where one user closing a tab
  # must not take the application down for everyone else. The default therefore
  # has to stay FALSE; launch() is what turns it on.
  expect_false(formals(georef_server)$stop_on_close)
  expect_true(formals(launch)$stop_on_close)
  expect_true(is.function(georef_server(stop_on_close = TRUE)))
})

test_that("each server instance counts its own sessions", {
  # The counter lives in georef_server()'s frame rather than the session
  # function's, so it survives a session ending. Two servers must not share it.
  a <- georef_server(stop_on_close = TRUE)
  b <- georef_server(stop_on_close = TRUE)
  expect_false(identical(environment(a), environment(b)))
  expect_equal(get("open_sessions", envir = environment(a)), 0L)
})
