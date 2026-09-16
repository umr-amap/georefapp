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
    # Closed explicitly: a withr::defer() here is scoped to the testServer
    # expression's environment, which is never torn down, so it never ran.
    n_localities <- nrow(store_localities(con))
    n_rows <- nrow(dwc_table(con))
    store_close(con)
    expect_equal(n_localities, 12L)
    # The record with no locality text is kept, and is not offered as work.
    expect_equal(n_rows, 24L)
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

test_that("the metrics say when the marker radius is in play, and only then", {
  con <- local_con("Yangambi")

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 2500)
    session$setInputs(map_draw_new_feature = drawn_marker(1, 24.4667, 0.8167))
    expect_equal(metrics_r()$point_radius_used, 2500)
    expect_equal(metrics_r()$coordinate_uncertainty_m, 2500)
    expect_match(as.character(output$metrics$html), "Includes the 2 500 m radius", fixed = TRUE)

    # Replace the marker with a circle: its own radius wins and the field is
    # no longer involved.
    session$setInputs(map_draw_deleted_features = drawn_marker(1, 24.4667, 0.8167))
    session$setInputs(map_draw_new_feature = drawn_circle(2, 24.4667, 0.8167, 800))
    expect_true(is.na(metrics_r()$point_radius_used))
    expect_equal(metrics_r()$coordinate_uncertainty_m, 800)
    expect_false(grepl("Includes the", as.character(output$metrics$html), fixed = TRUE))
  })
})

test_that("the radius field explains itself in the page", {
  html <- as.character(htmltools::renderTags(workbench_ui("wb"))$html)
  expect_match(html, "Only for markers", fixed = TRUE)
  expect_match(html, "coordinateUncertaintyInMeters", fixed = TRUE)
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

area_upload <- function(env = parent.frame()) {
  sq <- function(x0, y0) {
    sf::st_polygon(list(cbind(c(x0, x0 + 0.4, x0 + 0.4, x0, x0), c(y0, y0, y0 + 0.4, y0 + 0.4, y0))))
  }
  src <- file.path(withr::local_tempdir(.local_envir = env), "parks.geojson")
  sf::st_write(sf::st_sf(NAME = c("Odzala", "Lope"),
                         geometry = sf::st_sfc(sq(14.6, 0.4), sq(11.4, -0.4), crs = 4326)),
               src, quiet = TRUE)
  # fileInput stores the upload under a generated name.
  stored <- file.path(withr::local_tempdir(.local_envir = env), "0.geojson")
  file.copy(src, stored)
  data.frame(name = "parks.geojson", size = file.size(stored), type = "",
             datapath = stored, stringsAsFactors = FALSE)
}

test_that("an imported feature is saved as an area that names its source", {
  con <- local_con(c("PN Odzala", "Parc National d'Odzala", "Kribi"), c("a", "b", "c"))

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
    session$setInputs(area_file = area_upload())
    expect_equal(nrow(area_data_r()), 2L)

    session$setInputs(area_label = "NAME", area_features = "1", area_tolerance = 0,
                      is_area = TRUE)
    m <- metrics_r()
    expect_gt(m$footprint_area_m2, 1e9)

    session$setInputs(save = 1)
    dec <- store_current_decisions(con)
    expect_equal(dec$decision_type, "area")
    expect_match(dec$georeference_protocol,
                 "imported from parks.geojson, NAME = 'Odzala'", fixed = TRUE)
    expect_equal(dec$footprint_wkt, m$footprint_wkt)
    # Saving clears the pick, so the next locality does not inherit it.
    expect_null(imported_r())
  })
})

test_that("an imported area takes precedence over shapes drawn alongside it", {
  con <- local_con("PN Odzala")

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
    session$setInputs(map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 5000))
    drawn <- metrics_r()$decimal_longitude
    session$setInputs(area_file = area_upload(), area_label = "NAME",
                      area_features = "1", area_tolerance = 0)
    expect_false(isTRUE(all.equal(metrics_r()$decimal_longitude, drawn)))
    expect_equal(metrics_r()$decimal_longitude, 14.8, tolerance = 1e-3)
  })
})

test_that("a line cannot be saved as an area", {
  con <- local_con("Route de Ouesso")

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000, is_area = TRUE)
    session$setInputs(map_draw_new_feature = list(
      type = "Feature", properties = list(edit_id = 1),
      geometry = list(type = "LineString", coordinates = list(c(16, 1.6), c(16.3, 1.9)))
    ))
    expect_false(is.null(metrics_r()))
    session$setInputs(save = 1)
    expect_equal(nrow(store_decisions(con)), 0L)
  })
})

test_that("a project in the working directory can be reopened", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  con <- store_open(file.path(dir, "earlier.sqlite"))
  store_write_records(con, data.frame(record_id = "a", locality_key = "kribi",
                                      verbatim_locality = "Kribi"))
  store_close(con)

  shiny::testServer(import_server, {
    found <- projects_r()
    expect_equal(found$name, "earlier.sqlite")
    session$setInputs(open_existing = found$path[1], open_path = "", open_btn = 1)
    expect_equal(session$returned(), found$path[1])
  })
})

test_that("a typed path that is not a project is refused", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  shiny::testServer(import_server, {
    session$setInputs(open_path = file.path(dir, "absent.sqlite"), open_btn = 1)
    expect_null(session$returned())
    expect_false(file.exists(file.path(dir, "absent.sqlite")))
  })
})

test_that("creating over an existing project asks before touching it", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  path <- file.path(dir, "same.sqlite")
  con <- store_open(path)
  store_write_records(con, data.frame(record_id = "old", locality_key = "kribi",
                                      verbatim_locality = "Kribi"))
  store_close(con)

  shiny::testServer(import_server, {
    session$setInputs(use_example = 1)
    session$setInputs(col_locality = "locality", col_id = "catalog_number",
                      col_country = "", col_admin1 = "")
    session$setInputs(project_name = "same", create_btn = 1)
    # Nothing written, nothing opened: the user is asked first.
    expect_null(session$returned())
    expect_equal(project_counts(path)$n_records, 1L)

    session$setInputs(open_instead_btn = 1)
    expect_equal(basename(session$returned()), "same.sqlite")
    expect_equal(project_counts(path)$n_records, 1L)

    session$setInputs(reimport_btn = 1)
    expect_equal(project_counts(path)$n_records, 24L)
  })
})

test_that("deciding the last pending locality announces the end and leads to export", {
  con <- local_con(c("Yangambi", "Kribi"))
  went <- 0L
  count_visit <- function() went <<- went + 1L

  shiny::testServer(
    workbench_server,
    args = list(con_r = shiny::reactive(con), on_export = count_visit),
    {
      session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
      session$setInputs(map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 5000))
      session$setInputs(save = 1)
      expect_false(finished_rv())

      session$setInputs(localities__reactable__selected = 2L)
      session$setInputs(map_draw_new_feature = drawn_circle(2, 9.9, 2.9, 5000))
      session$setInputs(save = 2)
      expect_true(finished_rv())

      session$setInputs(go_export_modal = 1)
      expect_equal(went, 1L)
      session$setInputs(go_export_header = 1)
      expect_equal(went, 2L)
    }
  )
})

test_that("revising a finished project does not announce the end again", {
  con <- local_con("Yangambi")
  pt <- sf::st_sfc(sf::st_point(c(24.5, 0.77)), crs = 4326)
  store_add_decision(con, "yangambi", "drawn", georef_metrics(pt, point_radius_m = 1000))

  shiny::testServer(workbench_server, args = list(con_r = shiny::reactive(con)), {
    session$setInputs(localities__reactable__selected = 1L, radius_m = 1000)
    session$setInputs(map_draw_new_feature = drawn_circle(1, 24.4667, 0.8167, 5000))
    session$setInputs(save = 1)
    expect_equal(nrow(store_decisions(con)), 2L)
    expect_false(finished_rv())
  })
})

test_that("saving locally writes the exports beside the project file", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "field2026.sqlite")
  con <- store_open(path)
  withr::defer(store_close(con))
  store_write_records(con, data.frame(record_id = c("a", "b"),
                                      locality_key = c("kribi", "irangi"),
                                      verbatim_locality = c("Kribi", "Irangi")))
  pt <- sf::st_sfc(sf::st_point(c(9.9, 2.9)), crs = 4326)
  store_add_decision(con, "kribi", "drawn", georef_metrics(pt, point_radius_m = 1000))

  shiny::testServer(
    export_server,
    args = list(con_r = shiny::reactive(con), refresh_r = shiny::reactive(0),
                project_r = shiny::reactive(path), local_files = TRUE),
    {
      session$setInputs(save_local_btn = 1)
      written <- saved_rv()
      expect_equal(length(written), 3L)
      expect_true(all(file.exists(written)))
      expect_true(all(startsWith(basename(written), "field2026_")))
      expect_equal(nrow(footprints_r()$polygons), 1L)
    }
  )
})

test_that("identifiers that cannot identify records are reported, with what is wrong", {
  expect_null(record_id_issue(c("a", "b", "c"), "id"))

  issue <- record_id_issue(c("a", "b", "b", NA, "", "c", "c", "c"), "barcode")
  expect_equal(issue$column, "barcode")
  expect_equal(issue$n_missing, 2L)
  # Every row carrying a repeated value counts, not only the second copy.
  expect_equal(issue$n_duplicated, 5L)
  expect_equal(issue$examples, c("b", "c"))

  dat <- data.frame(loc = c("Kribi", "Yangambi", "Irangi"),
                    barcode = c("P001", "P001", "P002"))
  recs <- build_records(dat, col_locality = "loc", col_id = "barcode")
  expect_equal(recs$record_id, c("r000001", "r000002", "r000003"))
  expect_equal(attr(recs, "id_issue")$n_duplicated, 2L)

  # Generating identifiers on purpose is not a problem to report.
  expect_null(attr(build_records(dat, col_locality = "loc"), "id_issue"))
  expect_null(attr(build_records(dat[-2, ], col_locality = "loc", col_id = "barcode"), "id_issue"))

  text <- as.character(id_issue_alert(issue))
  expect_match(text, "barcode", fixed = TRUE)
  expect_match(text, "2 rows have no value", fixed = TRUE)
  expect_match(text, "5 rows share a value", fixed = TRUE)
  expect_match(text, "joined back to this file by row order", fixed = TRUE)
})

test_that("the import page warns about unusable identifiers and blank localities", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  shiny::testServer(import_server, {
    session$setInputs(use_example = 1)
    session$setInputs(col_locality = "locality", col_id = "catalog_number",
                      col_country = "", col_admin1 = "")
    summary_text <- function() {
      gsub("[[:space:]]+", " ", as.character(output$summary$html))
    }
    html <- summary_text()
    expect_false(grepl("cannot identify your records", html, fixed = TRUE))
    # The example has one record with no locality text; this warning used to
    # count NA keys, which the sentinel had already replaced, and never showed.
    expect_match(html, "1 record has no usable locality text", fixed = TRUE)
    expect_match(html, "<b>12</b> distinct localities", fixed = TRUE)

    # Country repeats across rows, so as an identifier it collides.
    session$setInputs(col_id = "country")
    html <- summary_text()
    expect_match(html, "cannot identify your records", fixed = TRUE)
    expect_match(html, "country", fixed = TRUE)
  })
})
