#' Workbench module: the georeferencing loop
#'
#' Three panes. The list of distinct localities on the left, the map in the
#' middle, and the evidence and provenance for the locality in hand on the
#' right. One locality is decided at a time, and every record sharing its key
#' inherits the result.
#'
#' @param id Module id.
#' @param con_r A [shiny::reactive()] returning the open project connection, or
#'   `NULL` when no project is open.
#'
#' @return
#'  * UI: HTML tags for the workbench page.
#'  * Server: a [shiny::reactiveVal()] incremented whenever a decision is
#'    written, so other pages can refresh.
#'
#' @name module-workbench
#'
#' @export
workbench_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("no_project")),
    shiny::conditionalPanel(
      condition = "output.has_project === true",
      ns = ns,
      bslib::layout_columns(
        col_widths = c(3, 6, 3),
        bslib::card(
          bslib::card_header(shiny::uiOutput(ns("progress"))),
          bslib::card_body(
            padding = 0,
            reactable::reactableOutput(ns("localities"), height = "620px")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::uiOutput(ns("current_title"))),
          bslib::card_body(
            padding = 0,
            leaflet::leafletOutput(ns("map"), height = "560px")
          ),
          bslib::card_footer(shiny::uiOutput(ns("metrics")))
        ),
        bslib::card(
          bslib::card_body(
            shiny::checkboxInput(
              ns("show_candidates"), "Show similar localities on the map",
              value = TRUE
            ),
            shiny::uiOutput(ns("candidates")),
            shiny::tags$hr(),
            shiny::tags$h6("Interpretation"),
            shiny::numericInput(
              ns("radius_m"), "Radius for a bare point (m)",
              value = 1000, min = 0, step = 500, width = "100%"
            ),
            shiny::checkboxInput(
              ns("centre_inside"),
              "Force the coordinate onto the footprint",
              value = FALSE
            ),
            shiny::tags$hr(),
            shiny::tags$h6("Provenance"),
            shiny::textInput(ns("by"), "Georeferenced by", width = "100%"),
            shiny::textInput(ns("sources"), "Sources consulted", width = "100%"),
            shiny::textAreaInput(
              ns("remarks"), "Remarks", width = "100%", height = "80px",
              placeholder = "Why this interpretation?"
            ),
            shiny::actionButton(
              ns("save"), "Save georeference",
              class = "btn-primary w-100 mb-2"
            ),
            shiny::actionButton(
              ns("unresolvable"), "Cannot be georeferenced",
              class = "btn-outline-secondary w-100"
            )
          )
        )
      )
    )
  )
}

#' @rdname module-workbench
#'
#' @export
workbench_server <- function(id, con_r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    features_rv <- shiny::reactiveValues(x = NULL)
    # Bumped on every write, so the locality list, the map and the export page
    # all refresh from the store rather than from cached copies.
    refresh_rv <- shiny::reactiveVal(0)
    # The map view is carried from one locality to the next: in Central Africa
    # consecutive localities are usually neighbours, and resetting the view
    # every time would cost more than the drawing does.
    view_rv <- shiny::reactiveValues(lng = 18, lat = 0, zoom = 4)

    output$has_project <- shiny::reactive(!is.null(con_r()))
    shiny::outputOptions(output, "has_project", suspendWhenHidden = FALSE)

    output$no_project <- shiny::renderUI({
      if (!is.null(con_r())) return(NULL)
      shinyWidgets::alert(
        status = "info",
        shiny::tags$b("No project open."),
        "Import a locality table on the Import page to begin."
      )
    })

    localities_r <- shiny::reactive({
      refresh_rv()
      con <- con_r()
      shiny::req(con)
      store_localities(con)
    })

    output$progress <- shiny::renderUI({
      con <- con_r()
      shiny::req(con)
      refresh_rv()
      p <- project_progress(con)
      shiny::tagList(
        shiny::tags$span(sprintf("%d of %d localities", p$done + p$unresolvable, p$localities)),
        shiny::tags$span(
          class = "text-muted small ms-2",
          sprintf("%s of %s records", format(p$records_done, big.mark = " "),
                  format(p$records, big.mark = " "))
        )
      )
    })

    output$localities <- reactable::renderReactable({
      loc <- localities_r()
      reactable::reactable(
        loc[, c("verbatim_locality", "n_records", "status")],
        selection = "single", onClick = "select",
        defaultSelected = if (nrow(loc) > 0) 1L else NULL,
        highlight = TRUE, compact = TRUE, searchable = TRUE,
        defaultPageSize = 100, pagination = FALSE, height = 600,
        columns = list(
          verbatim_locality = reactable::colDef(name = "Locality"),
          n_records = reactable::colDef(name = "n", maxWidth = 55),
          status = reactable::colDef(
            name = "", maxWidth = 40, align = "center",
            cell = function(value) status_dot(value)
          )
        ),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#e7f1ff", boxShadow = "inset 2px 0 0 0 #0d6efd")
        )
      )
    })

    selected_r <- shiny::reactive({
      reactable::getReactableState("localities", "selected")
    })

    current_r <- shiny::reactive({
      loc <- localities_r()
      sel <- selected_r()
      if (is.null(sel) || length(sel) == 0 || nrow(loc) == 0) return(NULL)
      if (sel[1] > nrow(loc)) return(NULL)
      loc[sel[1], , drop = FALSE]
    })

    output$current_title <- shiny::renderUI({
      cur <- current_r()
      if (is.null(cur)) return(shiny::tags$span("Select a locality"))
      shiny::tagList(
        shiny::tags$b(cur$verbatim_locality),
        shiny::tags$span(
          class = "text-muted small ms-2",
          sprintf("%d record%s", cur$n_records, if (cur$n_records > 1) "s" else "")
        ),
        if (!is.na(cur$decision_id)) {
          shiny::tags$span(
            class = "badge bg-success ms-2",
            "already georeferenced — saving will supersede it"
          )
        }
      )
    })

    # Moving to another locality clears whatever was drawn for the previous one.
    shiny::observeEvent(current_r()$locality_key, {
      features_rv$x <- NULL
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$map_center, {
      view_rv$lng <- input$map_center$lng
      view_rv$lat <- input$map_center$lat
    })
    shiny::observeEvent(input$map_zoom, {
      view_rv$zoom <- input$map_zoom
    })

    # Rebuilding the map is how drawn shapes get cleared: leafpm adds its layers
    # outside leaflet's group system, so a proxy cannot reliably remove them.
    # The view is restored explicitly, which keeps the rebuild invisible.
    output$map <- leaflet::renderLeaflet({
      cur <- current_r()
      refresh_rv()
      shiny::isolate({
        map <- base_map() |>
          leaflet::setView(lng = view_rv$lng, lat = view_rv$lat, zoom = view_rv$zoom) |>
          leafpm::addPmToolbar(
            toolbarOptions = leafpm::pmToolbarOptions(
              drawMarker = TRUE, drawPolyline = TRUE, drawPolygon = TRUE,
              drawCircle = TRUE, drawRectangle = TRUE,
              cutPolygon = FALSE, editMode = TRUE, removalMode = TRUE,
              position = "topright"
            ),
            drawOptions = leafpm::pmDrawOptions(snappable = FALSE, allowSelfIntersection = FALSE)
          )
        if (!is.null(cur) && !is.na(cur$decision_id) && !is.na(cur$decimal_latitude)) {
          map <- add_existing_decision(map, cur)
        }
        # Isolated: the checkbox is served by the proxy observer, so reading it
        # reactively here would rebuild the map on every toggle.
        if (isTRUE(input$show_candidates)) map <- add_candidates(map, candidates_r())
        map
      })
    })

    shiny::observeEvent(input$map_draw_new_feature, {
      f <- input$map_draw_new_feature
      features_rv$x[[paste0("f", f$properties$edit_id)]] <- f
    })
    shiny::observeEvent(input$map_draw_edited_features, {
      f <- input$map_draw_edited_features
      features_rv$x[[paste0("f", f$properties$edit_id)]] <- f
    })
    shiny::observeEvent(input$map_draw_deleted_features, {
      f <- input$map_draw_deleted_features
      features_rv$x[[paste0("f", f$properties$edit_id)]] <- NULL
    })

    metrics_r <- shiny::reactive({
      feats <- features_rv$x
      if (is.null(feats) || length(feats) == 0) return(NULL)
      g <- draw_features_to_sfc(unname(feats))
      if (is.null(g)) return(NULL)
      radius <- if (isTRUE(input$radius_m >= 0)) input$radius_m else 0
      tryCatch(
        georef_metrics(
          g,
          point_radius_m = radius,
          centre = if (isTRUE(input$centre_inside)) "inside" else "mbc"
        ),
        error = function(e) NULL
      )
    })

    output$metrics <- shiny::renderUI({
      m <- metrics_r()
      if (is.null(m)) {
        return(shiny::tags$span(
          class = "text-muted small",
          "Draw a point, circle, line or polygon on the map."
        ))
      }
      shiny::tagList(
        shiny::tags$div(
          class = "d-flex flex-wrap gap-3 small",
          metric_item("Latitude", sprintf("%.5f", m$decimal_latitude)),
          metric_item("Longitude", sprintf("%.5f", m$decimal_longitude)),
          metric_item("Uncertainty", sprintf("%s m", format(m$coordinate_uncertainty_m, big.mark = " "))),
          metric_item(
            "Spatial fit",
            if (is.na(m$point_radius_spatial_fit)) "undefined" else sprintf("%.3f", m$point_radius_spatial_fit)
          ),
          metric_item("Centre rule", m$centre_rule)
        ),
        if (m$coordinate_uncertainty_m == 0) {
          shiny::tags$div(
            class = "text-danger small mt-2",
            "A radius of zero claims the locality is known exactly. Set a radius before saving."
          )
        }
      )
    })

    candidates_r <- shiny::reactive({
      cur <- current_r()
      if (is.null(cur)) return(candidates_empty())
      candidates_query(cur$locality_key, verbatim = cur$verbatim_locality,
                       limit = 12L)
    })

    output$candidates <- shiny::renderUI({
      cand <- candidates_r()
      candidate_panel(cand, ns("focus"))
    })

    # Candidates are added through a proxy rather than by rebuilding the map:
    # a rebuild clears whatever the user has drawn, and toggling a reference
    # layer must never cost them their work. The rebuild path handles them
    # separately, in renderLeaflet above.
    shiny::observeEvent(input$show_candidates, {
      proxy <- leaflet::leafletProxy("map", session)
      leaflet::clearGroup(proxy, "candidates")
      if (isTRUE(input$show_candidates)) add_candidates(proxy, candidates_r())
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$focus, {
      cand <- candidates_r()
      row <- cand[cand$candidate_id == input$focus, , drop = FALSE]
      if (nrow(row) == 0 || is.na(row$decimal_latitude[1])) return()
      leaflet::flyTo(leaflet::leafletProxy("map", session),
                     lng = row$decimal_longitude[1], lat = row$decimal_latitude[1],
                     zoom = 11)
    })

    write_decision <- function(type, metrics) {
      con <- con_r()
      cur <- current_r()
      if (is.null(con) || is.null(cur)) return(invisible(NULL))
      store_add_decision(
        con,
        locality_key = cur$locality_key,
        decision_type = type,
        metrics = metrics,
        verbatim_locality = cur$verbatim_locality,
        georeferenced_by = input$by,
        georeference_sources = input$sources,
        georeference_remarks = input$remarks,
        supersedes = cur$decision_id,
        gazetteer_snapshot = candidates_snapshot()
      )
      features_rv$x <- NULL
      shiny::updateTextAreaInput(session, "remarks", value = "")
      refresh_rv(refresh_rv() + 1)
      advance_to_next_pending()
    }

    advance_to_next_pending <- function() {
      loc <- shiny::isolate(localities_r())
      sel <- shiny::isolate(selected_r())
      if (nrow(loc) == 0) return(invisible(NULL))
      from <- if (is.null(sel) || length(sel) == 0) 0L else sel[1]
      pending <- which(loc$status == "pending")
      nxt <- pending[pending > from]
      target <- if (length(nxt) > 0) nxt[1] else if (length(pending) > 0) pending[1] else NULL
      if (!is.null(target)) reactable::updateReactable("localities", selected = target)
      invisible(NULL)
    }

    shiny::observeEvent(input$save, {
      m <- metrics_r()
      if (is.null(m)) {
        shiny::showNotification("Draw a footprint on the map first.", type = "warning")
        return()
      }
      if (m$coordinate_uncertainty_m == 0) {
        shiny::showNotification(
          "Set a radius: a zero uncertainty claims the locality is known exactly.",
          type = "error"
        )
        return()
      }
      write_decision("drawn", m)
    })

    shiny::observeEvent(input$unresolvable, {
      write_decision("unresolvable", NULL)
    })

    refresh_rv
  })
}

#' Coloured status marker for the locality list
#'
#' @param value Status string.
#'
#' @return An HTML tag.
#' @noRd
status_dot <- function(value) {
  colour <- switch(value, done = "#198754", unresolvable = "#dc3545", "#dee2e6")
  htmltools::tags$span(
    style = sprintf(
      "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;",
      colour
    ),
    title = value
  )
}

#' One labelled figure in the metrics strip
#'
#' @param label,value Text to show.
#'
#' @return An HTML tag.
#' @noRd
metric_item <- function(label, value) {
  htmltools::tags$div(
    htmltools::tags$div(class = "text-muted", style = "font-size:0.75rem;", label),
    htmltools::tags$div(class = "fw-bold", value)
  )
}

#' The evidence panel: localities elsewhere whose name resembles this one
#'
#' Ungeoreferenced candidates are listed alongside georeferenced ones, greyed
#' and unclickable. They cannot be drawn, but knowing that a place is already
#' recorded under a near-identical spelling is itself evidence -- often that the
#' two entries are one place, and that whatever is decided here should be
#' decided for both.
#'
#' @param cand A tibble shaped like [candidates_empty()].
#' @param focus_input Namespaced id of the input a click reports the chosen
#'   candidate to.
#'
#' @return An HTML tag list.
#' @noRd
candidate_panel <- function(cand, focus_input) {
  q <- attr(cand, "query")
  ignored <- if (is.null(q)) character() else c(q$too_common, q$unknown)

  if (nrow(cand) == 0) {
    return(shiny::tagList(
      shiny::tags$h6("Similar localities"),
      shiny::tags$div(
        class = "text-muted small",
        if (is.null(getOption("georefapp.candidates"))) {
          shiny::tagList(
            "No locality dictionary is configured.",
            shiny::tags$br(),
            shiny::tags$span(class = "fst-italic",
                             "See gazetteer_provider().")
          )
        } else if (length(ignored) && is.null(q$token)) {
          paste0("Nothing distinctive to search on: every word in this ",
                 "locality is common across the dictionary.")
        } else {
          "No similar locality found in the dictionary."
        }
      )
    ))
  }

  rows <- lapply(seq_len(nrow(cand)), function(i) {
    r <- cand[i, ]
    geo <- isTRUE(r$is_georeferenced)
    name <- truncate_text(r$locality_verbatim, 78)
    label <- if (geo) {
      shiny::tags$a(
        href = "#", class = "text-decoration-none",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
          focus_input, jsonlite::toJSON(r$candidate_id, auto_unbox = TRUE)
        ),
        name
      )
    } else {
      shiny::tags$span(class = "text-muted", name)
    }
    detail <- paste(c(
      sprintf("%d record%s", r$n_records, if (r$n_records > 1) "s" else ""),
      if (geo && !is.na(r$n_distinct_coords))
        sprintf("%d coord%s", r$n_distinct_coords,
                if (r$n_distinct_coords > 1) "s" else ""),
      if (!geo) "not georeferenced",
      sprintf("%.0f%% match", 100 * r$coverage)
    ), collapse = " · ")

    shiny::tags$div(
      class = "d-flex gap-2 py-1 border-bottom",
      shiny::tags$span(
        style = sprintf(
          "flex:0 0 8px;height:8px;margin-top:6px;border-radius:50%%;background:%s;",
          if (geo) "#fd7e14" else "#dee2e6"
        )
      ),
      shiny::tags$div(
        style = "min-width:0;",
        shiny::tags$div(style = "font-size:0.8rem;line-height:1.2;", label),
        shiny::tags$div(class = "text-muted",
                        style = "font-size:0.7rem;", detail)
      )
    )
  })

  shiny::tagList(
    shiny::tags$h6(
      "Similar localities",
      shiny::tags$span(class = "badge bg-secondary ms-1", nrow(cand))
    ),
    if (length(ignored)) {
      shiny::tags$div(
        class = "text-muted", style = "font-size:0.7rem;",
        "Not searched on: ", paste(ignored, collapse = ", ")
      )
    },
    shiny::tags$div(style = "max-height:230px;overflow-y:auto;", rows)
  )
}

#' Add the georeferenced candidates to a map or a map proxy
#'
#' Plotted as bare markers with no uncertainty circle. The coordinate is the
#' median of the records filed under that name, and drawing a radius around it
#' would suggest a georeferencing decision that nobody has made.
#'
#' @param map A leaflet map or proxy.
#' @param cand A tibble shaped like [candidates_empty()].
#'
#' @return The map.
#' @noRd
add_candidates <- function(map, cand) {
  cand <- cand[which(cand$is_georeferenced & !is.na(cand$decimal_latitude)), ]
  if (nrow(cand) == 0) return(map)
  leaflet::addCircleMarkers(
    map,
    lng = cand$decimal_longitude, lat = cand$decimal_latitude,
    radius = pmin(10, pmax(4, sqrt(cand$n_records) + 3)),
    color = "#fd7e14", weight = 1, fillOpacity = 0.45,
    group = "candidates",
    label = lapply(seq_len(nrow(cand)), function(i) {
      htmltools::HTML(sprintf(
        "<b>%s</b><br>%d records, %s distinct coordinates",
        htmltools::htmlEscape(truncate_text(cand$locality_verbatim[i], 90)),
        cand$n_records[i],
        if (is.na(cand$n_distinct_coords[i])) "?" else cand$n_distinct_coords[i]
      ))
    })
  )
}

#' Shorten a string for display, marking that it was shortened
#'
#' @param x A string.
#' @param n Maximum characters.
#'
#' @return A string.
#' @noRd
truncate_text <- function(x, n = 70L) {
  x <- as.character(x)
  ifelse(is.na(x) | nchar(x) <= n, x, paste0(substr(x, 1L, n - 1L), "…"))
}

#' Draw an already-recorded decision on the map
#'
#' Shows the saved footprint and its uncertainty circle, so that a revision
#' starts from a visible account of what was decided before.
#'
#' @param map A leaflet map.
#' @param cur One row of [store_localities()].
#'
#' @return The map, with the footprint added.
#' @noRd
add_existing_decision <- function(map, cur) {
  map <- leaflet::addCircles(
    map,
    lng = cur$decimal_longitude, lat = cur$decimal_latitude,
    radius = cur$coordinate_uncertainty_m,
    color = "#198754", weight = 1, fillOpacity = 0.08,
    group = "existing"
  )
  leaflet::addCircleMarkers(
    map,
    lng = cur$decimal_longitude, lat = cur$decimal_latitude,
    radius = 4, color = "#198754", fillOpacity = 1,
    label = "Current georeference", group = "existing"
  )
}
