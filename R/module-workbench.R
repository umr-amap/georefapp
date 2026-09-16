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
#' @param on_export Function called, with no arguments, when the user asks to
#'   go to their results. The workbench does not know the page layout around
#'   it, so moving there is the caller's job.
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
            shiny::checkboxInput(
              ns("is_area"),
              "The shape is the locality itself (an area)",
              value = FALSE
            ),
            bslib::accordion(
              open = FALSE,
              class = "mb-2",
              bslib::accordion_panel(
                "Import an area from a file",
                value = "area_import",
                shiny::fileInput(
                  ns("area_file"), NULL, multiple = TRUE, width = "100%",
                  accept = area_file_extensions,
                  placeholder = "GeoJSON, KML, GPKG, shapefile"
                ),
                shiny::conditionalPanel(
                  condition = "output.area_loaded === true",
                  ns = ns,
                  shiny::selectInput(ns("area_layer"), "Layer", choices = NULL, width = "100%"),
                  shiny::selectInput(ns("area_label"), "Name features by", choices = NULL, width = "100%"),
                  shiny::selectizeInput(
                    ns("area_features"), "Features making up this locality",
                    choices = NULL, multiple = TRUE, width = "100%",
                    options = list(placeholder = "Type to search")
                  ),
                  shiny::numericInput(
                    ns("area_tolerance"), "Simplify boundary to (m, 0 = keep as is)",
                    value = 0, min = 0, step = 10, width = "100%"
                  )
                )
              )
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
workbench_server <- function(id, con_r, on_export = NULL) {
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
      shiny::tags$div(
        class = "d-flex flex-wrap align-items-baseline gap-2",
        shiny::tags$span(sprintf("%d of %d localities", p$done + p$unresolvable, p$localities)),
        shiny::tags$span(
          class = "text-muted small",
          sprintf("%s of %s records", format(p$records_done, big.mark = " "),
                  format(p$records, big.mark = " "))
        ),
        # Always there, not only at the end: a partial export is legitimate,
        # and the way out should not have to be discovered.
        shiny::actionLink(ns("go_export_header"), "Export results →",
                          class = "small ms-auto")
      )
    })

    go_export <- function() {
      shiny::removeModal()
      if (is.function(on_export)) on_export()
    }
    shiny::observeEvent(input$go_export_header, go_export())
    shiny::observeEvent(input$go_export_modal, go_export())

    # Deciding the last pending locality is the moment the user is done, and
    # otherwise nothing marks it: the list simply stops advancing.
    finished_rv <- shiny::reactiveVal(FALSE)
    announce_if_finished <- function(was_pending) {
      if (!was_pending) return(invisible(NULL))
      p <- project_progress(con_r())
      if (p$pending > 0) return(invisible(NULL))
      finished_rv(TRUE)
      shiny::showModal(shiny::modalDialog(
        title = "All localities decided",
        shiny::tags$p(sprintf(
          "%s of %s records now carry a georeference%s.",
          format(p$records_done, big.mark = " "), format(p$records, big.mark = " "),
          if (p$unresolvable > 0) sprintf("; %d localities were marked unresolvable", p$unresolvable) else ""
        )),
        shiny::tags$p(
          "Every decision is already saved in the project file. The Export page",
          "turns it into a Darwin Core table, the decision log and a GIS file of",
          "the footprints."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Stay here"),
          shiny::actionButton(ns("go_export_modal"), "Go to Export", class = "btn-primary")
        )
      ))
    }

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

    # Moving to another locality clears whatever was drawn or picked for the
    # previous one. The area file itself stays loaded: one file of parks or plot
    # networks usually serves many localities in a row.
    shiny::observeEvent(current_r()$locality_key, {
      features_rv$x <- NULL
      clear_area_pick()
      cur <- shiny::isolate(current_r())
      shiny::updateCheckboxInput(
        session, "is_area",
        value = !is.null(cur) && identical(cur$decision_type, "area")
      )
    }, ignoreNULL = FALSE)

    # -- Area import ----------------------------------------------------------

    notify_error <- function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    }

    area_upload_r <- shiny::reactive({
      f <- input$area_file
      shiny::req(f)
      tryCatch({
        path <- area_resolve_upload(f$datapath, f$name)
        layers <- area_layers(path)
        if (length(layers) == 0) stop("The file holds no polygon layer.", call. = FALSE)
        # Name the file as the user knows it: an archive by its own name, with
        # the member that was read.
        name <- if (nrow(f) == 1 && !identical(f$name, basename(path))) {
          paste0(f$name, "/", basename(path))
        } else {
          basename(path)
        }
        list(path = path, name = name, layers = layers)
      }, error = notify_error)
    })

    shiny::observeEvent(area_upload_r(), {
      u <- area_upload_r()
      shiny::updateSelectInput(session, "area_layer", choices = u$layers, selected = u$layers[1])
    })

    area_data_r <- shiny::reactive({
      u <- area_upload_r()
      shiny::req(u)
      layer <- if (isTRUE(input$area_layer %in% u$layers)) input$area_layer else u$layers[1]
      tryCatch(area_read(u$path, layer), error = notify_error)
    })

    output$area_loaded <- shiny::reactive(!is.null(area_upload_r()))
    shiny::outputOptions(output, "area_loaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(area_data_r(), {
      x <- area_data_r()
      cols <- setdiff(names(x), attr(x, "sf_column"))
      shiny::updateSelectInput(
        session, "area_label",
        choices = c("(feature number)" = "", cols),
        selected = area_label_column(x) %||% ""
      )
    })

    # Server-side selectize: a boundary file can hold thousands of features,
    # more than is sensible to send to the browser as a static list.
    shiny::observe({
      x <- area_data_r()
      shiny::req(x)
      col <- input$area_label
      fallback <- paste("Feature", seq_len(nrow(x)))
      labels <- if (isTRUE(col %in% names(x))) as.character(x[[col]]) else fallback
      blank <- is.na(labels) | !nzchar(trimws(labels))
      labels[blank] <- fallback[blank]
      shiny::updateSelectizeInput(
        session, "area_features",
        choices = stats::setNames(as.character(seq_len(nrow(x))), labels),
        server = TRUE
      )
    })

    # The pick is held on the server, not read straight from the input. Clearing
    # a selectize is a round trip through the browser, and until it comes back
    # the previous locality's area would still be live -- one quick second save
    # would record it again under the wrong locality.
    area_pick_rv <- shiny::reactiveVal(character(0))
    shiny::observeEvent(input$area_features, {
      area_pick_rv(input$area_features %||% character(0))
    }, ignoreNULL = FALSE)
    clear_area_pick <- function() {
      area_pick_rv(character(0))
      shiny::updateSelectizeInput(session, "area_features", selected = character(0))
    }

    imported_r <- shiny::reactive({
      ids <- suppressWarnings(as.integer(area_pick_rv()))
      if (length(ids) == 0) return(NULL)
      x <- area_data_r()
      u <- area_upload_r()
      if (is.null(x) || is.null(u)) return(NULL)
      ids <- ids[!is.na(ids) & ids <= nrow(x)]
      if (length(ids) == 0) return(NULL)
      tryCatch({
        p <- area_prepare(x[ids, ], input$area_tolerance)
        col <- input$area_label
        named <- isTRUE(col %in% names(x))
        attr(p, "origin") <- area_origin(
          u$name, attr(x, "layer"),
          label_column = if (named) col,
          labels = if (named) as.character(x[[col]][ids]),
          prepared = p
        )
        p
      }, error = notify_error)
    })

    # Picking features from an area file is itself the statement that the
    # locality is an area, so the box is ticked for the user. It can still be
    # unticked, for a boundary used only as an envelope.
    shiny::observeEvent(input$area_features, {
      shiny::updateCheckboxInput(session, "is_area", value = TRUE)
      imp <- imported_r()
      if (is.null(imp)) return()
      bb <- sf::st_bbox(imp)
      leaflet::flyToBounds(
        leaflet::leafletProxy("map", session),
        bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]
      )
    })

    shiny::observeEvent(imported_r(), {
      proxy <- leaflet::leafletProxy("map", session)
      leaflet::clearGroup(proxy, "imported")
      add_imported_area(proxy, imported_r())
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
          map <- add_existing_decision(
            map, cur, store_decision_footprint(con_r(), cur$decision_id)
          )
        }
        map <- add_imported_area(map, imported_r())
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

    # An imported area and drawn shapes are not combined: the protocol has to
    # name one source for the footprint, and a boundary file with a hand-drawn
    # addition would make that sentence untrue. The imported area wins.
    metrics_r <- shiny::reactive({
      g <- imported_r()
      if (is.null(g)) {
        feats <- features_rv$x
        if (is.null(feats) || length(feats) == 0) return(NULL)
        g <- draw_features_to_sfc(unname(feats))
      }
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
          "Draw a point, circle, line or polygon on the map, or import an area."
        ))
      }
      has_area <- !is.na(m$point_radius_spatial_fit) && m$footprint_area_m2 > 0
      wkt_kb <- nchar(m$footprint_wkt) / 1024
      n_rec <- current_r()$n_records %||% 1L
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
          if (has_area) metric_item("Area", format_area(m$footprint_area_m2)),
          metric_item("Centre rule", m$centre_rule)
        ),
        if (!is.null(imported_r()) && length(features_rv$x) > 0) {
          shiny::tags$div(
            class = "text-muted small mt-2",
            "Using the imported area; shapes drawn on the map are ignored."
          )
        },
        if (isTRUE(input$is_area) && !has_area) {
          shiny::tags$div(
            class = "text-danger small mt-2",
            "An area needs a footprint with an area: a line or a bare point cannot be one."
          )
        },
        # Worth saying because it is invisible until export: footprintWKT is
        # repeated on every record that inherits the decision.
        if (wkt_kb > 50) {
          shiny::tags$div(
            class = "text-warning small mt-2",
            sprintf(
              "The footprint is %s kB of text, written on each of %d record%s. Consider simplifying the boundary.",
              format(round(wkt_kb), big.mark = " "), n_rec, if (n_rec > 1) "s" else ""
            )
          )
        },
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

    write_decision <- function(type, metrics, footprint_origin = NA_character_) {
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
        gazetteer_snapshot = candidates_snapshot(),
        footprint_origin = footprint_origin
      )
      features_rv$x <- NULL
      clear_area_pick()
      shiny::updateTextAreaInput(session, "remarks", value = "")
      refresh_rv(refresh_rv() + 1)
      advance_to_next_pending()
      # Only when this save decided a pending locality: revising a finished
      # project should not announce the end again on every save.
      announce_if_finished(identical(cur$status, "pending"))
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
      is_area <- isTRUE(input$is_area)
      if (is_area && is.na(m$point_radius_spatial_fit)) {
        shiny::showNotification(
          "An area needs a footprint with an area. Draw a polygon or circle, or untick the area box.",
          type = "error"
        )
        return()
      }
      imp <- imported_r()
      write_decision(
        if (is_area) "area" else "drawn", m,
        footprint_origin = if (is.null(imp)) NA_character_ else attr(imp, "origin")
      )
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
#' starts from a visible account of what was decided before. For an area the
#' footprint is the decision, so it is drawn, not only its enclosing circle.
#'
#' @param map A leaflet map.
#' @param cur One row of [store_localities()].
#' @param footprint The decision's footprint, from [store_decision_footprint()],
#'   or `NULL`.
#'
#' @return The map, with the footprint added.
#' @noRd
add_existing_decision <- function(map, cur, footprint = NULL) {
  if (!is.null(footprint)) {
    type <- as.character(sf::st_geometry_type(footprint))
    if (type %in% c("POLYGON", "MULTIPOLYGON")) {
      map <- leaflet::addPolygons(
        map, data = footprint, color = "#198754", weight = 2, dashArray = "4",
        fillOpacity = 0.12, label = "Current footprint", group = "existing"
      )
    } else if (type %in% c("LINESTRING", "MULTILINESTRING")) {
      map <- leaflet::addPolylines(
        map, data = footprint, color = "#198754", weight = 3,
        label = "Current footprint", group = "existing"
      )
    }
  }
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

#' Add an imported area to a map or a map proxy
#'
#' Drawn in blue, apart from the green of a saved decision, and not editable:
#' leafpm slows to a crawl on boundaries of a few thousand vertices, and a
#' boundary taken from a file should be changed by choosing a different file or
#' tolerance, which the protocol can record, rather than by hand.
#'
#' @param map A leaflet map or proxy.
#' @param area Result of [area_prepare()], or `NULL`.
#'
#' @return The map.
#' @noRd
add_imported_area <- function(map, area) {
  if (is.null(area)) return(map)
  leaflet::addPolygons(
    map, data = area, color = "#0d6efd", weight = 2, fillOpacity = 0.15,
    label = "Imported area", group = "imported"
  )
}

#' Format an area for display
#'
#' @param m2 Area in square metres.
#'
#' @return A string in hectares below 1 km², in km² above.
#' @noRd
format_area <- function(m2) {
  if (m2 < 1e6) {
    sprintf("%s ha", format(signif(m2 / 1e4, 3), big.mark = " "))
  } else {
    sprintf("%s km²", format(round(m2 / 1e6, if (m2 < 1e8) 1 else 0), big.mark = " ", nsmall = 0))
  }
}
