#' Export module: getting the results out of a project
#'
#' Three files. The Darwin Core table is what goes into a database or a
#' publication; the decision log is what makes that table defensible, and the
#' two are meant to travel together; the footprints are the same decisions as
#' GIS layers. All three are regenerated from the project file on demand.
#'
#' @param id Module id.
#' @param con_r A [shiny::reactive()] returning the open project connection.
#' @param refresh_r A [shiny::reactive()] that changes whenever a decision is
#'   written.
#' @param project_r A [shiny::reactive()] returning the project file path, used
#'   to name the exported files and to locate the project folder.
#' @param local_files Offer to write the files next to the project file.
#'
#' @return
#'  * UI: HTML tags for the export page.
#'  * Server: nothing, called for its side effects.
#'
#' @name module-export
#'
#' @export
export_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Your results"),
      bslib::card_body(
        shiny::uiOutput(ns("status")),
        shiny::uiOutput(ns("save_local")),
        shiny::tags$h6("Download"),
        shiny::downloadButton(
          ns("dwc"), "Darwin Core table (CSV)",
          class = "btn-primary w-100 mb-1"
        ),
        shiny::helpText(
          class = "mb-3",
          "One row per imported record, with its coordinates and uncertainty.",
          "This is the table for your dataset or database."
        ),
        shiny::downloadButton(
          ns("log"), "Decision log (CSV)",
          class = "btn-outline-secondary w-100 mb-1"
        ),
        shiny::helpText(
          class = "mb-3",
          "Every decision ever made, revisions included. The audit trail: keep",
          "it with the table, and publish it alongside."
        ),
        shiny::uiOutput(ns("footprints_button")),
        shiny::helpText(
          "Exporting is safe at any time and can be repeated: the project file",
          "already holds every decision, and these files are rebuilt from it."
        )
      )
    ),
    bslib::card(
      bslib::card_header("Darwin Core preview"),
      bslib::card_body(reactable::reactableOutput(ns("preview")))
    )
  )
}

#' @rdname module-export
#'
#' @export
export_server <- function(id, con_r, refresh_r,
                          project_r = shiny::reactive(NULL),
                          local_files = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dwc_r <- shiny::reactive({
      refresh_r()
      con <- con_r()
      if (is.null(con)) return(dwc_empty())
      dwc_table(con)
    })

    footprints_r <- shiny::reactive({
      refresh_r()
      con <- con_r()
      shiny::req(con)
      dwc_footprints(con)
    })

    output$status <- shiny::renderUI({
      refresh_r()
      con <- con_r()
      if (is.null(con)) {
        return(shinyWidgets::alert(
          status = "info",
          "No project open. Open or create one on the Import page."
        ))
      }
      p <- project_progress(con)
      status <- if (p$pending == 0) "success" else "warning"
      shiny::tagList(
        shinyWidgets::alert(
          status = status,
          shiny::tags$b(format(p$records_done, big.mark = " ")),
          "of", format(p$records, big.mark = " "), "records georeferenced.",
          if (p$pending > 0) {
            shiny::tags$span(sprintf(" %d localities still pending.", p$pending))
          },
          if (p$unresolvable > 0) {
            shiny::tags$span(sprintf(" %d marked unresolvable.", p$unresolvable))
          }
        ),
        if (!is.null(project_r())) {
          shiny::tags$p(
            class = "small text-muted",
            "Project file: ", shiny::tags$code(project_r())
          )
        }
      )
    })

    # A browser download lands in the Downloads folder, away from the project.
    # Running locally, the project's own folder is the better home, and saying
    # where the files went is the whole point.
    output$save_local <- shiny::renderUI({
      if (!isTRUE(local_files) || is.null(con_r()) || is.null(project_r())) return(NULL)
      shiny::tagList(
        shiny::actionButton(
          ns("save_local_btn"), "Save all files next to the project",
          icon = shiny::icon("folder-open"), class = "btn-success w-100 mb-1"
        ),
        shiny::helpText(class = "mb-3", "Into ", shiny::tags$code(dirname(project_r()))),
        shiny::uiOutput(ns("saved"))
      )
    })

    saved_rv <- shiny::reactiveVal(NULL)
    shiny::observeEvent(project_r(), saved_rv(NULL), ignoreNULL = FALSE)

    shiny::observeEvent(input$save_local_btn, {
      con <- shiny::req(con_r())
      path <- shiny::req(project_r())
      paths <- shinyWidgets::execute_safely(
        export_project_con(con, path, dirname(path))
      )
      if (!is.null(paths)) saved_rv(paths)
    })

    output$saved <- shiny::renderUI({
      paths <- saved_rv()
      if (is.null(paths)) return(NULL)
      shinyWidgets::alert(
        status = "success",
        shiny::tags$b("Saved:"),
        shiny::tags$ul(
          class = "mb-0 small",
          lapply(unname(paths), function(p) shiny::tags$li(shiny::tags$code(basename(p))))
        )
      )
    })

    output$footprints_button <- shiny::renderUI({
      fp <- footprints_r()
      n <- nrow(fp$polygons) + nrow(fp$lines)
      if (n == 0) {
        return(shiny::helpText(class = "mb-3", "Footprints (GeoPackage): nothing decided yet."))
      }
      shiny::tagList(
        shiny::downloadButton(
          ns("footprints"), "Footprints (GeoPackage)",
          class = "btn-outline-secondary w-100 mb-1"
        ),
        shiny::helpText(
          class = "mb-3",
          sprintf("%d footprint%s as map layers, for QGIS or ArcGIS: areas and uncertainty envelopes.",
                  n, if (n > 1) "s" else "")
        )
      )
    })

    output$preview <- reactable::renderReactable({
      dat <- dwc_r()
      reactable::reactable(
        utils::head(dat, 200),
        compact = TRUE, bordered = TRUE, searchable = TRUE, resizable = TRUE,
        defaultPageSize = 15,
        defaultColDef = reactable::colDef(
          style = list(whiteSpace = "nowrap", textOverflow = "ellipsis"),
          minWidth = 120
        ),
        columns = list(
          footprintWKT = reactable::colDef(show = FALSE),
          georeferenceProtocol = reactable::colDef(minWidth = 260)
        )
      )
    })

    output$dwc <- shiny::downloadHandler(
      filename = function() export_file_name(project_r(), "dwc", "csv"),
      content = function(file) readr::write_csv(dwc_r(), file, na = "")
    )

    output$log <- shiny::downloadHandler(
      filename = function() export_file_name(project_r(), "log", "csv"),
      content = function(file) {
        con <- con_r()
        dat <- if (is.null(con)) tibble::tibble() else store_decisions(con)
        readr::write_csv(dat, file, na = "")
      }
    )

    output$footprints <- shiny::downloadHandler(
      filename = function() export_file_name(project_r(), "footprints", "gpkg"),
      content = function(file) {
        # Written under a .gpkg name and copied: the temporary file Shiny hands
        # over has no extension, and GDAL will not create a GeoPackage without.
        tmp <- tempfile(fileext = ".gpkg")
        on.exit(unlink(tmp), add = TRUE)
        write_footprints_gpkg(footprints_r(), tmp)
        file.copy(tmp, file, overwrite = TRUE)
      }
    )

    invisible(NULL)
  })
}
