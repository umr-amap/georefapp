#' Export module: Darwin Core output and the audit trail
#'
#' Two downloads. The Darwin Core table is what goes into a database or a
#' publication; the decision log is what makes that table defensible, and the
#' two are meant to travel together.
#'
#' @param id Module id.
#' @param con_r A [shiny::reactive()] returning the open project connection.
#' @param refresh_r A [shiny::reactive()] that changes whenever a decision is
#'   written.
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
      bslib::card_header("Export"),
      bslib::card_body(
        shiny::uiOutput(ns("status")),
        shiny::downloadButton(
          ns("dwc"), "Darwin Core table (CSV)",
          class = "btn-primary w-100 mb-2"
        ),
        shiny::downloadButton(
          ns("log"), "Decision log (CSV)",
          class = "btn-outline-secondary w-100 mb-2"
        ),
        shiny::helpText(
          "The Darwin Core table is one row per imported record. The decision",
          "log is one row per decision ever made, superseded ones included; it",
          "is the audit trail and should accompany any published dataset."
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
export_server <- function(id, con_r, refresh_r) {
  shiny::moduleServer(id, function(input, output, session) {

    dwc_r <- shiny::reactive({
      refresh_r()
      con <- con_r()
      if (is.null(con)) return(dwc_empty())
      dwc_table(con)
    })

    output$status <- shiny::renderUI({
      refresh_r()
      con <- con_r()
      if (is.null(con)) {
        return(shinyWidgets::alert(status = "info", "No project open."))
      }
      p <- project_progress(con)
      status <- if (p$pending == 0) "success" else "warning"
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
      filename = function() sprintf("georef_dwc_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) readr::write_csv(dwc_r(), file, na = "")
    )

    output$log <- shiny::downloadHandler(
      filename = function() sprintf("georef_log_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        con <- con_r()
        dat <- if (is.null(con)) tibble::tibble() else store_decisions(con)
        readr::write_csv(dat, file, na = "")
      }
    )

    invisible(NULL)
  })
}
