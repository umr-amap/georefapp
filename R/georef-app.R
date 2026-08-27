#' User interface for the georeferencing application
#'
#' @return A bslib page.
#'
#' @export
georef_ui <- function() {
  bslib::page_navbar(
    id = "nav",
    title = "georefapp",
    theme = bslib::bs_theme(version = 5, primary = "#0d6efd"),
    fillable = FALSE,
    header = htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML(
        ".reactable { font-size: 0.85rem; } .card-header { font-weight: 500; }"
      ))
    ),
    bslib::nav_panel(
      title = "Import",
      value = "import",
      htmltools::tags$div(class = "mt-3", import_ui("import"))
    ),
    bslib::nav_panel(
      title = "Georeference",
      value = "workbench",
      htmltools::tags$div(class = "mt-3", workbench_ui("workbench"))
    ),
    bslib::nav_panel(
      title = "Export",
      value = "export",
      htmltools::tags$div(class = "mt-3", export_ui("export"))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("project_label", inline = TRUE))
  )
}

#' Server for the georeferencing application
#'
#' @param stop_on_close Stop the application, returning control to the R
#'   console, once the last browser window closes. Defaults to `FALSE`: on a
#'   server, one user closing a tab must not take the application down for
#'   everyone. [launch()] turns it on, because there the browser is the only
#'   client and leaving R blocked on a dead app helps nobody.
#'
#' @return A Shiny server function.
#'
#' @export
georef_server <- function(stop_on_close = FALSE) {
  # Counted here rather than inside the session function: the decision to stop
  # is about the application, so it has to outlive any one session.
  open_sessions <- 0L

  function(input, output, session) {
    if (isTRUE(stop_on_close)) {
      open_sessions <<- open_sessions + 1L
      session$onSessionEnded(function() {
        open_sessions <<- open_sessions - 1L
        # A page reload ends the old session before the new one connects, so
        # stopping the moment a session ends would kill the application on
        # every refresh. Wait, then stop only if nothing reconnected -- which
        # also covers a second tab still being open.
        later::later(function() {
          if (open_sessions <= 0L) {
            message("Browser closed - stopping georefapp.")
            shiny::stopApp()
          }
        }, delay = 2)
      })
    }

    project_r <- import_server("import")

    # One connection for the whole session, owned here and handed to the pages
    # that need it. Opening per module would leave a handle behind every time
    # the project changed, and an orphaned handle on a file-backed store can
    # keep a lock alive.
    con_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(project_r(), {
      store_close(shiny::isolate(con_rv()))
      path <- project_r()
      con_rv(if (is.null(path)) NULL else store_open(path))
    }, ignoreNULL = FALSE)

    shiny::onStop(function() shiny::isolate(store_close(con_rv())))

    # Creating a project is the signal to start work, so the application moves
    # to the workbench rather than leaving the user to find it.
    shiny::observeEvent(project_r(), {
      shiny::req(project_r())
      bslib::nav_select("nav", "workbench", session = session)
    })

    refresh_r <- workbench_server("workbench", con_r = con_rv)
    export_server("export", con_r = con_rv, refresh_r = refresh_r)

    output$project_label <- shiny::renderUI({
      path <- project_r()
      if (is.null(path)) {
        return(htmltools::tags$span(class = "navbar-text text-muted small", "no project"))
      }
      htmltools::tags$span(class = "navbar-text small", basename(path))
    })
  }
}

#' Use a gazetteer snapshot for the rest of the session
#'
#' Sets the candidate provider and the snapshot identifier that every decision
#' is stamped with.
#'
#' @param file Snapshot file written by [gazetteer_snapshot_build()], or
#'   `NULL` to work without a dictionary.
#' @param ... Passed to [gazetteer_provider()].
#'
#' @return The provider, invisibly, or `NULL`.
#'
#' @export
use_gazetteer <- function(file, ...) {
  if (is.null(file)) {
    options(georefapp.candidates = NULL, georefapp.candidates_snapshot = NULL)
    return(invisible(NULL))
  }
  p <- gazetteer_provider(file, ...)
  options(georefapp.candidates = p)
  message("Gazetteer: ", gazetteer_info(attr(p, "con"))[["snapshot_id"]])
  invisible(p)
}

#' Launch the georeferencing application
#'
#' @param port Port to serve on.
#' @param launch.browser Whether to open a browser window.
#' @param gazetteer Gazetteer snapshot file, or `NULL` to run without one.
#'   The application is fully usable without a dictionary -- that is the normal
#'   state for most of Central Africa -- so a missing file is reported and then
#'   ignored rather than raised.
#' @param stop_on_close Return to the R console when the last browser window
#'   closes, rather than leaving the console blocked until interrupted.
#'   Reloading the page does not count as closing it. Set `FALSE` to keep the
#'   old behaviour and stop the application with Esc.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Called for its side effect; does not return until the app stops.
#'
#' @examples
#' \dontrun{
#' launch()
#' launch(gazetteer = "rainbio-gazetteer.sqlite")
#' }
#'
#' @export
launch <- function(port = 5792, launch.browser = TRUE,
                   gazetteer = getOption("georefapp.gazetteer"),
                   stop_on_close = TRUE, ...) {
  if (!is.null(gazetteer)) {
    if (file.exists(gazetteer)) {
      p <- use_gazetteer(gazetteer)
      on.exit(gazetteer_close(attr(p, "con")), add = TRUE)
    } else {
      message("No gazetteer at ", gazetteer, " — running without one.")
    }
  }
  app <- shiny::shinyApp(
    ui = georef_ui(),
    server = georef_server(stop_on_close = stop_on_close)
  )
  shiny::runApp(app, port = port, launch.browser = launch.browser, ...)
}
