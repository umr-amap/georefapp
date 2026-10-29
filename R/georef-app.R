#' User interface for the georeferencing application
#'
#' @param language Language the page opens in, `"fr"` (the default) or `"en"`.
#'   A switch in the navigation bar changes it during the session.
#'
#' @return A bslib page.
#'
#' @export
georef_ui <- function(language = "fr") {
  i18n <- georef_translator(language)
  # usei18n() must run before any t() call: it is what makes t() return spans
  # the language switch can rewrite in the browser.
  i18n_head <- shiny.i18n::usei18n(i18n)
  tr <- i18n$t
  bslib::page_navbar(
    id = "nav",
    title = "georefapp",
    theme = bslib::bs_theme(version = 5, primary = "#0d6efd"),
    fillable = FALSE,
    header = htmltools::tagList(
      i18n_head,
      htmltools::tags$head(
        htmltools::tags$style(htmltools::HTML(
          ".reactable { font-size: 0.85rem; } .card-header { font-weight: 500; }"
        ))
      )
    ),
    bslib::nav_panel(
      title = tr("Import"),
      value = "import",
      htmltools::tags$div(class = "mt-3", import_ui("import", i18n))
    ),
    bslib::nav_panel(
      title = tr("Georeference"),
      value = "workbench",
      htmltools::tags$div(class = "mt-3", workbench_ui("workbench", i18n))
    ),
    bslib::nav_panel(
      title = tr("Export"),
      value = "export",
      htmltools::tags$div(class = "mt-3", export_ui("export", i18n))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("project_label", inline = TRUE)),
    bslib::nav_item(
      shinyWidgets::radioGroupButtons(
        "lang", NULL, choices = georef_languages, selected = language,
        size = "sm"
      )
    )
  )
}

#' Server for the georeferencing application
#'
#' @param stop_on_close Stop the application, returning control to the R
#'   console, once the last browser window closes. Defaults to `FALSE`: on a
#'   server, one user closing a tab must not take the application down for
#'   everyone. [launch()] turns it on, because there the browser is the only
#'   client and leaving R blocked on a dead app helps nobody.
#' @param local_files Offer to write exports next to the project file, rather
#'   than only as browser downloads. Defaults to `FALSE`: on a server the
#'   project folder is the server's, not the user's. [launch()] turns it on.
#' @param language Language the session starts in, matching [georef_ui()].
#'
#' @return A Shiny server function.
#'
#' @export
georef_server <- function(stop_on_close = FALSE, local_files = FALSE,
                          language = "fr") {
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

    # One translator per session: a Translator holds its language as state, and
    # on a server one user switching to English must not switch everybody.
    translator <- georef_translator(language)
    lang_r <- shiny::reactive(input$lang %||% language)
    i18n <- shiny::reactive({
      translator$set_translation_language(lang_r())
      translator
    })
    # Also on start-up: the browser script first resets every static string to
    # the key language, English, and has to be told which one was asked for.
    shiny::observeEvent(lang_r(), shiny.i18n::update_lang(lang_r(), session))

    project_r <- import_server("import", i18n = i18n)

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

    refresh_r <- workbench_server(
      "workbench", con_r = con_rv,
      on_export = function() bslib::nav_select("nav", "export", session = session),
      i18n = i18n
    )
    export_server("export", con_r = con_rv, refresh_r = refresh_r,
                  project_r = project_r, local_files = local_files, i18n = i18n)

    # The project file is where the work lives, so where it is stays on screen.
    output$project_label <- shiny::renderUI({
      tr <- i18n()$t
      path <- project_r()
      if (is.null(path)) {
        return(htmltools::tags$span(class = "navbar-text text-muted small", tr("no project")))
      }
      htmltools::tags$span(
        class = "navbar-text small", title = path,
        tr("Project"), " ", htmltools::tags$b(basename(path)),
        htmltools::tags$span(class = "text-muted", " · ", tr("saved as you go"), " · "),
        htmltools::tags$a(
          href = "#", class = "text-decoration-none",
          onclick = sprintf(
            "navigator.clipboard.writeText(%s); this.textContent = %s; return false;",
            jsonlite::toJSON(path, auto_unbox = TRUE),
            jsonlite::toJSON(tr("path copied"), auto_unbox = TRUE)
          ),
          tr("copy path")
        )
      )
    })
  }
}

#' Use a gazetteer snapshot for the rest of the session
#'
#' Sets the candidate provider and the snapshot identifier that every decision
#' is stamped with. Several snapshots can be used at once -- RAINBIO and a
#' dictionary built for one source, say -- in which case their candidates are
#' listed one dictionary after the other (see [candidates_combine()]) and every
#' decision is stamped with all of their identifiers.
#'
#' @param file Snapshot file(s) written by [gazetteer_snapshot_build()], or
#'   `NULL` to work without a dictionary.
#' @param ... Passed to [gazetteer_provider()].
#'
#' @return The provider, invisibly, or `NULL`. Its open connection(s) are
#'   attached as the `con` attribute.
#'
#' @export
use_gazetteer <- function(file, ...) {
  if (is.null(file)) {
    options(georefapp.candidates = NULL, georefapp.candidates_snapshot = NULL)
    return(invisible(NULL))
  }
  ps <- lapply(file, gazetteer_provider, ...)
  ids <- vapply(ps, function(p) unname(gazetteer_info(attr(p, "con"))[["snapshot_id"]]),
                character(1))
  if (length(ps) == 1L) {
    p <- ps[[1]]
  } else {
    p <- do.call(candidates_combine, ps)
    attr(p, "con") <- lapply(ps, attr, "con")
  }
  # Set after building: each gazetteer_provider() call stamps its own id, and
  # the decision must name every dictionary that was on screen.
  id <- paste(ids, collapse = " + ")
  options(georefapp.candidates = p, georefapp.candidates_snapshot = id)
  message("Gazetteer: ", id)
  invisible(p)
}

#' Launch the georeferencing application
#'
#' @param port Port to serve on.
#' @param launch.browser Whether to open a browser window.
#' @param gazetteer Gazetteer snapshot file, several of them, or `NULL` to run
#'   without one. The application is fully usable without a dictionary -- that
#'   is the normal state for most of Central Africa -- so a missing file is
#'   reported and then ignored rather than raised.
#' @param stop_on_close Return to the R console when the last browser window
#'   closes, rather than leaving the console blocked until interrupted.
#'   Reloading the page does not count as closing it. Set `FALSE` to keep the
#'   old behaviour and stop the application with Esc.
#' @param language Language the interface opens in: `"fr"` (the default) or
#'   `"en"`. It can be switched from the navigation bar at any time.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Called for its side effect; does not return until the app stops.
#'
#' @examples
#' \dontrun{
#' launch()
#' launch(gazetteer = "rainbio-gazetteer.sqlite")
#' launch(gazetteer = c("rainbio-gazetteer.sqlite", "mandango1982-iles.sqlite"))
#' }
#'
#' @export
launch <- function(port = 5792, launch.browser = TRUE,
                   gazetteer = getOption("georefapp.gazetteer"),
                   stop_on_close = TRUE, language = "fr", ...) {
  if (!is.null(gazetteer)) {
    found <- file.exists(gazetteer)
    for (g in gazetteer[!found]) message("No gazetteer at ", g, " — running without it.")
    if (any(found)) {
      p <- use_gazetteer(gazetteer[found])
      on.exit(gazetteer_close(attr(p, "con")), add = TRUE)
    }
  }
  app <- shiny::shinyApp(
    ui = georef_ui(language),
    server = georef_server(stop_on_close = stop_on_close, local_files = TRUE,
                           language = language)
  )
  shiny::runApp(app, port = port, launch.browser = launch.browser, ...)
}
