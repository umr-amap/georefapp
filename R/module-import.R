#' Import module: open a project, or create one from a locality table
#'
#' Lists the projects already in the working directory so that work can be
#' resumed. Otherwise reads a delimited or Excel file, lets the user say which
#' column holds the locality text, and writes the resulting records into a new
#' project file -- asking first if a project of that name already exists.
#'
#' @param id Module id.
#'
#' @return
#'  * UI: HTML tags for the import page.
#'  * Server: a [shiny::reactiveVal()] holding the path of the project that was
#'    created or opened, or `NULL` before there is one.
#'
#' @name module-import
#'
#' @export
import_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Project"),
      shiny::tags$h6("Continue a project"),
      shiny::uiOutput(ns("existing")),
      shiny::tags$hr(),
      shiny::tags$h6("Or start a new one: 1. Locality table"),
      shiny::fileInput(
        ns("file"), "CSV, TSV or Excel file",
        accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"), width = "100%"
      ),
      shiny::actionLink(ns("use_example"), "or load the bundled example"),
      shiny::uiOutput(ns("mapping")),
      shiny::uiOutput(ns("create"))
    ),
    bslib::card(
      bslib::card_header("Preview"),
      bslib::card_body(
        shiny::uiOutput(ns("summary")),
        reactable::reactableOutput(ns("preview"))
      )
    )
  )
}

#' @rdname module-import
#'
#' @export
import_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    raw_rv <- shiny::reactiveVal(NULL)
    project_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      dat <- shinyWidgets::execute_safely(
        read_locality_file(input$file$datapath, input$file$name)
      )
      raw_rv(dat)
      project_rv(NULL)
    })

    shiny::observeEvent(input$use_example, {
      raw_rv(read_locality_file(example_localities_path(), "example_localities.csv"))
      project_rv(NULL)
    })

    output$mapping <- shiny::renderUI({
      dat <- raw_rv()
      shiny::req(dat)
      cols <- names(dat)
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$h6("2. Which column is which"),
        shiny::selectInput(
          ns("col_locality"), "Locality text (required)",
          choices = cols, selected = guess_column(cols, c("locality", "localite", "lieu", "site", "place")),
          width = "100%"
        ),
        shiny::selectInput(
          ns("col_id"), "Record identifier",
          choices = c("(generate one)" = "", cols),
          selected = guess_column(cols, c("id", "record", "barcode", "catalog"), default = ""),
          width = "100%"
        ),
        shiny::selectInput(
          ns("col_country"), "Country",
          choices = c("(none)" = "", cols),
          selected = guess_column(cols, c("country", "pays"), default = ""),
          width = "100%"
        ),
        shiny::selectInput(
          ns("col_admin1"), "Province or region",
          choices = c("(none)" = "", cols),
          selected = guess_column(cols, c("province", "region", "admin1", "state"), default = ""),
          width = "100%"
        )
      )
    })

    records_r <- shiny::reactive({
      dat <- raw_rv()
      shiny::req(dat, input$col_locality)
      build_records(
        dat,
        col_locality = input$col_locality,
        col_id = nzchar_or_null(input$col_id),
        col_country = nzchar_or_null(input$col_country),
        col_admin1 = nzchar_or_null(input$col_admin1)
      )
    })

    output$summary <- shiny::renderUI({
      recs <- shiny::req(records_r())
      n_key <- length(unique(stats::na.omit(recs$locality_key)))
      n_blank <- sum(is.na(recs$locality_key))
      shiny::tagList(
        shinyWidgets::alert(
          status = "info",
          shiny::tags$b(format(nrow(recs), big.mark = " ")), "records collapse to",
          shiny::tags$b(format(n_key, big.mark = " ")), "distinct localities",
          shiny::tags$span(
            class = "text-muted",
            sprintf(" (%.0f%% of the work removed by grouping)",
                    100 * (1 - n_key / max(nrow(recs), 1)))
          )
        ),
        if (n_blank > 0) {
          shinyWidgets::alert(
            status = "warning",
            format(n_blank, big.mark = " "),
            "records have no usable locality text and will be carried through with empty coordinates."
          )
        }
      )
    })

    output$preview <- reactable::renderReactable({
      recs <- shiny::req(records_r())
      reactable::reactable(
        utils::head(recs[, c("record_id", "verbatim_locality", "locality_key", "country")], 200),
        compact = TRUE, bordered = TRUE, searchable = TRUE, resizable = TRUE,
        defaultPageSize = 10,
        columns = list(
          record_id = reactable::colDef(name = "Record", maxWidth = 140),
          verbatim_locality = reactable::colDef(name = "Verbatim locality"),
          locality_key = reactable::colDef(name = "Grouping key"),
          country = reactable::colDef(name = "Country", maxWidth = 140)
        )
      )
    })

    output$create <- shiny::renderUI({
      shiny::req(records_r())
      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$h6("3. Project file"),
        shiny::textInput(
          ns("project_name"), NULL,
          value = default_project_name(), width = "100%"
        ),
        shiny::helpText(
          "Saved in", shiny::tags$code(normalizePath(getwd(), winslash = "/")),
          "unless you type a full path. This file holds the records and every",
          "decision you make, saved as you go; open it again from this page to",
          "carry on."
        ),
        shiny::actionButton(
          ns("create_btn"), "Create project and start georeferencing",
          class = "btn-primary", width = "100%"
        )
      )
    })

    # -- Continue a project -----------------------------------------------------

    # Listed from the working directory, where new projects are written, so the
    # project a user made last time is simply there to pick. Re-listed whenever
    # a project is created or opened.
    projects_r <- shiny::reactive({
      project_rv()
      find_projects(getwd())
    })

    output$existing <- shiny::renderUI({
      found <- projects_r()
      shiny::tagList(
        if (nrow(found) > 0) {
          labels <- sprintf(
            "%s  (%s records, %s decisions, %s)", found$name,
            format(found$n_records, big.mark = " "),
            format(found$n_decisions, big.mark = " "),
            format(found$modified, "%Y-%m-%d %H:%M")
          )
          shiny::selectInput(ns("open_existing"), NULL,
                             choices = stats::setNames(found$path, labels),
                             width = "100%")
        } else {
          shiny::helpText("No project in the working directory.")
        },
        shiny::textInput(
          ns("open_path"), NULL, width = "100%",
          placeholder = "…or the full path to a project file elsewhere"
        ),
        shiny::actionButton(ns("open_btn"), "Open project",
                            class = "btn-outline-primary", width = "100%")
      )
    })

    open_project <- function(path) {
      if (!is_project_file(path)) {
        shiny::showNotification(
          paste("Not a georefapp project:", path), type = "error", duration = 8
        )
        return(invisible(FALSE))
      }
      # Cleared first, so that reopening the project already open still counts
      # as a change and takes the user back to the workbench.
      project_rv(NULL)
      project_rv(normalizePath(path, winslash = "/"))
      invisible(TRUE)
    }

    shiny::observeEvent(input$open_btn, {
      typed <- as_chr1(input$open_path)
      path <- if (!is.na(typed)) project_path(typed) else as_chr1(input$open_existing)
      if (is.na(path)) {
        shiny::showNotification("Choose a project to open.", type = "warning")
        return()
      }
      open_project(path)
    })

    # -- Create a project -------------------------------------------------------

    create_project <- function(path) {
      recs <- shiny::req(records_r())
      ok <- shinyWidgets::execute_safely({
        con <- store_open(path)
        on.exit(store_close(con), add = TRUE)
        store_write_records(con, recs)
        store_meta_set(con, "source_file", as_chr1(attr(recs, "source_name")))
        store_meta_set(con, "imported_at", iso_now())
        TRUE
      })
      if (isTRUE(ok)) {
        project_rv(NULL)
        project_rv(path)
      }
    }

    pending_path_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$create_btn, {
      shiny::req(records_r())
      path <- project_path(input$project_name)
      if (!file.exists(path)) return(create_project(path))

      if (!is_project_file(path)) {
        shiny::showNotification(
          paste(basename(path), "already exists and is not a georefapp project. Choose another name."),
          type = "error", duration = 8
        )
        return()
      }
      # Creating over an existing project used to replace its records without
      # a word. Its decisions survived, but the user had no way of knowing that
      # they had just reopened old work rather than started new.
      n <- project_counts(path)
      pending_path_rv(path)
      shiny::showModal(shiny::modalDialog(
        title = "This project already exists",
        shiny::tags$p(
          shiny::tags$b(basename(path)), sprintf(
            "already holds %s records and %s decisions.",
            format(n$n_records, big.mark = " "), format(n$n_decisions, big.mark = " ")
          )
        ),
        shiny::tags$p(
          "Open it to carry on where you left off. Re-importing replaces its",
          "records with this table and keeps every decision, which is the way to",
          "bring in a corrected table. To start afresh, cancel and choose another name."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("reimport_btn"), "Re-import records",
                              class = "btn-outline-danger"),
          shiny::actionButton(ns("open_instead_btn"), "Open it", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$open_instead_btn, {
      shiny::removeModal()
      shiny::req(pending_path_rv())
      open_project(pending_path_rv())
    })

    shiny::observeEvent(input$reimport_btn, {
      shiny::removeModal()
      shiny::req(pending_path_rv())
      create_project(pending_path_rv())
    })

    project_rv
  })
}

#' Read a locality table from disk
#'
#' @param path Path to the file.
#' @param name Original file name, used to pick a reader and recorded in the
#'   project metadata.
#'
#' @return A data frame with a `source_name` attribute.
#' @noRd
read_locality_file <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name))
  dat <- switch(
    ext,
    xlsx = ,
    xls = readxl::read_excel(path),
    tsv = readr::read_tsv(path, show_col_types = FALSE, progress = FALSE),
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  )
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  if (ncol(dat) == 0) stop("No columns found in ", name)
  attr(dat, "source_name") <- name
  dat
}

#' Turn an imported table into records
#'
#' @param dat Imported data frame.
#' @param col_locality Name of the locality text column.
#' @param col_id,col_country,col_admin1 Optional column names.
#'
#' @return A data frame of records ready for [store_write_records()]. Columns
#'   not mapped to a known field are kept as JSON in `extra`, so nothing from
#'   the source table is lost.
#' @noRd
build_records <- function(dat, col_locality, col_id = NULL,
                          col_country = NULL, col_admin1 = NULL) {
  verbatim <- trimws(as.character(dat[[col_locality]]))
  verbatim[is.na(verbatim) | !nzchar(verbatim)] <- NA_character_

  ids <- if (!is.null(col_id)) as.character(dat[[col_id]]) else sprintf("r%06d", seq_len(nrow(dat)))
  # Duplicate or missing identifiers would silently collide in the store, and
  # the record identifier is what ties the output back to the user's own data.
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) > 0) {
    ids <- sprintf("r%06d", seq_len(nrow(dat)))
  }

  mapped <- c(col_locality, col_id, col_country, col_admin1)
  rest <- setdiff(names(dat), mapped)
  extra <- if (length(rest) > 0) {
    vapply(seq_len(nrow(dat)), function(i) {
      as.character(jsonlite::toJSON(as.list(dat[i, rest, drop = FALSE]), auto_unbox = TRUE))
    }, character(1))
  } else {
    rep(NA_character_, nrow(dat))
  }

  key <- normalise_locality(verbatim)
  key[is.na(key)] <- no_locality_key

  out <- data.frame(
    record_id = ids,
    locality_key = key,
    verbatim_locality = ifelse(is.na(verbatim), "", verbatim),
    country = if (!is.null(col_country)) as.character(dat[[col_country]]) else NA_character_,
    admin1 = if (!is.null(col_admin1)) as.character(dat[[col_admin1]]) else NA_character_,
    extra = extra,
    stringsAsFactors = FALSE
  )
  attr(out, "source_name") <- attr(dat, "source_name")
  out
}

#' Guess which column the user means
#'
#' @param cols Available column names.
#' @param patterns Substrings to look for, in order of preference.
#' @param default Value returned when nothing matches.
#'
#' @return A column name, or `default`.
#' @noRd
guess_column <- function(cols, patterns, default = cols[1]) {
  low <- tolower(cols)
  for (p in patterns) {
    hit <- which(grepl(p, low, fixed = TRUE))
    if (length(hit) > 0) return(cols[hit[1]])
  }
  default
}

#' Treat an empty selectInput value as absent
#'
#' @param x Input value.
#'
#' @return `x`, or `NULL` when it is empty.
#' @noRd
nzchar_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) NULL else x
}

#' Default name for a new project file
#'
#' @return A single string.
#' @noRd
default_project_name <- function() {
  sprintf("georef_%s.sqlite", format(Sys.Date(), "%Y%m%d"))
}

#' Resolve a project file name to a path
#'
#' @param name File name as typed by the user.
#'
#' @return An absolute path ending in `.sqlite`. A bare name is resolved against
#'   the working directory; a path the user typed in full is honoured as given.
#' @noRd
project_path <- function(name) {
  name <- as_chr1(name)
  if (is.na(name)) name <- default_project_name()
  if (!grepl("\\.sqlite$", name)) name <- paste0(name, ".sqlite")
  absolute <- grepl("^(/|\\\\|[A-Za-z]:)", name)
  normalizePath(if (absolute) name else file.path(getwd(), name), mustWork = FALSE)
}

#' Path to the bundled example locality table
#'
#' @return A file path.
#' @noRd
example_localities_path <- function() {
  system.file("extdata", "example_localities.csv", package = "georefapp")
}
