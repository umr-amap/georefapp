test_that("the interface is French unless English is asked for", {
  expect_equal(formals(georef_ui)$language, "fr")
  expect_equal(formals(georef_server)$language, "fr")
  expect_equal(formals(launch)$language, "fr")
  expect_equal(georef_translator()$get_translation_language(), "fr")

  fr <- as.character(htmltools::renderTags(workbench_ui("wb"))$html)
  expect_match(fr, "Enregistrer le géoréférencement", fixed = TRUE)
  en <- as.character(htmltools::renderTags(workbench_ui("wb", georef_translator("en")))$html)
  expect_match(en, "Save georeference", fixed = TRUE)
  expect_error(georef_translator("de"))
})

test_that("every string the code translates has a French translation", {
  # Read from the sources, so a string added without a translation fails here
  # instead of silently showing in English. Not available from an installed
  # package, where the R/ directory is gone.
  src_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src_dir))
  src <- unlist(lapply(list.files(src_dir, full.names = TRUE), readLines, encoding = "UTF-8"))
  pat <- '(?:\\btr|i18n[$]t|i18n\\(\\)[$]t|i18n_text\\(i18n,\\s*)\\(?"((?:[^"\\\\]|\\\\.)*)"'
  used <- unique(sub(pat, "\\1", unlist(regmatches(src, gregexpr(pat, src, perl = TRUE))), perl = TRUE))
  expect_gt(length(used), 100)

  tab <- georef_translator()$get_translations()
  expect_equal(setdiff(used, rownames(tab)), character())
  expect_false(anyNA(tab[used, "fr"]))
})

test_that("format strings keep their placeholders in French", {
  tab <- georef_translator()$get_translations()
  spec <- function(x) regmatches(x, gregexpr("%[-0-9.]*[dsf]", x))[[1]]
  keys <- rownames(tab)[grepl("%", rownames(tab), fixed = TRUE)]
  for (k in keys) expect_identical(spec(tab[k, "fr"]), spec(k), label = k)
})

test_that("an attribute gets plain text, not a span", {
  tr <- georef_translator()
  tr$use_js()
  expect_s3_class(tr$t("Remarks"), "shiny.tag")
  expect_equal(i18n_text(tr, "Remarks"), "Remarques")
  expect_equal(i18n_text(georef_translator("en"), "Remarks"), "Remarks")
})

test_that("the server builds its pages in the language asked for", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  shiny::testServer(import_server, args = list(i18n = shiny::reactive(georef_translator("fr"))), {
    session$setInputs(use_example = 1)
    session$setInputs(col_locality = "locality", col_id = "catalog_number",
                      col_country = "", col_admin1 = "")
    expect_match(as.character(output$summary$html), "localités distinctes", fixed = TRUE)
  })
})
