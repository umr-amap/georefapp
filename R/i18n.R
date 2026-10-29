#' Languages the interface is available in
#'
#' @noRd
georef_languages <- c(FR = "fr", EN = "en")

#' A translator for the interface
#'
#' Strings are keyed on their English text and the dictionary lives in
#' `inst/translations/translation.json`, as in the plotsdatabase apps. Only the
#' interface is translated. What goes into the project file and the Darwin Core
#' output -- protocol sentences, decision types, column names -- stays in
#' English: it is data, and must read the same whichever language the session
#' was in.
#'
#' @param language `"fr"` (the default) or `"en"`.
#'
#' @return A `shiny.i18n` Translator set to `language`.
#'
#' @export
georef_translator <- function(language = "fr") {
  language <- match.arg(language, unname(georef_languages))
  path <- system.file("translations", "translation.json", package = "georefapp")
  tr <- shiny.i18n::Translator$new(translation_json_path = path)
  tr$set_translation_language(language)
  tr
}

#' Translate to plain text, for an HTML attribute
#'
#' Once [shiny.i18n::usei18n()] has been called, a translator's `t()` returns a
#' `<span>` the language switch can rewrite. That is what static text needs,
#' but a span cannot go inside an attribute such as a placeholder. Text placed
#' there is fixed in the language the page was built in.
#'
#' @param i18n A translator.
#' @param keyword English text.
#'
#' @return A string.
#' @noRd
i18n_text <- function(i18n, keyword) {
  lang <- i18n$get_translation_language()
  if (identical(lang, i18n$get_key_translation())) return(keyword)
  tab <- i18n$get_translations()
  out <- if (keyword %in% rownames(tab)) tab[keyword, lang] else NA_character_
  if (is.na(out)) keyword else out
}
