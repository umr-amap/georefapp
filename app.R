# Entry point for shiny::runApp() / shinyapps.io deployment.
pkgload::load_all(".", quiet = TRUE)
shiny::shinyApp(ui = georef_ui(), server = georef_server())
