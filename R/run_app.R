# Title: Run the ZHOUSE Application
# Adapted from Saira (R/run_app.R), trimmed to the ZHOUSE scope.

#' Run the ZHOUSE Shiny application
#'
#' Starts the app for Darwin Core standardization and biodiversity validation.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @return A Shiny app object.
#' @export
run_app <- function(...) {
    # Allow large uploads (species spreadsheet + shapefile .zip).
    options(shiny.maxRequestSize = 500 * 1024^2)

    www_path <- system.file("app/www", package = "validabio")
    if (www_path == "") {
        www_path <- "inst/app/www"
    }
    if (dir.exists(www_path)) {
        shiny::addResourcePath("www", www_path)
    }

    shiny::shinyApp(
        ui = app_ui(),
        server = app_server,
        options = list(launch.browser = getOption("shiny.launch.browser", interactive())),
        ...
    )
}
