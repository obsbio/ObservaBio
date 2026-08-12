#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom shiny addResourcePath shinyApp moduleServer NS reactive
#' @importFrom shiny reactiveVal observeEvent req showNotification validate need
#' @importFrom stringr str_detect str_trim str_replace_all
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
    # Register the built-in taxonomic providers so the cascade can resolve them
    # without the caller wiring the registry by hand. Kept resilient: a provider
    # that fails to register must not block package load.
    tryCatch(register_default_providers(), error = function(e) NULL)
}
