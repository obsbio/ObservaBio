# Title: Provider Registry
# Holds the registered taxonomic providers and their cascade order (SPEC §16.3).
# Adding a base = a new provider_<name>.R + one register_provider() call here.

.provider_registry <- new.env(parent = emptyenv())

#' Register a provider in the cascade
#'
#' @param provider A `validabio_provider` from [new_provider()].
#' @return Invisibly the provider id.
#' @noRd
register_provider <- function(provider) {
    if (!inherits(provider, "validabio_provider")) {
        stop("register_provider(): expected a validabio_provider object.")
    }
    assign(provider$id, provider, envir = .provider_registry)
    invisible(provider$id)
}

#' Retrieve registered providers ordered by cascade priority
#'
#' @return List of `validabio_provider` objects, lowest `priority` first.
#' @noRd
get_providers <- function() {
    ids <- ls(envir = .provider_registry)
    if (length(ids) == 0L) {
        return(list())
    }
    providers <- lapply(ids, get, envir = .provider_registry)
    providers[order(vapply(providers, function(p) p$priority, integer(1)))]
}

#' Remove all registered providers (test helper)
#' @noRd
clear_providers <- function() {
    rm(list = ls(envir = .provider_registry), envir = .provider_registry)
    invisible(TRUE)
}

#' Register the built-in providers: Flora BR -> Fauna BR -> GBIF
#'
#' Called from `.onLoad`. Resilient: a provider that fails to construct is
#' skipped rather than blocking package load.
#'
#' @return Invisibly the character vector of registered ids.
#' @noRd
register_default_providers <- function() {
    registered <- character(0)
    for (make in list(florabr_provider, faunabr_provider, gbif_provider)) {
        id <- tryCatch(
            register_provider(make()),
            error = function(e) NULL
        )
        if (!is.null(id)) registered <- c(registered, id)
    }
    invisible(registered)
}
