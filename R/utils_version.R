# Title: Package Version / Embedded-Base Version Utilities
# Adapted from Saira (R/utils_version.R). Reports the running package version and
# the versions of embedded reference bases (from their *.meta.json sidecars).

#' Detect a stale R session (loaded namespace older than the installed package)
#'
#' After a user reinstalls ObservaBio without restarting R, the running session
#' keeps executing the previously loaded namespace while the on-disk package is
#' newer. Under `pkgload::load_all()` both resolve to the same version, so dev
#' sessions are never flagged.
#'
#' @param loaded,installed Version strings; default to the live lookups.
#' @return List with `stale` (logical), `loaded`, `installed` (character).
#' @noRd
ObservaBio_session_is_stale <- function(loaded = NULL, installed = NULL) {
    if (is.null(loaded)) {
        loaded <- tryCatch(
            as.character(getNamespaceVersion("ObservaBio")),
            error = function(e) NA_character_
        )
    }
    if (is.null(installed)) {
        installed <- tryCatch(
            as.character(utils::packageVersion("ObservaBio")),
            error = function(e) NA_character_
        )
    }
    stale <- !is.na(loaded) && !is.na(installed) && !identical(loaded, installed)
    list(stale = stale, loaded = loaded, installed = installed)
}

#' Version actually loaded in this R session
#'
#' @return Character scalar, e.g. "0.0.0.9000", or NA on lookup failure.
#' @noRd
ObservaBio_running_version <- function() {
    tryCatch(
        as.character(getNamespaceVersion("ObservaBio")),
        error = function(e) tryCatch(
            as.character(utils::packageVersion("ObservaBio")),
            error = function(e2) NA_character_
        )
    )
}

#' Path to the metadata sidecar for an embedded base RDS
#'
#' Convention: `foo.rds` -> `foo.meta.json` in the same directory.
#'
#' @param rds_path Path to the base RDS.
#' @return Path to the sidecar (may not exist).
#' @noRd
base_meta_path <- function(rds_path) {
    sub("\\.rds$", ".meta.json", rds_path)
}

#' Read the version metadata of an embedded base
#'
#' Reads the `*.meta.json` sidecar written by the `data-raw/prep_*.R` scripts.
#' Degrades gracefully to a list with NA fields when the sidecar is absent or
#' unreadable, so the UI can always render something.
#'
#' @param rds_path Path to the base RDS whose sidecar to read.
#' @return List with at least `version` and `date` (character; NA when unknown).
#' @noRd
read_base_meta <- function(rds_path) {
    fallback <- list(version = NA_character_, date = NA_character_)
    meta_file <- base_meta_path(rds_path)
    if (!file.exists(meta_file)) {
        return(fallback)
    }
    tryCatch(
        {
            meta <- jsonlite::fromJSON(meta_file)
            if (is.null(meta$version)) meta$version <- NA_character_
            if (is.null(meta$date)) meta$date <- NA_character_
            meta
        },
        error = function(e) fallback
    )
}
