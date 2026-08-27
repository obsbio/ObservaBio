# Title: Embedded-Base Version Utilities
# Adapted from Saira (R/utils_version.R). Reports the version of each embedded
# reference base, read from its *.meta.json sidecar.

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
