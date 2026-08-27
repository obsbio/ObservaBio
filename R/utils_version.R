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

#' Version of every reference base the app ships with
#'
#' Two sources feed this. A registered provider answers through the contract, so
#' a base added later appears here without this function changing. The layers
#' that are not providers (the UF/biome geometry, the invasive lists, the MMA
#' list) are read from their `*.meta.json` sidecars, because nothing queries them
#' by name and they never join the cascade registry — but the client regenerates
#' them on the same annual pass, so the app must report them too.
#'
#' Labels are PT-BR: this feeds the UI directly (SPEC §2.1), like the `label`
#' field the providers already carry.
#'
#' @param providers Registered providers. Defaults to the registry.
#' @return Data frame `label`/`version`, one row per base, providers first.
#'   An unreadable version reads as an em dash rather than `NA`.
#' @noRd
reference_bases_versions <- function(providers = get_providers()) {
    one <- function(label, version) {
        if (length(version) != 1L) {
            version <- NA_character_
        }
        data.frame(label = as.character(label), version = as.character(version),
                   stringsAsFactors = FALSE)
    }

    from_provider <- lapply(providers, function(p) {
        one(p$label, tryCatch(p$version(), error = function(e) NA_character_))
    })

    from_sidecar <- lapply(
        list(
            list(file = "br_uf_biomes.rds", label = "Estados e biomas (IBGE)"),
            list(file = "invasive_species.rds", label = "Espécies exóticas invasoras"),
            list(file = "sensitive_species.rds", label = "Lista de ameaçadas (MMA)")
        ),
        function(base) {
            meta <- read_base_meta(br_extdata_path(base$file))
            one(base$label, meta$version %||% NA_character_)
        }
    )

    out <- do.call(rbind, c(from_provider, from_sidecar))
    if (is.null(out) || nrow(out) == 0L) {
        return(data.frame(label = character(0), version = character(0),
                          stringsAsFactors = FALSE))
    }
    blank <- is.na(out$version) | !nzchar(trimws(out$version))
    out$version[blank] <- "—"
    rownames(out) <- NULL
    out
}
