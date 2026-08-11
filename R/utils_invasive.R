# Title: Invasive Alien Species Cross-Check
# A status cross-check, exactly like the MMA list (SPEC §5): the embedded base
# (three national lists, unioned in data-raw/generate_invasive_species.R) is
# matched offline by normalized name and answers "is this taxon listed as an
# invasive alien species in Brazil, and by which list?".
#
# It is NOT a taxonomic provider: it never resolves a name, never touches the
# cascade, the provider contract or the registry. Like `distributionFlag` and the
# conservation status, the signal is an ALERT, NOT A VERDICT -- an occurrence of a
# listed taxon still needs a human call.
#
# Every path degrades to NA: a missing base must never abort the pipeline.

.invasive_cache <- create_rds_cache("invasive_species")

#' Canonical match key for the invasive base
#'
#' The single recipe used on BOTH sides of the join -- by
#' `data-raw/generate_invasive_species.R` when it builds the base and by
#' [invasive_lookup()] at runtime -- so a name carrying an author or a
#' cf./aff. qualifier lands on the same key from either side (LESSONS L-006).
#'
#' Author-stripping matters here in a way it does not for [mma_lookup()]: the MMA
#' lookup only ever sees cascade-resolved names, while this one also sees the
#' names of already-validated rows, straight from the user's sheet, where
#' "Abrus precatorius L." is perfectly normal.
#'
#' @param scientific_names Character vector.
#' @return Character vector of match keys (NA where the name is unusable).
#' @noRd
invasive_match_key <- function(scientific_names) {
    canonical <- vapply(
        as.character(scientific_names),
        function(nm) {
            normalize_scientific_name(nm, remove_authors = TRUE, ignore_qualifiers = TRUE)
        },
        FUN.VALUE = character(1), USE.NAMES = FALSE
    )
    normalize_for_matching(canonical)
}

#' Load and cache the embedded invasive-species base
#'
#' @return Data frame with `scientificName`, `match_key`, `invasiveSource`.
#' @noRd
invasive_load_data <- function() {
    cached <- .invasive_cache$get()
    if (!is.null(cached)) {
        return(cached)
    }
    path <- br_extdata_path("invasive_species.rds")
    if (!file.exists(path)) {
        stop(sprintf(
            "Invasive-species base not found at '%s'. Run data-raw/generate_invasive_species.R.",
            path
        ))
    }
    data <- readRDS(path)
    .invasive_cache$set(data)
    data
}

#' Invasive-species status for a vector of scientific names
#'
#' @param scientific_names Character vector.
#' @return Data frame with `scientificName`, `invasive` (TRUE where the taxon is
#'   listed, NA otherwise -- never FALSE, mirroring `statusMMA`, where NA reads
#'   "not on the list") and `invasiveSource` (the list(s) that carry it), same
#'   length/order as the input.
#' @noRd
invasive_lookup <- function(scientific_names) {
    n <- length(scientific_names)
    out <- data.frame(
        scientificName = as.character(scientific_names),
        invasive       = rep(NA, n),
        invasiveSource = rep(NA_character_, n),
        stringsAsFactors = FALSE
    )
    if (n == 0L) {
        return(out)
    }

    data <- tryCatch(invasive_load_data(), error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0L) {
        return(out)
    }

    keys <- invasive_match_key(scientific_names)
    idx <- match(keys, data$match_key)
    hits <- which(!is.na(idx) & is_non_empty(keys))
    out$invasive[hits] <- TRUE
    out$invasiveSource[hits] <- as.character(data$invasiveSource[idx[hits]])
    out
}

#' Cross-check every species of an upload against the invasive base
#'
#' Runs over ALL species in the sheet -- the ones the cascade resolved *and* the
#' already-validated rows it skipped -- keyed the way taxonomy and
#' `distributionFlag` are, so the result joins records identically
#' ([build_dwc_output()]) and names identically ([build_audit_table()]). The check
#' is offline and costs nothing per species, so there is no reason to restrict it
#' to the new names the way the IUCN HTTP lookup is restricted.
#'
#' Reuses [geo_name_map()] for "one row per unique species, final accepted name,
#' keyed by normalized query name" -- the same map the geographic verification
#' runs on.
#'
#' @param records Data frame of uploaded records.
#' @param cascade Cascade result (canonical schema), or NULL.
#' @return Named character vector: `invasiveSource` values, named by the species'
#'   normalized `query_name`. **Only listed species are present** -- membership is
#'   the TRUE/NA signal. Empty (but named) when nothing is listed or the base is
#'   unavailable.
#' @noRd
resolve_invasive_flags <- function(records, cascade) {
    empty <- stats::setNames(character(0), character(0))
    map <- geo_name_map(records, cascade)
    if (nrow(map) == 0L) {
        return(empty)
    }
    found <- invasive_lookup(map$scientificName)
    hits <- which(!is.na(found$invasive))
    if (length(hits) == 0L) {
        return(empty)
    }
    stats::setNames(found$invasiveSource[hits], map$query_name[hits])
}
