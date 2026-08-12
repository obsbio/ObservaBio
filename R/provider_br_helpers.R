# Title: Brazilian Provider Helpers
# Shared load + mapping for the embedded Flora BR / Fauna BR providers. Both
# florabr::check_names() and faunabr::check_fauna_names() return the same
# Spelling / Suggested name / taxonomicStatus / family shape (Saira
# normalize_brprovider_result), so one mapper serves both.

.br_cache <- new.env(parent = emptyenv())

#' Resolve the on-disk path to an embedded BR base RDS
#'
#' Uses `system.file()` for the installed package and falls back to the source
#' tree for `pkgload::load_all()` dev sessions.
#'
#' @param filename Basename under `inst/extdata/`.
#' @return Path (possibly non-existent — check with `file.exists`).
#' @noRd
br_extdata_path <- function(filename) {
    path <- system.file("extdata", filename, package = "ObservaBio")
    if (nzchar(path)) {
        return(path)
    }
    file.path("inst", "extdata", filename)
}

#' Load and cache an embedded BR base
#'
#' Rows without a `species` binomial (genus, family, tribe … ranks) are dropped
#' on load — see the note in the body for why that is not merely a size win.
#'
#' @param provider_id Cache key / provider id.
#' @param filename Basename of the RDS under `inst/extdata/`.
#' @param force_reload Bypass the cache.
#' @return The base data frame, restricted to rows carrying a `species` binomial.
#' @noRd
br_load_data <- function(provider_id, filename, force_reload = FALSE) {
    key <- paste0("data_", provider_id)
    if (!force_reload && !is.null(.br_cache[[key]])) {
        return(.br_cache[[key]])
    }
    path <- br_extdata_path(filename)
    if (!file.exists(path)) {
        stop(sprintf(
            "Embedded base for provider '%s' not found at '%s'. Run data-raw/prep_%s.R.",
            provider_id, path, provider_id
        ))
    }
    data <- readRDS(path)
    # Supra-specific rows (genus, family, tribe, ...) carry `species = NA` and can
    # never match a binomial, so every consumer here already ignores them. Keeping
    # them is not just dead weight, it is an OOM: check_fauna_names() only drops
    # non-species ranks when `include_subspecies = FALSE`, so with TRUE (what
    # faunabr_query passes) they reach its merge(all = TRUE) — where NA matches NA,
    # and each unresolved name crosses with all 42,660 of them. Measured at ~485 MB
    # per unresolved name; 363 mixed names peaked at 4.8 GB and the server killed
    # the process. Subspecies rows carry a filled `species`, so this is neutral.
    if ("species" %in% names(data)) {
        data <- data[!is.na(data$species), , drop = FALSE]
        rownames(data) <- NULL
    }
    .br_cache[[key]] <- data
    data
}

#' Cached, normalized species-key index for an embedded BR base
#'
#' The distribution lookup matches names against the base `species` binomial via
#' `normalize_for_matching()`. Normalizing all ~160-200k rows on every call is
#' wasteful, so the key vector is cached next to the base (per provider, session).
#'
#' @param provider_id Cache key / provider id.
#' @param data The embedded base.
#' @return Character vector of normalized `species` values (one per row).
#' @noRd
br_species_key <- function(provider_id, data) {
    key <- paste0("spkey_", provider_id)
    cached <- .br_cache[[key]]
    if (!is.null(cached) && length(cached) == nrow(data)) {
        return(cached)
    }
    spk <- normalize_for_matching(data$species)
    .br_cache[[key]] <- spk
    spk
}

#' Names an embedded BR base carries verbatim (no fuzzy matching)
#'
#' Backed by the cached normalized key from [br_species_key()], so this is a hash
#' lookup rather than a scan. `run_cascade()` uses it to stop paying a
#' higher-priority base's fuzzy pass for a name a lower-priority base holds
#' outright — `check_names()`/`check_fauna_names()` spend ~0.45 s per unmatched
#' name inside `agrep()`.
#'
#' @param provider_id Provider id (cache key).
#' @param names Character vector of scientific names.
#' @param data The embedded base, or NULL to load the cached base.
#' @param filename Base RDS basename, used to load when `data` is NULL.
#' @return The subset of `names` present in the base as a `species` binomial.
#' @noRd
br_exact_match <- function(provider_id, names, data = NULL, filename) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(character(0))
    }
    if (is.null(data)) {
        data <- br_load_data(provider_id, filename)
    }
    if (!is.data.frame(data) || nrow(data) == 0L || !"species" %in% names(data)) {
        return(character(0))
    }
    names_chr[normalize_for_matching(names_chr) %in% br_species_key(provider_id, data)]
}

#' UF/biome distribution for a set of species from an embedded BR base
#'
#' Matches each input name against the base `species` binomial (normalized) and
#' unions the `;`-separated `states` (and `biome`, flora only) across every
#' matching row — subspecies/variety rows carry partial distributions. Callers
#' should pass the *accepted* name (the cascade resolves synonyms first) so the
#' lookup reflects the taxon's true distribution. Returns one row per unique input
#' name; `states`/`biomes` are `NA` when the species is absent or the base has no
#' data for that dimension. Feeds `geo_crosscheck_distribution()` directly.
#'
#' @param provider_id Provider id (cache key).
#' @param names Character vector of species names.
#' @param data The embedded base (or NULL to load the cached base).
#' @param filename Base RDS basename, used to load when `data` is NULL.
#' @param has_biome Whether the base carries a `biome` column (florabr TRUE).
#' @return Data frame `query_name`/`states`/`biomes` (one row per unique name).
#' @noRd
br_distribution <- function(provider_id, names, data, filename, has_biome) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(data.frame(query_name = character(0), states = character(0),
                          biomes = character(0), stringsAsFactors = FALSE))
    }
    if (is.null(data)) {
        data <- br_load_data(provider_id, filename)
    }
    if (!is.data.frame(data) || nrow(data) == 0L || !"species" %in% names(data)) {
        return(data.frame(query_name = names_chr, states = NA_character_,
                          biomes = NA_character_, stringsAsFactors = FALSE))
    }

    spk <- br_species_key(provider_id, data)
    states_col <- if ("states" %in% names(data)) as.character(data$states) else rep(NA_character_, nrow(data))
    biome_col <- if (has_biome && "biome" %in% names(data)) as.character(data$biome) else rep(NA_character_, nrow(data))

    key_in <- normalize_for_matching(names_chr)
    agg <- function(vals) {
        toks <- split_distribution(vals)
        if (length(toks) == 0L) NA_character_ else paste(toks, collapse = ";")
    }
    states <- vapply(key_in, function(k) {
        hit <- which(spk == k)
        if (length(hit) == 0L) NA_character_ else agg(states_col[hit])
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    biomes <- if (!has_biome) {
        rep(NA_character_, length(names_chr))
    } else {
        vapply(key_in, function(k) {
            hit <- which(spk == k)
            if (length(hit) == 0L) NA_character_ else agg(biome_col[hit])
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    }

    data.frame(query_name = names_chr, states = states, biomes = biomes,
               stringsAsFactors = FALSE)
}

#' Map a florabr/faunabr check result to the canonical schema
#'
#' Base-specific mapping (Spelling + taxonomicStatus -> validation_status,
#' "Suggested name" -> scientificName), deduplicating multiple suggestion rows
#' per input by smallest `Distance`. Any canonical taxonomy columns present in
#' the raw output (kingdom..genus, taxonID, taxonRank, vernacularName) pass
#' through. The schema is finalized by `normalize_provider_result()`.
#'
#' @param raw_df Raw check result.
#' @param provider_id Provider id.
#' @return Canonical-schema data frame.
#' @noRd
#' Recover the full taxonomy for accepted names from the embedded base
#'
#' `check_names()`/`check_fauna_names()` answer with a *match verdict*
#' (`Suggested_name`, `taxonomicStatus`, `family`) — they do not return the
#' higher taxonomy. The embedded base does carry it, so join it back by the
#' accepted name (the base's clean binomial `species` column). Fills blanks only,
#' so the verdict's own columns (e.g. `family`) win. (ADR-016 covers the columns
#' derivable from the name; these are the ones that are not.)
#'
#' @param out Canonical-ish data frame with a `scientificName` column.
#' @param data The embedded base, or NULL to skip.
#' @param default_kingdom Kingdom to use when the base has no `kingdom` column
#'   (faunabr is fauna-only, so `"Animalia"`).
#' @return `out` with taxonomy columns filled where the base had them.
#' @noRd
br_join_base_taxonomy <- function(out, data, default_kingdom = NA_character_) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
        !"species" %in% names(data)) {
        return(out)
    }
    idx <- match(normalize_for_matching(out$scientificName),
                 normalize_for_matching(data$species))

    # base column -> canonical column
    from_base <- c(taxonID = "id", kingdom = "kingdom", phylum = "phylum",
                   class = "class", order = "order", family = "family",
                   genus = "genus", taxonRank = "taxonRank",
                   vernacularName = "vernacularName")
    for (canonical in names(from_base)) {
        src <- from_base[[canonical]]
        if (!src %in% names(data)) {
            next
        }
        if (!canonical %in% names(out)) {
            out[[canonical]] <- NA_character_
        }
        out[[canonical]] <- fill_blank_values(out[[canonical]],
                                              as.character(data[[src]])[idx])
    }

    # faunabr ships no `kingdom` column — it is fauna by construction.
    if (!"kingdom" %in% names(data) && !is.na(default_kingdom)) {
        if (!"kingdom" %in% names(out)) {
            out$kingdom <- NA_character_
        }
        matched <- !is.na(idx)
        fallback <- rep(NA_character_, nrow(out))
        fallback[matched] <- default_kingdom
        out$kingdom <- fill_blank_values(out$kingdom, fallback)
    }
    out
}

br_map_check_result <- function(raw_df, provider_id, data = NULL,
                                default_kingdom = NA_character_) {
    if (!is.data.frame(raw_df) || nrow(raw_df) == 0L) {
        return(empty_canonical_result())
    }
    names(raw_df) <- trimws(names(raw_df))

    # Keep the closest suggestion per input_name.
    if ("Distance" %in% names(raw_df) && "input_name" %in% names(raw_df)) {
        dist_num <- suppressWarnings(as.numeric(raw_df[["Distance"]]))
        dist_num[is.na(dist_num)] <- Inf
        raw_df[["Distance"]] <- dist_num
        split_list <- split(seq_len(nrow(raw_df)), raw_df[["input_name"]])
        keep_rows <- vapply(split_list, function(idxs) {
            if (length(idxs) == 1L) idxs[[1L]] else idxs[[which.min(raw_df[["Distance"]][idxs])]]
        }, FUN.VALUE = integer(1))
        raw_df <- raw_df[keep_rows, , drop = FALSE]
        rownames(raw_df) <- NULL
    }

    n <- nrow(raw_df)
    col <- function(name) if (name %in% names(raw_df)) as.character(raw_df[[name]]) else rep(NA_character_, n)

    query_name <- col("input_name")
    spelling <- col("Spelling")
    tax_status <- tolower(col("taxonomicStatus"))

    validation_status <- vapply(seq_len(n), function(i) {
        sp <- spelling[[i]]
        ts <- tax_status[[i]]
        if (is.na(sp)) return("not_found")
        if (identical(sp, "Correct")) {
            if (!is.na(ts) && grepl("synonym", ts, fixed = TRUE)) return("synonym")
            return("accepted")
        }
        if (identical(sp, "Probably_incorrect")) return("ambiguous")
        "not_found"
    }, FUN.VALUE = character(1))

    suggested <- col("Suggested name")
    scientific_name <- ifelse(is.na(suggested) | !nzchar(suggested), query_name, suggested)

    out <- data.frame(
        query_name = query_name,
        scientificName = scientific_name,
        taxonomicStatus = tax_status,
        validation_status = validation_status,
        stringsAsFactors = FALSE
    )
    # Pass through any canonical taxonomy columns the base already provides.
    for (extra in c("taxonID", "taxonRank", "acceptedNameUsageID", "kingdom",
                    "phylum", "class", "order", "family", "genus",
                    "specificEpithet", "infraspecificEpithet", "vernacularName")) {
        if (extra %in% names(raw_df)) {
            out[[extra]] <- as.character(raw_df[[extra]])
        }
    }

    out <- br_join_base_taxonomy(out, data, default_kingdom)

    normalize_provider_result(out, provider_id)
}
