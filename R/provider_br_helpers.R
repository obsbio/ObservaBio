# Title: Brazilian Provider Helpers
# Shared load + mapping for the embedded Flora BR / Fauna BR providers. Both
# florabr::check_names() and faunabr::check_fauna_names() return the same
# Spelling / Suggested_name / taxonomicStatus / family shape (Saira
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
    # One pass to group the base rows by species key, instead of re-scanning the
    # whole ~150k key vector once per name (and twice per name when the base has
    # biomes). `by_key` is a list of row indices, so the lookup is a hash hit.
    by_key <- split(seq_along(spk), spk)
    hits <- by_key[key_in]
    gather <- function(column) {
        vapply(hits, function(hit) {
            if (is.null(hit) || length(hit) == 0L) NA_character_ else agg(column[hit])
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    }
    states <- gather(states_col)
    biomes <- if (!has_biome) {
        rep(NA_character_, length(names_chr))
    } else {
        gather(biome_col)
    }

    data.frame(query_name = names_chr, states = states, biomes = biomes,
               stringsAsFactors = FALSE)
}

#' Count whitespace-separated tokens in a name (vectorized)
#'
#' Used to tell a spelling correction from a rank change: a suggestion with a
#' different token count than the input is a different taxon, not a typo fix.
#'
#' @param x Character vector of names.
#' @return Integer vector of token counts.
#' @noRd
count_name_tokens <- function(x) {
    lengths(strsplit(trimws(as.character(x)), "\\s+"))
}

#' Map a florabr/faunabr check result to the canonical schema
#'
#' Base-specific mapping (Spelling + taxonomicStatus -> validation_status,
#' `Suggested_name` -> scientificName), deduplicating multiple suggestion rows
#' per input by smallest `Distance` and, on a tie, preferring the accepted row
#' over a synonym one. Any canonical taxonomy columns present in
#' the raw output (kingdom..genus, taxonID, taxonRank, vernacularName) pass
#' through, except on a refused suggestion (another taxon, so `not_found`).
#' The schema is finalized by `normalize_provider_result()`.
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
#' @param provider_id Provider id, so the normalized species key can come from the
#'   [br_species_key()] cache instead of being recomputed. `NULL` computes it
#'   inline, for callers passing a base that is not the cached one.
#' @return `out` with taxonomy columns filled where the base had them.
#' @noRd
br_join_base_taxonomy <- function(out, data, default_kingdom = NA_character_,
                                  provider_id = NULL) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
        !"species" %in% names(data)) {
        return(out)
    }
    # Normalizing all ~150k `species` values costs ~0.4 s and happens on every
    # query; br_species_key() already caches exactly this vector per provider.
    base_key <- if (is.null(provider_id)) {
        normalize_for_matching(data$species)
    } else {
        br_species_key(provider_id, data)
    }
    # match() returns the first row with the key, and Fauna do Brasil files each
    # subspecies under its binomial (`Iguana iguana iguana` has `species` =
    # `Iguana iguana`). When that row came first, the species got the subspecies
    # id and rank, and lost its vernacular name. So put the species-rank,
    # non-synonym row of each key first.
    base_chr <- function(name) {
        if (!name %in% names(data)) {
            return(rep(NA_character_, nrow(data)))
        }
        as.character(data[[name]])
    }
    ord <- order(!(tolower(base_chr("taxonRank")) %in% "species"),
                 grepl("synonym", tolower(base_chr("taxonomicStatus")), fixed = TRUE))
    idx <- ord[match(normalize_for_matching(out$scientificName), base_key[ord])]

    # base column -> canonical column
    from_base <- c(taxonID = "id", kingdom = "kingdom", phylum = "phylum",
                   class = "class", order = "order", family = "family",
                   genus = "genus", taxonRank = "taxonRank",
                   vernacularName = "vernacularName")
    values <- lapply(from_base, function(src) {
        if (src %in% names(data)) as.character(data[[src]])[idx] else NULL
    })
    # Taxon attributes the base carries in its own vocabulary (ADR-029).
    values$scientificNameAuthorship <- br_authorship(base_chr("scientificName")[idx],
                                                     base_chr("species")[idx])
    values$establishmentMeans <- br_establishment_means(base_chr("origin")[idx])
    values$taxonRemarks <- br_endemism_remark(base_chr("endemism")[idx])
    for (canonical in names(values)) {
        if (is.null(values[[canonical]])) {
            next
        }
        if (!canonical %in% names(out)) {
            out[[canonical]] <- NA_character_
        }
        out[[canonical]] <- fill_blank_values(out[[canonical]], values[[canonical]])
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

#' Authorship of a base row: its `scientificName` without the `species` part
#'
#' Both BR bases write the author after the name (`Annona crassiflora Mart.`,
#' `Iguana iguana (Linnaeus, 1758)`). A row whose name does not start with its
#' `species` value (a flora autonym such as `Cissus erosa Rich. subsp. erosa`)
#' gets NA.
#'
#' @param scientific_name,species Character vectors of the same length.
#' @return Character vector of authorship strings, NA where there is none.
#' @noRd
br_authorship <- function(scientific_name, species) {
    sn <- as.character(scientific_name)
    sp <- as.character(species)
    ok <- !is.na(sn) & !is.na(sp) & startsWith(sn, paste0(sp, " "))
    out <- rep(NA_character_, length(sn))
    out[ok] <- trimws(substring(sn[ok], nchar(sp[ok]) + 2L))
    out[!is_non_empty(out)] <- NA_character_
    out
}

#' DwC `establishmentMeans` from the `origin` of a BR base
#'
#' Flora do Brasil writes Native/Naturalized/Cultivated and Fauna do Brasil
#' writes native/introduced/uncertain. A naturalized or cultivated plant came to
#' Brazil through people, which DwC calls "introduced". An unknown origin stays
#' NA.
#'
#' @param origin Character vector of base `origin` values.
#' @return Character vector of DwC `establishmentMeans` values.
#' @noRd
br_establishment_means <- function(origin) {
    dwc <- c(native = "native", naturalized = "introduced",
             cultivated = "introduced", introduced = "introduced",
             uncertain = "uncertain")
    unname(dwc[tolower(as.character(origin))])
}

#' `taxonRemarks` from the Flora do Brasil `endemism`
#'
#' Writes the `endemism=<value>` form that the client's reference sheets use.
#' An unknown endemism stays NA.
#'
#' @param endemism Character vector of base `endemism` values.
#' @return Character vector of `taxonRemarks` values.
#' @noRd
br_endemism_remark <- function(endemism) {
    e <- as.character(endemism)
    ifelse(e %in% c("Endemic", "Non-endemic"), paste0("endemism=", e), NA_character_)
}

br_map_check_result <- function(raw_df, provider_id, data = NULL,
                                default_kingdom = NA_character_) {
    if (!is.data.frame(raw_df) || nrow(raw_df) == 0L) {
        return(empty_canonical_result())
    }
    names(raw_df) <- trimws(names(raw_df))

    # Keep the closest suggestion per input_name, accepted row first on a tie.
    if ("Distance" %in% names(raw_df) && "input_name" %in% names(raw_df)) {
        dist_num <- suppressWarnings(as.numeric(raw_df[["Distance"]]))
        dist_num[is.na(dist_num)] <- Inf
        raw_df[["Distance"]] <- dist_num
        # A binomial published by two authors has two base rows — one Accepted, one
        # Synonym (`Euterpe oleracea Mart.` vs `Euterpe oleracea Engel`) — and
        # check_names() returns both at Distance 0. Ranking on Distance alone left
        # the winner to whatever order the merge inside check_names produced, which
        # shifts with the size of the batch: the same name came back `accepted` in
        # one run and `synonym` in another. Break the tie on the status instead.
        tax_raw <- if ("taxonomicStatus" %in% names(raw_df)) {
            tolower(as.character(raw_df[["taxonomicStatus"]]))
        } else {
            rep(NA_character_, nrow(raw_df))
        }
        is_synonym <- !is.na(tax_raw) & grepl("synonym", tax_raw, fixed = TRUE)
        split_list <- split(seq_len(nrow(raw_df)), raw_df[["input_name"]])
        keep_rows <- vapply(split_list, function(idxs) {
            if (length(idxs) == 1L) {
                return(idxs[[1L]])
            }
            idxs[order(raw_df[["Distance"]][idxs], is_synonym[idxs])][[1L]]
        }, FUN.VALUE = integer(1))
        raw_df <- raw_df[keep_rows, , drop = FALSE]
        rownames(raw_df) <- NULL
    }

    n <- nrow(raw_df)
    col <- function(name) if (name %in% names(raw_df)) as.character(raw_df[[name]]) else rep(NA_character_, n)

    query_name <- col("input_name")
    spelling <- col("Spelling")
    tax_status <- tolower(col("taxonomicStatus"))
    # Fauna do Brasil writes "valid" where Darwin Core and Flora do Brasil write
    # "accepted".
    tax_status[tax_status %in% "valid"] <- "accepted"

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

    # Both bases spell this column `Suggested_name`. Reading it as "Suggested name"
    # silently yielded all-NA, so every fuzzy hit fell back to the *misspelled*
    # input: `Cedrela fissillis` stayed misspelled even though check_names had
    # answered `Cedrela fissilis` at Distance 1.
    suggested <- col("Suggested_name")
    # ...but only accept a suggestion that is a *spelling* correction. agrep() is
    # happy to answer a genus with a binomial, and those answers are not near
    # misses, they are different taxa: `Psidium` (guava) -> `Pisidium vile` (a
    # clam), `Mycena` (a fungus) -> `Mycetarotes acutus` (an ant). A correction
    # that changes the rank of the name is not a correction, so require the token
    # count to match — `Amona crassiflora` -> `Annona crassiflora` still passes.
    usable <- !is.na(suggested) & nzchar(suggested) &
        count_name_tokens(suggested) == count_name_tokens(query_name)
    scientific_name <- ifelse(usable, suggested, query_name)
    # The rest of a refused verdict row (status, family) describes the refused
    # taxon, not the input. Kept, it gave `Psidium` (a plant) the clam family
    # Sphaeriidae. The base has no answer for the input, so say not_found.
    validation_status[!usable] <- "not_found"
    tax_status[!usable] <- NA_character_

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
            vals <- as.character(raw_df[[extra]])
            vals[!usable] <- NA_character_
            out[[extra]] <- vals
        }
    }

    out <- br_join_base_taxonomy(out, data, default_kingdom, provider_id = provider_id)

    normalize_provider_result(out, provider_id)
}
