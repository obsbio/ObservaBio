# Title: Geographic Verification Orchestration (SPEC §8, week 7 runtime)
# Pure composition of the finished geo layers (utils_geo.R + utils_gbif_occ.R +
# the provider distribution() accessors) into a single verification pass the
# process module calls behind its progress bar. No Shiny here. The GBIF query and
# the base distribution lookup run on the *accepted* scientificName (the cascade
# resolves synonyms first); the resulting per-species flags are re-keyed to the
# normalized query_name so they join records in build_dwc_output() and the audit.

#' Union the UF/biome distribution of a set of names across all providers
#'
#' Each provider that carries a `distribution()` accessor (florabr/faunabr;
#' GBIF has none) returns `query_name`/`states`/`biomes`. A given taxon matches
#' at most one base, so the union is just "take whatever any base knows". Tokens
#' are merged and re-joined with `;`.
#'
#' @param names Character vector of (accepted) scientific names.
#' @param providers List of `ObservaBio_provider` objects (default: the registry).
#' @return Data frame `query_name`/`states`/`biomes`, one row per unique name.
#' @noRd
providers_distribution <- function(names, providers = get_providers()) {
    names_u <- unique(as.character(names))
    names_u <- names_u[!is.na(names_u) & nzchar(names_u)]
    if (length(names_u) == 0L) {
        return(data.frame(query_name = character(0), states = character(0),
                          biomes = character(0), stringsAsFactors = FALSE))
    }
    states_tok <- stats::setNames(vector("list", length(names_u)), names_u)
    biomes_tok <- stats::setNames(vector("list", length(names_u)), names_u)

    for (p in providers) {
        if (is.null(p$distribution) || !is.function(p$distribution)) {
            next
        }
        d <- tryCatch(p$distribution(names_u), error = function(e) NULL)
        if (is.null(d) || !is.data.frame(d) || nrow(d) == 0L) {
            next
        }
        for (i in seq_len(nrow(d))) {
            nm <- as.character(d$query_name[[i]])
            if (!nm %in% names_u) next
            states_tok[[nm]] <- c(states_tok[[nm]], split_distribution(d$states[[i]]))
            biomes_tok[[nm]] <- c(biomes_tok[[nm]], split_distribution(d$biomes[[i]]))
        }
    }

    collapse <- function(toks) {
        toks <- unique(toks)
        if (length(toks) == 0L) NA_character_ else paste(toks, collapse = ";")
    }
    data.frame(
        query_name = names_u,
        states = vapply(names_u, function(nm) collapse(states_tok[[nm]]),
                        FUN.VALUE = character(1), USE.NAMES = FALSE),
        biomes = vapply(names_u, function(nm) collapse(biomes_tok[[nm]]),
                        FUN.VALUE = character(1), USE.NAMES = FALSE),
        stringsAsFactors = FALSE
    )
}

#' Resolve the operation-area geometry from the upload's shapefile slot
#'
#' Accepts an `sf`/`sfc` (already read — used in tests), the `unzip_shapefile()`
#' list (uses its `dir`), or a directory / `.shp` path.
#' @noRd
geo_resolve_area <- function(shapefile) {
    if (inherits(shapefile, "sf") || inherits(shapefile, "sfc")) {
        return(shapefile)
    }
    path <- NULL
    if (is.list(shapefile) && !is.null(shapefile$dir)) {
        path <- shapefile$dir
    } else if (is.character(shapefile) && length(shapefile) == 1L) {
        path <- shapefile
    }
    if (is.null(path)) {
        return(NULL)
    }
    geo_read_shapefile(path)
}

#' Resolve the upload's study areas into a named, read list (pure)
#'
#' One `.zip` is one area. Accepts either the multi-area upload slot (a list of
#' `name`/`dir`/`localities` entries) or a single bare area — an `sf`/`sfc`, a
#' path, or one `unzip_shapefile()` list — which becomes a one-element list. An
#' area whose geometry cannot be read is skipped rather than sinking the run.
#'
#' @param areas Upload `areas` slot, or a single area in any accepted form.
#' @return List of `list(name, localities, geom)`, or `NULL` when nothing
#'   resolves (the caller then skips the geographic verification entirely).
#' @noRd
geo_resolve_areas <- function(areas) {
    if (is.null(areas)) {
        return(NULL)
    }
    # A single bare area vs. a list of area entries: only the former is itself an
    # sf/path/unzip list, and only the latter has no `dir`/`geom` of its own.
    is_single <- inherits(areas, "sf") || inherits(areas, "sfc") ||
        is.character(areas) ||
        (is.list(areas) && (!is.null(areas$dir) || !is.null(areas$geom)))
    entries <- if (is_single) list(areas) else areas
    if (!is.list(entries) || length(entries) == 0L) {
        return(NULL)
    }

    out <- list()
    for (i in seq_along(entries)) {
        entry <- entries[[i]]
        source <- if (is.list(entry) && !is.null(entry$geom)) entry$geom else entry
        geom <- geo_resolve_area(source)
        if (is.null(geom)) {
            next
        }
        nm <- if (is.list(entry) && !is.null(entry$name)) {
            as.character(entry$name)[[1L]]
        } else {
            NA_character_
        }
        if (is.na(nm) || !nzchar(nm)) {
            nm <- if (length(entries) == 1L) "Área de estudo" else sprintf("Área %d", i)
        }
        out[[length(out) + 1L]] <- list(
            name = nm,
            localities = if (is.list(entry)) .area_localities(entry) else character(0),
            geom = geom
        )
    }
    if (length(out) == 0L) {
        return(NULL)
    }
    out
}

#' Build the (query_name, accepted scientificName) set to geo-verify
#'
#' The geographic question — "is this species recorded near the operation area?"
#' — is relevant for *every* species in the list, not only the ones the
#' taxonomic cascade reprocessed. So the geo set unions:
#'   - the cascade's names (new rows), taking the *accepted* name so synonyms are
#'     resolved before the GBIF/distribution lookup; and
#'   - every other species already in the sheet (already-validated rows), whose
#'     own scientificName is the accepted name.
#' Keyed by the normalized `query_name` so the resulting flags join records the
#' same way taxonomy does (`build_dwc_output` / `build_results_view`). The
#' cascade mapping wins on collision (it resolves synonyms); records only add the
#' species the cascade did not process.
#'
#' @param records Uploaded records (one row per record).
#' @param cascade Cascade result (query_name + scientificName), or NULL.
#' @param name_col Name column in `records`. Default "scientificName".
#' @return Data frame `query_name`/`scientificName`, one row per unique species.
#' @noRd
geo_name_map <- function(records, cascade, name_col = "scientificName") {
    empty <- data.frame(query_name = character(0), scientificName = character(0),
                        stringsAsFactors = FALSE)
    from_cascade <- empty
    if (is.data.frame(cascade) && nrow(cascade) > 0L &&
        all(c("query_name", "scientificName") %in% names(cascade))) {
        qn <- as.character(cascade$query_name)
        acc <- as.character(cascade$scientificName)
        blank <- is.na(acc) | !nzchar(acc)
        acc[blank] <- qn[blank]
        from_cascade <- data.frame(query_name = qn, scientificName = acc,
                                   stringsAsFactors = FALSE)
    }
    from_records <- empty
    if (is.data.frame(records) && nrow(records) > 0L && name_col %in% names(records)) {
        nm <- as.character(records[[name_col]])
        nm <- nm[!is.na(nm) & nzchar(trimws(nm))]
        if (length(nm) > 0L) {
            qn <- vapply(nm, normalize_scientific_name,
                         FUN.VALUE = character(1), USE.NAMES = FALSE)
            from_records <- data.frame(query_name = qn, scientificName = nm,
                                       stringsAsFactors = FALSE)
        }
    }
    add <- from_records[!from_records$query_name %in% from_cascade$query_name, , drop = FALSE]
    out <- rbind(from_cascade, add)
    out <- out[nzchar(out$query_name) & !is.na(out$query_name), , drop = FALSE]
    out <- out[!duplicated(out$query_name), , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Run the geographic verification for ONE study area (SPEC §8)
#'
#' Composes: 10 km buffer -> area UF/biome -> GBIF-in-buffer (by accepted name)
#' -> provider distribution cross-check -> `distributionFlag`. The area/buffer
#' come out even when there are no names to check (so the map can draw the
#' polygon). [run_geo_verification()] calls this once per uploaded area.
#'
#' @param area Already-resolved area geometry (`sf`/`sfc`).
#' @param cascade A `query_name` + `scientificName` frame: the species this area
#'   claims (from [geo_name_map_by_area()], or the whole list for one area).
#' @param providers Providers for the distribution cross-check.
#' @param uf_biomes `list(states, biomes)` layer.
#' @param buffer_m Buffer distance in metres.
#' @param fetch Per-species GBIF fetch seam (test hook).
#' @return List with `area`, `buffer`, `crs_metric`, `dist_m`, `occ`,
#'   `area_states`, `area_biomes`, `per_species`, `gbif_failed`.
#' @noRd
run_geo_verification_one <- function(area, cascade,
                                     providers = get_providers(),
                                     uf_biomes = NULL, buffer_m = 10000,
                                     fetch = gbif_occ_fetch_species) {
    buf <- geo_buffer(area, dist_m = buffer_m)
    if (is.null(uf_biomes)) {
        uf_biomes <- load_br_uf_biomes()
    }
    area_ufb <- geo_area_uf_biomes(buf$area, layers = uf_biomes)

    empty_ps <- data.frame(
        query_name = character(0), scientificName = character(0),
        has_gbif = logical(0), gbif_count = integer(0),
        state_match = logical(0), biome_match = logical(0),
        present = logical(0), distributionFlag = character(0),
        stringsAsFactors = FALSE
    )
    result <- list(
        area = buf$area, buffer = buf$buffer, crs_metric = buf$crs_metric,
        dist_m = buf$dist_m, occ = gbif_occ_parse(NULL),
        area_states = area_ufb$states, area_biomes = area_ufb$biomes,
        per_species = empty_ps, gbif_failed = character(0)
    )

    if (is.null(cascade) || !is.data.frame(cascade) || nrow(cascade) == 0L ||
        !all(c("query_name", "scientificName") %in% names(cascade))) {
        return(result)
    }

    query_name <- as.character(cascade$query_name)
    accepted <- as.character(cascade$scientificName)
    accepted[is.na(accepted) | !nzchar(accepted)] <- query_name[is.na(accepted) | !nzchar(accepted)]
    uacc <- unique(accepted[!is.na(accepted) & nzchar(accepted)])

    occ <- gbif_occ_in_buffer(uacc, buf$buffer, fetch = fetch)
    gbif_failed <- attr(occ, "failed") %||% character(0)           # names we could not check
    has_gbif <- gbif_occ_presence(occ, uacc)                       # named by accepted
    counts <- if (nrow(occ) > 0L) table(as.character(occ$species)) else integer(0)
    gbif_count <- stats::setNames(
        vapply(uacc, function(a) as.integer(if (a %in% names(counts)) counts[[a]] else 0L),
               integer(1)), uacc
    )

    dist <- providers_distribution(uacc, providers)                # query_name(=accepted)
    dist_idx <- match(uacc, dist$query_name)
    cross <- lapply(seq_along(uacc), function(i) {
        j <- dist_idx[[i]]
        sp_states <- if (!is.na(j)) dist$states[[j]] else NA
        sp_biomes <- if (!is.na(j)) dist$biomes[[j]] else NA
        geo_crosscheck_distribution(area_ufb$states, area_ufb$biomes,
                                    sp_states, sp_biomes)
    })
    present <- stats::setNames(vapply(cross, function(x) x$present, logical(1)), uacc)
    state_m <- stats::setNames(vapply(cross, function(x) x$state_match, logical(1)), uacc)
    biome_m <- stats::setNames(vapply(cross, function(x) x$biome_match, logical(1)), uacc)
    flag_acc <- classify_distribution_flag(has_gbif[uacc], present[uacc])  # named by accepted

    # Re-key everything to the per-cascade-row query_name.
    per_species <- data.frame(
        query_name = query_name,
        scientificName = accepted,
        has_gbif = unname(has_gbif[accepted]),
        gbif_count = unname(gbif_count[accepted]),
        state_match = unname(state_m[accepted]),
        biome_match = unname(biome_m[accepted]),
        present = unname(present[accepted]),
        distributionFlag = unname(flag_acc[accepted]),
        stringsAsFactors = FALSE
    )
    result$occ <- occ
    result$per_species <- per_species
    result$gbif_failed <- gbif_failed
    result
}

#' Combine the per-area verifications into one result (pure)
#'
#' The per-area frames (`occ`, `per_species`) gain an `area` column and are
#' stacked; the geometries are concatenated so the map can frame everything at
#' once; UF(s)/biome(s) and failed GBIF names are unioned. `distribution_flags`
#' is keyed by [area_flag_key()] — the join stopped being per-species the moment
#' one upload could carry several areas.
#'
#' @param per List of [run_geo_verification_one()] results, each carrying the
#'   `name` and `localities` of its area.
#' @return The combined geo list (see [run_geo_verification()]).
#' @noRd
geo_combine_areas <- function(per) {
    stack <- function(key) {
        frames <- lapply(per, function(p) {
            d <- p[[key]]
            if (!is.data.frame(d) || nrow(d) == 0L) {
                return(NULL)
            }
            d$area <- p$name
            d
        })
        frames <- Filter(Negate(is.null), frames)
        if (length(frames) == 0L) {
            empty <- per[[1L]][[key]]
            empty$area <- character(0)
            return(empty)
        }
        out <- do.call(rbind, frames)
        rownames(out) <- NULL
        out
    }
    union_sorted <- function(key) {
        v <- unlist(lapply(per, function(p) as.character(p[[key]])), use.names = FALSE)
        v <- v[!is.na(v) & nzchar(v)]
        sort(unique(v))
    }

    per_species <- stack("per_species")
    flags <- stats::setNames(
        as.character(per_species$distributionFlag),
        area_flag_key(per_species$area, per_species$query_name)
    )
    flags <- flags[!is.na(names(flags))]

    list(
        areas = per,
        area = do.call(c, lapply(per, function(p) p$area)),
        buffer = do.call(c, lapply(per, function(p) p$buffer)),
        crs_metric = per[[1L]]$crs_metric,
        dist_m = per[[1L]]$dist_m,
        occ = stack("occ"),
        area_states = union_sorted("area_states"),
        area_biomes = union_sorted("area_biomes"),
        per_species = per_species,
        distribution_flags = flags,
        gbif_failed = union_sorted("gbif_failed")
    )
}

#' Run the full geographic verification for one upload, over N areas (SPEC §8)
#'
#' Each uploaded `.zip` is one study area, verified independently against only
#' the species its own records claim (the `locality` link, see
#' [assign_record_areas()]). Returns `NULL` when no area resolves — the caller
#' then leaves `distributionFlag` empty and the map empty (pre-geo behaviour).
#'
#' @param areas The upload `areas` slot, or a single bare area (`sf`,
#'   `unzip_shapefile()` list, or a path), or `NULL`.
#' @param cascade The species to verify. With an `area` column (from
#'   [geo_name_map_by_area()]) each area gets its own slice; without one, the
#'   same set is verified against every area.
#' @param providers Providers for the distribution cross-check.
#' @param uf_biomes Optional `list(states, biomes)` layer (default: embedded).
#' @param buffer_m Buffer distance in metres.
#' @param fetch Per-species GBIF fetch seam (test hook).
#' @return List with `areas` (the per-area results), the combined `area`,
#'   `buffer`, `occ`, `per_species` (both with an `area` column), `area_states`,
#'   `area_biomes`, `distribution_flags` (keyed by [area_flag_key()]),
#'   `gbif_failed`; or `NULL`.
#' @noRd
run_geo_verification <- function(areas, cascade,
                                 providers = get_providers(),
                                 uf_biomes = NULL, buffer_m = 10000,
                                 fetch = gbif_occ_fetch_species) {
    resolved <- geo_resolve_areas(areas)
    if (is.null(resolved)) {
        return(NULL)
    }
    if (is.null(uf_biomes)) {
        uf_biomes <- load_br_uf_biomes()
    }
    by_area <- is.data.frame(cascade) && "area" %in% names(cascade)

    per <- lapply(resolved, function(a) {
        slice <- if (by_area) {
            cascade[as.character(cascade$area) == a$name, , drop = FALSE]
        } else {
            cascade
        }
        one <- run_geo_verification_one(
            a$geom, slice, providers = providers, uf_biomes = uf_biomes,
            buffer_m = buffer_m, fetch = fetch
        )
        one$name <- a$name
        one$localities <- a$localities
        one
    })
    geo_combine_areas(per)
}

#' CSS class for a distributionFlag value (badge styling; presentation-neutral)
#'
#' @param flag Character vector of distributionFlag values.
#' @return Character vector of class names (`flag-na` for empty/unknown).
#' @noRd
distribution_flag_class <- function(flag) {
    lv <- distribution_flag_levels()
    cls <- c(confirmed = "flag-confirmed", near_absent = "flag-present",
             outside = "flag-outside", no_data = "flag-nodata")
    key <- names(lv)[match(as.character(flag), lv)]
    out <- unname(cls[key])
    out[is.na(out)] <- "flag-na"
    out
}

#' PT-BR label for a cascade validation_status (results table / side panel)
#' @noRd
validation_status_label <- function(status) {
    map <- c(accepted = "aceito", synonym = "sinônimo",
             ambiguous = "ambíguo", not_found = "não encontrado")
    out <- unname(map[as.character(status)])
    out
}

#' Assemble the per-record results view for Step 3 (pure)
#'
#' Joins the standardized output (one row per record) with the cascade (validator
#' + status, by normalized name) and the geo per-species table (GBIF count), so
#' the DT and the side panel read from one frame. Rows that were already validated
#' on upload have no cascade/geo match, so `validator`/`matchType`/`gbif_count`
#' come back `NA` (the inherited "geo runs on new names only" limitation).
#'
#' @param dwc Standardized output from [build_dwc_output()].
#' @param cascade Cascade result (one row per processed name).
#' @param geo [run_geo_verification()] result, or `NULL`.
#' @param name_col Name column in `dwc`.
#' @param record_areas Study area per record ([assign_record_areas()]), or
#'   `NULL`. Drives the `area` column and the per-(species, area) GBIF count.
#' @return Data frame, one row per record.
#' @noRd
build_results_view <- function(dwc, cascade, geo = NULL,
                               name_col = "scientificName",
                               record_areas = NULL) {
    n <- if (is.data.frame(dwc)) nrow(dwc) else 0L
    col <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
    if (n == 0L) {
        return(data.frame(
            scientificName = character(0), family = character(0), genus = character(0),
            kingdom = character(0), taxonID = character(0), status = character(0),
            validator = character(0), matchType = character(0),
            distributionFlag = character(0), gbif_count = integer(0),
            invasive = logical(0), invasiveSource = character(0),
            area = character(0),
            stringsAsFactors = FALSE
        ))
    }
    keys <- vapply(dwc[[name_col]], normalize_scientific_name,
                   FUN.VALUE = character(1), USE.NAMES = FALSE)

    validator <- rep(NA_character_, n)
    match_type <- rep(NA_character_, n)
    if (is.data.frame(cascade) && nrow(cascade) > 0L && "query_name" %in% names(cascade)) {
        ci <- match(keys, as.character(cascade$query_name))
        validator <- as.character(cascade$provider)[ci]
        match_type <- validation_status_label(as.character(cascade$validation_status)[ci])
    }

    # The record's study area: the count (like the flag) is per (species, area).
    areas <- if (length(record_areas) == n) as.character(record_areas) else rep(NA_character_, n)

    gbif_count <- rep(NA_integer_, n)
    if (!is.null(geo) && is.data.frame(geo$per_species) && nrow(geo$per_species) > 0L) {
        ps <- geo$per_species
        gi <- if ("area" %in% names(ps) && !all(is.na(areas))) {
            match(area_flag_key(areas, keys), area_flag_key(ps$area, ps$query_name))
        } else {
            match(keys, as.character(ps$query_name))
        }
        gbif_count <- ps$gbif_count[gi]
    }

    # `invasive` stays logical (TRUE/NA): the badge asks a yes/no question.
    invasive <- if ("invasive" %in% names(dwc)) as.logical(dwc$invasive) else rep(NA, n)

    data.frame(
        scientificName = col(dwc, name_col),
        family = col(dwc, "family"), genus = col(dwc, "genus"),
        kingdom = col(dwc, "kingdom"), taxonID = col(dwc, "taxonID"),
        status = col(dwc, "status"),
        validator = validator, matchType = match_type,
        distributionFlag = col(dwc, "distributionFlag"),
        gbif_count = gbif_count,
        invasive = invasive, invasiveSource = col(dwc, "invasiveSource"),
        area = areas,
        stringsAsFactors = FALSE
    )
}

#' Count records/species per distributionFlag level (SPEC §8)
#'
#' @param flags Character vector of distributionFlag values.
#' @return Named integer vector over the four verbatim levels (0 where absent).
#' @noRd
geo_flag_counts <- function(flags) {
    lv <- distribution_flag_levels()
    flags <- as.character(flags)
    stats::setNames(
        vapply(lv, function(level) sum(flags == level, na.rm = TRUE), integer(1)),
        names(lv)
    )
}

#' Summary numbers for the Step-3 results tiles (pure)
#'
#' @param dwc Standardized output (one row per record).
#' @param cascade Cascade result (one row per processed name).
#' @param geo Result of [run_geo_verification()] or `NULL`.
#' @return List: `records`, `species`, `resolved_pct`, `unresolved`,
#'   `flag_counts` (named over the four levels), `alerts` (outside-range count,
#'   `NA` when no geo), `invasive` (distinct species on an invasive list).
#' @noRd
process_summary <- function(dwc, cascade, geo = NULL) {
    n_records <- if (is.data.frame(dwc)) nrow(dwc) else 0L
    n_species <- if (is.data.frame(cascade)) nrow(cascade) else 0L
    resolved <- if (is.data.frame(cascade) && "validation_status" %in% names(cascade)) {
        sum(cascade$validation_status %in% c("accepted", "synonym"))
    } else 0L
    unresolved <- if (n_species > 0L) n_species - resolved else 0L
    pct <- if (n_species > 0L) round(100 * resolved / n_species) else 0L

    flag_counts <- geo_flag_counts(character(0))
    alerts <- NA_integer_
    if (!is.null(geo) && is.data.frame(geo$per_species) && nrow(geo$per_species) > 0L) {
        flag_counts <- geo_flag_counts(geo$per_species$distributionFlag)
        alerts <- unname(flag_counts[["outside"]])
    }

    # Counted off `dwc` (not the flags vector) so the tile stays right for every
    # caller, and as DISTINCT species — like `alerts`, and unlike `records`.
    n_invasive <- 0L
    if (is.data.frame(dwc) && all(c("invasive", "scientificName") %in% names(dwc))) {
        listed <- which(!is.na(as.logical(dwc$invasive)))
        n_invasive <- length(unique(normalize_for_matching(dwc$scientificName[listed])))
    }

    list(records = n_records, species = n_species, resolved_pct = pct,
         unresolved = unresolved, flag_counts = flag_counts, alerts = alerts,
         invasive = n_invasive)
}
