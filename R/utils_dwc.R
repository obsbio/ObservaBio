# Title: Darwin Core Output + Audit Mapping
# Turns a taxonomic cascade result (canonical schema, one row per unique
# normalized name; see provider_contract.R) plus the uploaded records into the
# two ObservaBio deliverables (SPEC §9):
#   1. standardized DwC sheet — one row per record, the input column model
#      preserved, taxonomy filled for newly resolved names.
#   2. audit table — one decision per name, plus its `nao_resolvidos` subset.
# Pure functions with no Shiny coupling; utils_export.R writes them to .xlsx.
#
# Scope note (SPEC §4/§16.2): the canonical schema carries no
# `scientificNameAuthorship`, so this layer fills only the canonical taxonomy
# columns. Non-canonical model columns (e.g. authorship) are preserved from the
# upload but never synthesized here. The Saira DwC-mapping module and its term
# catalog are out of scope (SPEC §3), so no `dwc_terms.rds` is used.

# ---------------------------------------------------------------------------
# Standardized DwC output (one row per record)
# ---------------------------------------------------------------------------

#' Canonical taxonomy columns copied from a resolved name onto its records
#' @noRd
dwc_taxonomy_columns <- function() {
    c("scientificName", "taxonID", "taxonRank", "taxonomicStatus",
      "acceptedNameUsageID", "kingdom", "phylum", "class", "order",
      "family", "genus", "specificEpithet", "infraspecificEpithet",
      "vernacularName")
}

#' Add any missing columns as empty character columns (like the script's ensure_cols)
#' @noRd
ensure_columns <- function(df, cols) {
    for (col in setdiff(cols, names(df))) {
        df[[col]] <- NA_character_
    }
    df
}

#' Map cascade status columns onto the ObservaBio output model columns
#'
#' `resolve_threat_status()` produces `statusMMA`/`statusSourceMMA`/
#' `iucnCategory`/`iucnCriteria`; the real ObservaBio model expects
#' `status`/`statusSource`/`statusIUCN`/`criteria`.
#' @noRd
dwc_status_columns <- function() {
    c(status = "statusMMA", statusSource = "statusSourceMMA",
      statusIUCN = "iucnCategory", criteria = "iucnCriteria")
}

#' Format an MMA category code into the ObservaBio `status` label
#'
#' Mirrors the reference `format_status_label` so the standardized `status`
#' column reads e.g. "(VU) Vulneravel". Unknown or blank codes yield NA.
#'
#' @param x Character vector of category codes (e.g. "VU", "CR (PEX)").
#' @return Character vector of formatted labels.
#' @noRd
format_mma_status_label <- function(x) {
    labels <- c(
        "EX" = "(EX) Extinta",
        "EW" = "(EW) Extinta na Natureza",
        "CR (PEX)" = "(CR (PEX)) Criticamente Em Perigo (Possivelmente Extinta)",
        "CR" = "(CR) Criticamente Em Perigo",
        "EN" = "(EN) Em Perigo",
        "VU" = "(VU) Vulneravel",
        "NT" = "(NT) Quase Ameacada",
        "LC" = "(LC) Pouco Preocupante"
    )
    code <- toupper(trimws(as.character(x)))
    code <- gsub("\\s+", " ", code)
    code[code == "CR(PEX)"] <- "CR (PEX)"
    unname(labels[code])
}

#' Rows already carrying taxonomy, preserved as-is (script 260626 logic)
#'
#' A record counts as already validated when `taxonID` or `kingdom` is filled.
#'
#' @param records Data frame of uploaded records.
#' @return Logical vector, one per row.
#' @noRd
dwc_prefilled_mask <- function(records) {
    n <- nrow(records)
    has_tax <- function(col) {
        if (col %in% names(records)) is_non_empty(records[[col]]) else rep(FALSE, n)
    }
    has_tax("taxonID") | has_tax("kingdom")
}

#' Scientific names of the records that still need cascade processing
#'
#' The complement of [dwc_prefilled_mask()]: names on rows that are *not* already
#' validated. Already-validated rows are preserved verbatim by [build_dwc_output()]
#' and carry their taxonomy/status in the input model, so only these names need to
#' reach the taxonomic cascade and the HTTP-per-species IUCN resolver — re-querying
#' the validated rows is wasted work.
#'
#' @param records Data frame of uploaded records.
#' @param name_col Column holding the name to validate. Default "scientificName".
#' @return Character vector of names from non-prefilled rows, order preserved
#'   (empty when there is nothing to process).
#' @noRd
dwc_unvalidated_names <- function(records, name_col = "scientificName") {
    if (!is.data.frame(records) || nrow(records) == 0L ||
        !name_col %in% names(records)) {
        return(character(0))
    }
    as.character(records[[name_col]])[!dwc_prefilled_mask(records)]
}

#' Attach resolved taxonomy to uploaded records (one row per record)
#'
#' Fills the canonical taxonomy columns for records that are *not* already
#' validated, joining the cascade result by normalized scientific name. Already
#' validated rows are preserved untouched. The input column model is preserved
#' in order; a `distributionFlag` slot is appended and, when `distribution_flags`
#' is supplied, filled per species from the geo verification (SPEC §8 step 5).
#'
#' @param records Data frame of uploaded records (DwC columns).
#' @param cascade Canonical-schema cascade result (one row per unique name).
#' @param model_cols Output column order. Defaults to the record columns.
#' @param name_col Column holding the name to validate. Default "scientificName".
#' @param add_distribution_flag Append a `distributionFlag` column.
#' @param distribution_flags Optional named character vector of distributionFlag
#'   values from the geo verification. Keyed by `normalize_scientific_name()` of
#'   the species, or — when `record_areas` is supplied — by [area_flag_key()],
#'   since one upload can carry several study areas. `NULL` (default, e.g. no
#'   shapefile uploaded) leaves the slot empty.
#' @param invasive_sources Optional named character vector from
#'   [resolve_invasive_flags()] (`invasiveSource` keyed by normalized name; only
#'   listed species are present). When supplied, the `invasive`/`invasiveSource`
#'   columns are appended and spread onto every matching record. `NULL` (default)
#'   omits the columns entirely — the cross-check did not run.
#' @param record_areas Optional study area per record ([assign_record_areas()]),
#'   `nrow(records)` long. Switches the `distribution_flags` join to the
#'   per-(species, area) key; records with no area keep an empty flag.
#' @return Data frame of records with taxonomy filled, in `model_cols` order.
#' @noRd
build_dwc_output <- function(records, cascade, model_cols = names(records),
                             name_col = "scientificName",
                             add_distribution_flag = TRUE,
                             distribution_flags = NULL,
                             invasive_sources = NULL,
                             record_areas = NULL) {
    if (!is.data.frame(records)) {
        stop("build_dwc_output(): records must be a data frame.")
    }
    records <- as.data.frame(records, stringsAsFactors = FALSE, check.names = FALSE)
    n <- nrow(records)
    records <- ensure_columns(records, unique(c(model_cols, name_col)))

    prefilled <- dwc_prefilled_mask(records)

    # Normalized name per row, reused to join both resolved taxonomy and the geo
    # flag back onto records.
    keys <- if (name_col %in% names(records) && n > 0L) {
        vapply(records[[name_col]], normalize_scientific_name,
               FUN.VALUE = character(1), USE.NAMES = FALSE)
    } else {
        character(0)
    }

    has_cascade <- !is.null(cascade) && is.data.frame(cascade) &&
        nrow(cascade) > 0L && "query_name" %in% names(cascade)
    if (has_cascade && length(keys) > 0L) {
        idx <- match(keys, as.character(cascade$query_name))
        fill_rows <- which(!prefilled & !is.na(idx))
        if (length(fill_rows) > 0L) {
            for (col in dwc_taxonomy_columns()) {
                if (!col %in% names(cascade)) {
                    next
                }
                records <- ensure_columns(records, col)
                records[[col]][fill_rows] <- as.character(cascade[[col]][idx[fill_rows]])
            }
            # Conservation status, only when resolve_threat_status() enriched the
            # cascade and the model actually has the target column (do not invent
            # status columns that the uploaded model does not carry).
            status_map <- dwc_status_columns()
            for (model_col in names(status_map)) {
                src <- status_map[[model_col]]
                if (!src %in% names(cascade) || !model_col %in% names(records)) {
                    next
                }
                vals <- as.character(cascade[[src]][idx[fill_rows]])
                if (identical(model_col, "status")) {
                    vals <- format_mma_status_label(vals)
                }
                records[[model_col]][fill_rows] <- vals
            }
        }
    }

    if (isTRUE(add_distribution_flag) && !"distributionFlag" %in% names(records)) {
        records$distributionFlag <- NA_character_
    }

    # Spread the geo flag onto every matching record. An upload can carry several
    # study areas, each linked to its own `locality` values, so the flag is a
    # per-(species, area) answer: with `record_areas` the join key is
    # area_flag_key(), and the same species in two areas can land on two
    # different flags. Without it (one bare area, or no locality link) the key is
    # the normalized name alone. Unlike the taxonomy fill above, this is NOT
    # gated on !prefilled: the geographic check runs for the whole list, so
    # already-validated rows carry a distributionFlag too. Records with no area
    # (`NA` key) are left empty — the client rule is to verify only what is
    # linked.
    if (!is.null(distribution_flags) && length(keys) > 0L &&
        "distributionFlag" %in% names(records)) {
        row_keys <- if (length(record_areas) == n) {
            area_flag_key(record_areas, keys)
        } else {
            keys
        }
        flag_idx <- match(row_keys, names(distribution_flags))
        flag_rows <- which(!is.na(flag_idx))
        if (length(flag_rows) > 0L) {
            records$distributionFlag[flag_rows] <-
                as.character(distribution_flags[flag_idx[flag_rows]])
        }
    }

    # Invasive-species cross-check, spread like the geo flag (per species, onto
    # every record) and for the same reason: the check runs on the whole list, so
    # already-validated rows carry it too. Membership in `invasive_sources` IS the
    # signal, so `invasive` is TRUE/NA — never FALSE — mirroring `statusMMA`.
    if (!is.null(invasive_sources)) {
        records$invasive <- NA
        records$invasiveSource <- NA_character_
        if (length(keys) > 0L && length(invasive_sources) > 0L) {
            inv_idx <- match(keys, names(invasive_sources))
            inv_rows <- which(!is.na(inv_idx))
            if (length(inv_rows) > 0L) {
                records$invasive[inv_rows] <- TRUE
                records$invasiveSource[inv_rows] <-
                    as.character(invasive_sources[inv_idx[inv_rows]])
            }
        }
    }

    out_cols <- unique(c(model_cols,
                         if (isTRUE(add_distribution_flag)) "distributionFlag",
                         if (!is.null(invasive_sources)) invasive_columns()))
    out_cols <- out_cols[out_cols %in% names(records)]
    out <- records[, out_cols, drop = FALSE]
    rownames(out) <- NULL
    out
}

# ---------------------------------------------------------------------------
# Audit table (one decision per name)
# ---------------------------------------------------------------------------

#' Audit column order (SPEC §9)
#' @noRd
audit_columns <- function() {
    c("originalName", "queryName", "validator", "matchType",
      "finalScientificName", "decisionReason", "taxonRank", "taxonomicStatus",
      "taxonID", "kingdom", "phylum", "class", "order", "family", "genus",
      "specificEpithet", "infraspecificEpithet")
}

#' Geo columns appended to the audit when a geo verification ran (SPEC §9)
#' @noRd
audit_geo_columns <- function() {
    c("distributionFlag", "gbifRecords", "areaName", "areaStates", "areaBiomes")
}

#' Invasive cross-check columns, appended to both deliverables
#' @noRd
invasive_columns <- function() {
    c("invasive", "invasiveSource")
}

#' Append the invasive cross-check columns to an audit table
#'
#' Joins [resolve_invasive_flags()] by `queryName` — the key it is named on. A
#' `NULL` (the cross-check did not run) leaves the audit unchanged.
#'
#' @param audit Base audit data frame (has `queryName`).
#' @param invasive_sources Named character vector, or `NULL`.
#' @return `audit`, with the two invasive columns appended when non-NULL.
#' @noRd
audit_attach_invasive <- function(audit, invasive_sources) {
    if (is.null(invasive_sources)) {
        return(audit)
    }
    audit$invasive <- NA
    audit$invasiveSource <- NA_character_
    if (nrow(audit) > 0L && length(invasive_sources) > 0L) {
        idx <- match(as.character(audit$queryName), names(invasive_sources))
        hits <- which(!is.na(idx))
        audit$invasive[hits] <- TRUE
        audit$invasiveSource[hits] <- as.character(invasive_sources[idx[hits]])
    }
    audit
}

#' Append the geo verification columns to an audit table
#'
#' The audit stays **one row per name** (SPEC §9) — the taxonomic decision it
#' records is not per-area — so a name verified in several study areas gets its
#' flags collapsed as `"Área A: confirmada; Área B: sem registro…"`, its
#' `gbifRecords` summed, and `areaName`/`areaStates`/`areaBiomes` listing only
#' the areas that name actually occurs in. The per-record DwC output carries the
#' exact per-area flag, so nothing is lost. A `NULL` geo (no shapefile) leaves
#' the audit unchanged.
#'
#' @param audit Base audit data frame (has `queryName`).
#' @param geo `run_geo_verification()` result, or `NULL`.
#' @return `audit`, with the geo columns appended when `geo` is non-NULL.
#' @noRd
audit_attach_geo <- function(audit, geo) {
    if (is.null(geo)) {
        return(audit)
    }
    collapse_or_na <- function(x) {
        x <- as.character(x)
        x <- x[!is.na(x) & nzchar(x)]
        if (length(x) == 0L) NA_character_ else paste(unique(x), collapse = "; ")
    }

    areas <- geo$areas %||% list()
    area_names <- vapply(areas, function(a) as.character(a$name), character(1))
    area_ufb <- function(names_used, key) {
        collapse_or_na(unlist(
            lapply(areas[match(names_used, area_names)], function(a) a[[key]]),
            use.names = FALSE
        ))
    }

    n <- nrow(audit)
    audit$distributionFlag <- NA_character_
    audit$gbifRecords <- NA_integer_
    audit$areaName <- NA_character_
    audit$areaStates <- NA_character_
    audit$areaBiomes <- NA_character_

    ps <- geo$per_species
    if (is.data.frame(ps) && nrow(ps) > 0L && n > 0L) {
        has_area <- "area" %in% names(ps)
        by_name <- split(seq_len(nrow(ps)), as.character(ps$query_name))
        hits <- by_name[as.character(audit$queryName)]
        for (i in seq_len(n)) {
            rows <- hits[[i]]
            if (is.null(rows)) {
                next
            }
            flags <- as.character(ps$distributionFlag[rows])
            used <- if (has_area) as.character(ps$area[rows]) else character(0)
            audit$distributionFlag[[i]] <- if (length(rows) == 1L || !has_area) {
                flags[[1L]]
            } else {
                paste(sprintf("%s: %s", used, flags), collapse = "; ")
            }
            audit$gbifRecords[[i]] <- sum(as.integer(ps$gbif_count[rows]), na.rm = TRUE)
            audit$areaName[[i]] <- collapse_or_na(used)
            audit$areaStates[[i]] <- area_ufb(used, "area_states")
            audit$areaBiomes[[i]] <- area_ufb(used, "area_biomes")
        }
    }

    # No per-area breakdown to draw on: fall back to the upload-wide union.
    if (length(areas) == 0L) {
        audit$areaStates <- collapse_or_na(geo$area_states)
        audit$areaBiomes <- collapse_or_na(geo$area_biomes)
    }
    audit
}

#' Derive an audit matchType from the cascade validation_status
#'
#' Accepted names split into "exact" (unchanged) vs "corrected" (name changed).
#' The other statuses pass through. Mirrors the reference audit vocabulary while
#' staying faithful to the ObservaBio provider contract's `validation_status`.
#'
#' @noRd
audit_match_type <- function(validation_status, query_name, final_name) {
    vs <- as.character(validation_status)
    changed <- normalize_for_matching(final_name) != normalize_for_matching(query_name)
    changed[is.na(changed)] <- FALSE
    out <- vs
    out[vs == "accepted" & !changed] <- "exact"
    out[vs == "accepted" & changed] <- "corrected"
    out
}

#' Derive a human-readable decisionReason from status + provider
#' @noRd
audit_decision_reason <- function(validation_status, provider) {
    vs <- as.character(validation_status)
    prov <- as.character(provider)
    prov[is.na(prov) | !nzchar(prov)] <- "none"
    reason <- rep(NA_character_, length(vs))
    reason[vs == "accepted"]  <- paste0("accepted_match_", prov[vs == "accepted"])
    reason[vs == "synonym"]   <- paste0("synonym_resolved_", prov[vs == "synonym"])
    reason[vs == "ambiguous"] <- paste0("ambiguous_match_", prov[vs == "ambiguous"])
    reason[vs == "not_found"] <- "not_found_kept_input"
    reason
}

#' Map each cascade query_name back to its pipe-joined original input names
#' @noRd
audit_original_names <- function(query_names, original_names = NULL) {
    q <- as.character(query_names)
    if (is.null(original_names) || length(original_names) == 0L) {
        return(q)
    }
    orig <- as.character(original_names)
    orig_key <- vapply(orig, normalize_scientific_name,
                       FUN.VALUE = character(1), USE.NAMES = FALSE)
    vapply(q, function(qn) {
        hits <- orig[!is.na(orig_key) & orig_key == qn]
        hits <- unique(hits[is_non_empty(hits)])
        if (length(hits) == 0L) qn else paste(hits, collapse = " | ")
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Build the per-name audit table from a cascade result
#'
#' @param cascade Canonical-schema cascade result.
#' @param original_names Optional raw input names, used to fill `originalName`
#'   (grouped by the normalized name). When omitted, `queryName` is reused.
#' @param geo Optional `run_geo_verification()` result; when supplied, the four
#'   geo columns ([audit_geo_columns()]) are appended (SPEC §9).
#' @param invasive_sources Optional [resolve_invasive_flags()] result; when
#'   supplied, the two [invasive_columns()] are appended.
#' @return Data frame in [audit_columns()] order (plus the invasive columns when
#'   `invasive_sources` and the geo columns when `geo`).
#' @noRd
build_audit_table <- function(cascade, original_names = NULL, geo = NULL,
                              invasive_sources = NULL) {
    cols <- audit_columns()
    if (is.null(cascade) || !is.data.frame(cascade) || nrow(cascade) == 0L) {
        empty <- stats::setNames(
            as.data.frame(rep(list(character(0)), length(cols)),
                          stringsAsFactors = FALSE, check.names = FALSE),
            cols
        )
        return(audit_attach_geo(audit_attach_invasive(empty, invasive_sources), geo))
    }
    q <- as.character(cascade$query_name)
    final_name <- as.character(cascade$scientificName)

    audit <- data.frame(
        originalName        = audit_original_names(q, original_names),
        queryName           = q,
        validator           = as.character(cascade$provider),
        matchType           = audit_match_type(cascade$validation_status, q, final_name),
        finalScientificName = final_name,
        decisionReason      = audit_decision_reason(cascade$validation_status, cascade$provider),
        stringsAsFactors    = FALSE
    )
    for (col in c("taxonRank", "taxonomicStatus", "taxonID", "kingdom", "phylum",
                  "class", "order", "family", "genus", "specificEpithet",
                  "infraspecificEpithet")) {
        audit[[col]] <- if (col %in% names(cascade)) as.character(cascade[[col]]) else NA_character_
    }
    audit <- audit[, cols, drop = FALSE]
    audit <- audit_attach_invasive(audit, invasive_sources)
    audit <- audit_attach_geo(audit, geo)
    rownames(audit) <- NULL
    audit
}

#' Subset of the audit table that needs expert attention
#'
#' Unresolved = ambiguous/not_found match, missing `taxonID`/`kingdom`, or an
#' **uncertain identification** (the `originalName` carries a cf./aff./sp.
#' qualifier). This covers the `safe_rank` bucket of script 260626 and goes one
#' step further, on purpose: the reference *promotes* an uncertain name that
#' matches an accepted binomial exactly (`exact_promoted`) and leaves it
#' unflagged. ObservaBio flags it anyway — `Scinax aff. fuscovarius` means the
#' recorder was not sure it is that species, and `normalize_scientific_name()`
#' strips the qualifier before the cascade, so without this the doubt would be
#' silently promoted to a confident `taxonID` (ADR-015).
#'
#' @param audit_tbl Data frame from [build_audit_table()].
#' @return The filtered subset (same columns).
#' @noRd
audit_unresolved <- function(audit_tbl) {
    if (!is.data.frame(audit_tbl) || nrow(audit_tbl) == 0L) {
        return(audit_tbl)
    }
    flagged <- audit_tbl$matchType %in% c("ambiguous", "not_found") |
        !is_non_empty(audit_tbl$taxonID) |
        !is_non_empty(audit_tbl$kingdom) |
        has_name_qualifier(audit_tbl$originalName)
    out <- audit_tbl[flagged, , drop = FALSE]
    rownames(out) <- NULL
    out
}
