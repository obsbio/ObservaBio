# Title: Darwin Core Output + Audit Mapping
# Turns a taxonomic cascade result (canonical schema, one row per unique
# normalized name; see provider_contract.R) plus the uploaded records into the
# two ObservaBio deliverables (SPEC §9):
#   1. standardized DwC sheet — one row per record, the input column model
#      preserved, taxonomy filled for newly resolved names.
#   2. audit table — one decision per name (one row per name and study area
#      when the geo check ran), plus its `nao_resolvidos` subset.
# Pure functions with no Shiny coupling; utils_export.R writes them to .xlsx.
#
# Scope note: this layer fills only the columns of the canonical schema
# (provider_contract.R), which includes the authorship, establishmentMeans and
# taxonRemarks that the BR bases carry (ADR-029). Other model columns are
# preserved from the upload but never synthesized here. The Saira DwC-mapping
# module and its term catalog are out of scope (SPEC §3), so no
# `dwc_terms.rds` is used.

# ---------------------------------------------------------------------------
# Standardized DwC output (one row per record)
# ---------------------------------------------------------------------------

#' Canonical taxonomy columns copied from a resolved name onto its records
#' @noRd
dwc_taxonomy_columns <- function() {
    c("scientificName", "taxonID", "taxonRank", "taxonomicStatus",
      "acceptedNameUsageID", "kingdom", "phylum", "class", "order",
      "family", "genus", "specificEpithet", "infraspecificEpithet",
      "vernacularName", "scientificNameAuthorship", "establishmentMeans",
      "taxonRemarks")
}

#' Columns that spell out the resolved name itself
#'
#' The cascade value replaces the input even when it is empty, so a record
#' never mixes parts of two names: in one real sheet, the author "Maas" sat in
#' `infraspecificEpithet`. Every other column keeps its input value when the
#' cascade has none (ADR-029).
#' @noRd
dwc_name_columns <- function() {
    c("scientificName", "taxonID", "taxonRank", "taxonomicStatus",
      "acceptedNameUsageID", "genus", "specificEpithet", "infraspecificEpithet")
}

#' Author that the user typed after a name, e.g. "(Linnaeus, 1766)"
#'
#' The typed name must start with the output name, and the rest must look like
#' an author: it starts with a capital letter or "(". So "sp." or an epithet
#' after the name is not taken for an author.
#'
#' @param typed Character vector of names as uploaded.
#' @param name Character vector of output names, same length.
#' @return Character vector of authorship strings, NA where there is none.
#' @noRd
typed_authorship <- function(typed, name) {
    typed <- gsub("\\s+", " ", trimws(as.character(typed)))
    name <- gsub("\\s+", " ", trimws(as.character(name)))
    out <- rep(NA_character_, length(typed))
    ok <- !is.na(typed) & !is.na(name) & nzchar(name) &
        startsWith(tolower(typed), paste0(tolower(name), " "))
    rest <- trimws(substring(typed[ok], nchar(name[ok]) + 2L))
    out[ok] <- ifelse(grepl("^[A-Z(]", rest), rest, NA_character_)
    out
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

#' The federal portaria that a `statusSource` value is, as "number/year"
#'
#' The value must be one reference and nothing more: "Portaria 148/2022",
#' "Portaria MMA nº 148, de 7 de junho de 2022", "Portaria GM/MMA 1.783/2026".
#' A value with an other body ("Portaria SEMA 148/2022"), a second source or
#' free text gives NA. Such a value can be a state list, and only the user
#' knows if it is current.
#'
#' @param source Character vector of `statusSource` values.
#' @return Character vector, same length as `source`, NA where the value is
#'   not one plain federal reference.
#' @noRd
dwc_mma_reference <- function(source) {
    src <- gsub("(\\d)\\.(\\d)", "\\1\\2", as.character(source))
    key <- normalize_for_matching(src)
    pattern <- paste0(
        "^portaria(?: do)?(?: gm)?(?: mma)?(?: no?)? ([0-9]+)",
        "(?: de [0-9]{1,2} de [a-z]+)?(?: de)? ([0-9]{4})$"
    )
    hit <- !is.na(key) & grepl(pattern, key, perl = TRUE)
    out <- rep(NA_character_, length(src))
    out[hit] <- sub(pattern, "\\1/\\2", key[hit], perl = TRUE)
    out
}

#' Is a `statusSource` value an MMA list that the embedded base replaced?
#'
#' Only a value that is one plain federal reference to a replaced portaria
#' ([mma_replaced_sources()]) counts. The word "portaria" alone does not
#' prove that a list is revoked, or that the list is from the MMA. Other
#' values (a state list, an IUCN note, a mixed source) belong to the user,
#' and they stay (ADR-029).
#'
#' @param source Character vector of `statusSource` values.
#' @param replaced_sources "number/year" of the replaced portarias.
#' @return Logical vector, same length as `source`.
#' @noRd
dwc_stale_mma_source <- function(source, replaced_sources) {
    ref <- dwc_mma_reference(source)
    !is.na(ref) & ref %in% gsub(".", "", replaced_sources, fixed = TRUE)
}

#' Write the current MMA answer onto output rows
#'
#' The MMA check is offline and costs nothing, so every row gets the current
#' answer, also a row that arrives already validated (audit A02):
#'
#' - A listed species gets the current label and source.
#' - An unlisted species loses a status whose source is a replaced list.
#' - Any other input value stays (ADR-029).
#'
#' Only the `status`/`statusSource` columns of the input model change.
#'
#' @param records Output records.
#' @param rows Row indices that the answer applies to.
#' @param category,source MMA category code and source, one per row of `rows`
#'   (NA where the species is not listed).
#' @param replaced_sources Portarias that the embedded base replaced.
#' @return `records`.
#' @noRd
dwc_apply_mma_status <- function(records, rows, category, source, replaced_sources) {
    has_status <- "status" %in% names(records)
    has_source <- "statusSource" %in% names(records)
    if (length(rows) == 0L || !(has_status || has_source)) {
        return(records)
    }
    label <- format_mma_status_label(category)
    listed <- !is.na(label)
    if (has_status) {
        records$status[rows[listed]] <- label[listed]
    }
    if (has_source) {
        records$statusSource[rows[listed]] <- as.character(source)[listed]
        stale <- rows[!listed][dwc_stale_mma_source(records$statusSource[rows[!listed]],
                                                    replaced_sources)]
        records$statusSource[stale] <- NA_character_
        if (has_status) {
            records$status[stale] <- NA_character_
        }
    }
    records
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
#' @param mma_status Optional data frame `statusMMA`/`statusSourceMMA`, one row
#'   per record ([mma_status_for_names()] on the record names). It gives the MMA
#'   answer to the rows that the cascade does not fill. `NULL` leaves those
#'   rows as they are.
#' @param mma_replaced Portarias that the embedded MMA base replaced
#'   ([mma_replaced_sources()]), to find a status from a replaced list.
#' @return Data frame of records with taxonomy filled, in `model_cols` order.
#' @noRd
build_dwc_output <- function(records, cascade, model_cols = names(records),
                             name_col = "scientificName",
                             add_distribution_flag = TRUE,
                             distribution_flags = NULL,
                             invasive_sources = NULL,
                             record_areas = NULL,
                             mma_status = NULL,
                             mma_replaced = character(0)) {
    if (!is.data.frame(records)) {
        stop("build_dwc_output(): records must be a data frame.")
    }
    records <- as.data.frame(records, stringsAsFactors = FALSE, check.names = FALSE)
    n <- nrow(records)
    records <- ensure_columns(records, unique(c(model_cols, name_col)))

    prefilled <- dwc_prefilled_mask(records)
    typed_names <- as.character(records[[name_col]])

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
    fill_rows <- integer(0)
    if (has_cascade && length(keys) > 0L) {
        idx <- match(keys, as.character(cascade$query_name))
        fill_rows <- which(!prefilled & !is.na(idx))
        if (length(fill_rows) > 0L) {
            # An empty cascade value never erases an input value outside the
            # name columns. It erased 166 MMA labels and 67 vernacular names on
            # rows whose name the cascade could not resolve.
            for (col in dwc_taxonomy_columns()) {
                if (!col %in% names(cascade)) {
                    next
                }
                records <- ensure_columns(records, col)
                vals <- as.character(cascade[[col]][idx[fill_rows]])
                if (!col %in% dwc_name_columns()) {
                    vals <- fill_blank_values(vals, records[[col]][fill_rows])
                }
                records[[col]][fill_rows] <- vals
            }
            # The cascade queries the name without its author. When no base gives
            # an author, keep the one that the user typed after the name.
            if ("scientificNameAuthorship" %in% names(records)) {
                typed <- typed_authorship(typed_names[fill_rows],
                                          records[[name_col]][fill_rows])
                records$scientificNameAuthorship[fill_rows] <- fill_blank_values(
                    records$scientificNameAuthorship[fill_rows], typed
                )
            }
            # Conservation status, only when resolve_threat_status() enriched the
            # cascade and the model actually has the target column (do not invent
            # status columns that the uploaded model does not carry).
            status_map <- dwc_status_columns()
            if ("statusMMA" %in% names(cascade)) {
                records <- dwc_apply_mma_status(
                    records, fill_rows,
                    category = as.character(cascade$statusMMA[idx[fill_rows]]),
                    source = as.character(cascade$statusSourceMMA[idx[fill_rows]]),
                    replaced_sources = mma_replaced
                )
            }
            for (model_col in c("statusIUCN", "criteria")) {
                src <- status_map[[model_col]]
                if (!src %in% names(cascade) || !model_col %in% names(records)) {
                    next
                }
                vals <- as.character(cascade[[src]][idx[fill_rows]])
                records[[model_col]][fill_rows] <-
                    fill_blank_values(vals, records[[model_col]][fill_rows])
            }
        }
    }

    # The rows that the cascade did not fill (already validated, or not
    # resolved) get the MMA answer for the name that they carry. The IUCN
    # columns of these rows stay as they are, because each IUCN check is a
    # network call.
    if (is.data.frame(mma_status) && nrow(mma_status) == n && n > 0L) {
        rest <- setdiff(seq_len(n), fill_rows)
        records <- dwc_apply_mma_status(
            records, rest,
            category = as.character(mma_status$statusMMA[rest]),
            source = as.character(mma_status$statusSourceMMA[rest]),
            replaced_sources = mma_replaced
        )
    }

    # A flag from the input file belongs to another run, another area or another
    # link. Only the current geo verification can give a flag (audit A12).
    if (isTRUE(add_distribution_flag)) {
        records$distributionFlag <- rep(NA_character_, n)
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
# Audit table (one decision per name, one row per name and area)
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
    # rep() keeps the length right for a zero-row audit (audit A03).
    audit$invasive <- rep(NA, nrow(audit))
    audit$invasiveSource <- rep(NA_character_, nrow(audit))
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
#' The audit has **one row per name and study area** (ADR-030). A name verified
#' in two areas gets two rows, each with the flag, the GBIF record count and the
#' UFs/biomes of its own area. The taxonomic columns repeat on each row. A name
#' that no area verified keeps one row with empty geo columns. A `NULL` geo (no
#' shapefile) leaves the audit unchanged.
#'
#' @param audit Base audit data frame (has `queryName`).
#' @param geo `run_geo_verification()` result, or `NULL`.
#' @return `audit` with the geo columns appended, one row per (name, area), when
#'   `geo` is non-NULL.
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
    area_value <- function(names_used, key) {
        vapply(names_used, function(nm) {
            i <- match(nm, area_names)
            if (is.na(i)) NA_character_ else collapse_or_na(areas[[i]][[key]])
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    }

    ps <- geo$per_species
    hits <- vector("list", nrow(audit))
    if (is.data.frame(ps) && nrow(ps) > 0L && nrow(audit) > 0L) {
        by_name <- split(seq_len(nrow(ps)), as.character(ps$query_name))
        hits <- by_name[as.character(audit$queryName)]
    }
    ps_row <- unlist(lapply(hits, function(h) if (length(h) == 0L) NA_integer_ else h),
                     use.names = FALSE)
    ps_col <- function(name) {
        if (!is.data.frame(ps) || !name %in% names(ps)) {
            return(rep(NA_character_, length(ps_row)))
        }
        as.character(ps[[name]])[ps_row]
    }

    out <- audit[rep(seq_len(nrow(audit)), pmax(lengths(hits), 1L)), , drop = FALSE]
    out$distributionFlag <- ps_col("distributionFlag")
    out$gbifRecords <- suppressWarnings(as.integer(ps_col("gbif_count")))
    out$areaName <- ps_col("area")
    out$areaStates <- area_value(out$areaName, "area_states")
    out$areaBiomes <- area_value(out$areaName, "area_biomes")

    # No per-area breakdown to draw on: fall back to the upload-wide union.
    if (length(areas) == 0L && nrow(out) > 0L) {
        out$areaStates <- collapse_or_na(geo$area_states)
        out$areaBiomes <- collapse_or_na(geo$area_biomes)
    }
    rownames(out) <- NULL
    out
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
#' @param geo Optional `run_geo_verification()` result; when supplied, the
#'   geo columns ([audit_geo_columns()]) are appended, one row per name and
#'   study area (ADR-030).
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
