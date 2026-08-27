# Standardization parity (SPEC §11): the DwC layer must map a cascade result onto
# uploaded records exactly like the reference script's standardized output — one
# row per record, input column model preserved, already-validated rows kept, new
# names filled from the resolved taxonomy. Driven by a deterministic stub cascade
# (no network), which is the equivalence the parity test asserts. A full diff
# against the client's real spreadsheet needs that fixture, added when available.

# A ObservaBio-shaped upload: DwC model columns, a mix of pre-validated and new rows.
sample_records <- function() {
    model_cols <- c("datasetName", "scientificName", "taxonID", "taxonRank",
                    "kingdom", "phylum", "class", "order", "family", "genus",
                    "specificEpithet", "infraspecificEpithet", "vernacularName",
                    "locality")
    df <- data.frame(
        datasetName = "ObservaBio",
        scientificName = c("Already Validated", "Handroanthus impetiginosus",
                           "Handroanthus impetiginosus", "Cedrela odorata",
                           "Zzz nonexistus"),
        taxonID = c("old-1", NA, NA, NA, NA),
        taxonRank = c("Species", NA, NA, NA, NA),
        kingdom = c("Animalia", NA, NA, NA, NA),
        phylum = NA_character_, class = NA_character_, order = NA_character_,
        family = NA_character_, genus = NA_character_, specificEpithet = NA_character_,
        infraspecificEpithet = NA_character_, vernacularName = NA_character_,
        locality = c("Fazenda A", "Fazenda A", "Fazenda B", "Fazenda A", "Fazenda C"),
        stringsAsFactors = FALSE
    )
    list(df = df, model_cols = model_cols)
}

# Stub cascade result (canonical schema) for the three unique new names.
sample_cascade <- function() {
    data.frame(
        query_name = c("Handroanthus impetiginosus", "Cedrela odorata", "Zzz nonexistus"),
        scientificName = c("Handroanthus impetiginosus", "Cedrela fissilis", "Zzz nonexistus"),
        taxonomicStatus = c("accepted", "synonym", NA_character_),
        validation_status = c("accepted", "synonym", "not_found"),
        match_count = c(1L, 1L, 0L),
        provider = c("florabr", "florabr", NA_character_),
        taxonID = c("t-123", "t-456", NA_character_),
        taxonRank = c("Species", "Species", NA_character_),
        acceptedNameUsageID = c(NA_character_, "a-456", NA_character_),
        kingdom = c("Plantae", "Plantae", NA_character_),
        phylum = c("Tracheophyta", "Tracheophyta", NA_character_),
        class = c("Magnoliopsida", "Magnoliopsida", NA_character_),
        order = c("Lamiales", "Sapindales", NA_character_),
        family = c("Bignoniaceae", "Meliaceae", NA_character_),
        genus = c("Handroanthus", "Cedrela", NA_character_),
        specificEpithet = c("impetiginosus", "fissilis", NA_character_),
        infraspecificEpithet = NA_character_,
        vernacularName = c("ipe-roxo", NA_character_, NA_character_),
        stringsAsFactors = FALSE
    )
}

test_that("build_dwc_output keeps one row per record and the model column order", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    expect_equal(nrow(out), nrow(s$df))
    expect_equal(names(out), c(s$model_cols, "distributionFlag"))
})

test_that("build_dwc_output preserves already-validated rows untouched", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    expect_equal(out$scientificName[[1]], "Already Validated")
    expect_equal(out$taxonID[[1]], "old-1")
    expect_equal(out$kingdom[[1]], "Animalia")
})

test_that("build_dwc_output fills new accepted rows, including duplicates", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    # Rows 2 and 3 are the same name at different localities.
    expect_equal(out$taxonID[c(2, 3)], c("t-123", "t-123"))
    expect_equal(out$family[c(2, 3)], c("Bignoniaceae", "Bignoniaceae"))
    expect_equal(out$scientificName[c(2, 3)],
                 c("Handroanthus impetiginosus", "Handroanthus impetiginosus"))
    expect_equal(out$locality[c(2, 3)], c("Fazenda A", "Fazenda B"))
})

test_that("build_dwc_output resolves a synonym to the accepted name", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    expect_equal(out$scientificName[[4]], "Cedrela fissilis")
    expect_equal(out$taxonID[[4]], "t-456")
    expect_equal(out$kingdom[[4]], "Plantae")
})

test_that("build_dwc_output leaves not-found rows without taxonomy", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    expect_true(is.na(out$taxonID[[5]]))
    expect_true(is.na(out$kingdom[[5]]))
    expect_true(all(is.na(out$distributionFlag)))
})

test_that("build_dwc_output can omit the distributionFlag slot", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            add_distribution_flag = FALSE)
    expect_false("distributionFlag" %in% names(out))
    expect_equal(names(out), s$model_cols)
})

test_that("build_dwc_output spreads distribution_flags onto matching records", {
    s <- sample_records()
    flags <- c(
        "confirmada",
        "sem registro no estado/bioma"
    )
    names(flags) <- c(
        normalize_scientific_name("Handroanthus impetiginosus"),
        normalize_scientific_name("Cedrela odorata")
    )
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            distribution_flags = flags)
    # Rows 2 and 3 are the two Handroanthus records; row 4 is Cedrela odorata.
    expect_equal(out$distributionFlag[2:3], c("confirmada", "confirmada"))
    expect_equal(out$distributionFlag[[4]], "sem registro no estado/bioma")
    # Row 5 (Zzz nonexistus) has no flag -> stays empty.
    expect_true(is.na(out$distributionFlag[[5]]))
})

test_that("build_dwc_output leaves distributionFlag empty without a shapefile", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            distribution_flags = NULL)
    expect_true(all(is.na(out$distributionFlag)))
})

test_that("build_dwc_output writes the geo flag to already-validated rows too", {
    s <- sample_records()
    flags <- c("confirmada")
    names(flags) <- normalize_scientific_name("Already Validated")
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            distribution_flags = flags)
    # Row 1 is pre-validated (taxonID = "old-1"): the geographic check now runs
    # for the whole list, so it IS flagged — but its taxonomy stays intact.
    expect_equal(out$distributionFlag[[1]], "confirmada")
    expect_equal(out$taxonID[[1]], "old-1")
})

test_that("build_dwc_output appends the invasive cross-check columns", {
    s <- sample_records()
    # Keyed like the geo flags: normalized name -> the list(s) that carry it.
    inv <- c("Instituto Hórus 2023; GRIIS Brasil")
    names(inv) <- normalize_scientific_name("Cedrela odorata")

    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            invasive_sources = inv)

    expect_equal(names(out), c(s$model_cols, "distributionFlag", invasive_columns()))
    # Row 4 is the Cedrela odorata record.
    expect_true(out$invasive[[4]])
    expect_equal(out$invasiveSource[[4]], "Instituto Hórus 2023; GRIIS Brasil")
    # Everything else is not listed: NA, never FALSE.
    expect_true(all(is.na(out$invasive[-4])))
    expect_true(all(is.na(out$invasiveSource[-4])))
})

test_that("build_dwc_output flags an already-validated row as invasive", {
    s <- sample_records()
    inv <- c("UCs Federais")
    names(inv) <- normalize_scientific_name("Already Validated")

    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols,
                            invasive_sources = inv)

    # Row 1 never reaches the cascade, but the cross-check runs on the whole list.
    expect_true(out$invasive[[1]])
    expect_equal(out$invasiveSource[[1]], "UCs Federais")
    expect_equal(out$taxonID[[1]], "old-1")
})

test_that("build_dwc_output omits the invasive columns when the check did not run", {
    s <- sample_records()
    out <- build_dwc_output(s$df, sample_cascade(), model_cols = s$model_cols)
    expect_false(any(invasive_columns() %in% names(out)))
})

test_that("build_audit_table maps the cascade to the audit schema", {
    cascade <- sample_cascade()
    audit <- build_audit_table(
        cascade,
        original_names = c("Handroanthus impetiginosus", "Cedrela odorata", "Zzz nonexistus")
    )
    expect_equal(names(audit), audit_columns())
    expect_equal(nrow(audit), nrow(cascade))
    expect_equal(audit$matchType, c("exact", "synonym", "not_found"))
    expect_equal(audit$decisionReason,
                 c("accepted_match_florabr", "synonym_resolved_florabr", "not_found_kept_input"))
    expect_equal(audit$finalScientificName,
                 c("Handroanthus impetiginosus", "Cedrela fissilis", "Zzz nonexistus"))
})

test_that("audit matchType marks a corrected spelling", {
    cascade <- data.frame(
        query_name = "Handroanthus impetiginoso",
        scientificName = "Handroanthus impetiginosus",
        taxonomicStatus = "accepted", validation_status = "accepted",
        match_count = 1L, provider = "florabr", taxonID = "t-1",
        taxonRank = "Species", acceptedNameUsageID = NA_character_,
        kingdom = "Plantae", phylum = NA, class = NA, order = NA,
        family = "Bignoniaceae", genus = "Handroanthus",
        specificEpithet = "impetiginosus", infraspecificEpithet = NA,
        vernacularName = NA, stringsAsFactors = FALSE
    )
    audit <- build_audit_table(cascade)
    expect_equal(audit$matchType, "corrected")
})

test_that("audit_original_names groups distinct originals with a pipe", {
    got <- audit_original_names(
        "Handroanthus impetiginosus",
        original_names = c("Handroanthus impetiginosus (Mart.) Mattos",
                           "handroanthus impetiginosus")
    )
    # Author string and lower-case spelling normalize to the same query name.
    expect_true(grepl(" | ", got, fixed = TRUE))
})

test_that("audit_unresolved keeps only ambiguous/not-found/incomplete rows", {
    cascade <- sample_cascade()
    audit <- build_audit_table(cascade)
    unresolved <- audit_unresolved(audit)
    expect_equal(nrow(unresolved), 1L)
    expect_equal(unresolved$queryName, "Zzz nonexistus")
})

test_that("audit_unresolved flags every uncertain (cf./aff.) name, even an exact match", {
    cascade <- sample_cascade()
    # The cf./aff. qualifier is stripped before the cascade, so it survives only
    # on originalName. An uncertain identification must surface for review even
    # when the stripped binomial resolves exactly (ADR-015: we deliberately go
    # beyond the reference, which would promote it).
    originals <- c(
        "Handroanthus cf. impetiginosus", # exact match -> flagged anyway
        "Cedrela aff. odorata",           # synonym -> flagged
        "Zzz cf. nonexistus"              # not_found -> flagged
    )
    audit <- build_audit_table(cascade, original_names = originals)
    unresolved <- audit_unresolved(audit)
    expect_true(all(c("Handroanthus impetiginosus", "Cedrela odorata",
                      "Zzz nonexistus") %in% unresolved$queryName))

    # A certain name that resolves exactly must still NOT be flagged.
    audit_certain <- build_audit_table(
        cascade,
        original_names = c("Handroanthus impetiginosus", "Cedrela odorata",
                           "Zzz nonexistus")
    )
    certain <- audit_unresolved(audit_certain)
    expect_false("Handroanthus impetiginosus" %in% certain$queryName)
})

test_that("build_audit_table returns an empty schema for an empty cascade", {
    audit <- build_audit_table(empty_canonical_result())
    expect_equal(names(audit), audit_columns())
    expect_equal(nrow(audit), 0L)
})

test_that("build_audit_table without geo keeps the base schema", {
    audit <- build_audit_table(sample_cascade())
    expect_equal(names(audit), audit_columns())
    expect_false(any(audit_geo_columns() %in% names(audit)))
    expect_false(any(invasive_columns() %in% names(audit)))
})

test_that("build_audit_table appends the invasive columns joined by queryName", {
    inv <- c("GRIIS Brasil")
    names(inv) <- normalize_scientific_name("Cedrela odorata")

    audit <- build_audit_table(sample_cascade(), invasive_sources = inv)
    expect_true(all(invasive_columns() %in% names(audit)))

    i <- match("Cedrela odorata", audit$queryName)
    expect_true(audit$invasive[i])
    expect_equal(audit$invasiveSource[i], "GRIIS Brasil")

    z <- match("Zzz nonexistus", audit$queryName)   # not listed -> NA
    expect_true(is.na(audit$invasive[z]))
    expect_true(is.na(audit$invasiveSource[z]))
})

test_that("build_audit_table appends geo columns joined by queryName (SPEC §9)", {
    cascade <- sample_cascade()
    geo <- list(
        per_species = data.frame(
            query_name = c("Handroanthus impetiginosus", "Cedrela odorata"),
            gbif_count = c(4L, 0L),
            distributionFlag = c("confirmada", "sem registro no estado/bioma"),
            stringsAsFactors = FALSE
        ),
        area_states = c("BA", "MG"),
        area_biomes = "Mata Atlântica"
    )
    audit <- build_audit_table(cascade, geo = geo)
    expect_true(all(audit_geo_columns() %in% names(audit)))

    i <- match("Handroanthus impetiginosus", audit$queryName)
    expect_equal(audit$distributionFlag[i], "confirmada")
    expect_equal(audit$gbifRecords[i], 4L)

    z <- match("Zzz nonexistus", audit$queryName)   # not in per_species -> NA
    expect_true(is.na(audit$distributionFlag[z]))

    expect_equal(unique(audit$areaStates), "BA; MG")
    expect_equal(unique(audit$areaBiomes), "Mata Atlântica")
})

test_that("format_mma_status_label maps codes and blanks the rest", {
    expect_equal(
        format_mma_status_label(c("VU", "CR (PEX)", "EN")),
        c("(VU) Vulneravel",
          "(CR (PEX)) Criticamente Em Perigo (Possivelmente Extinta)",
          "(EN) Em Perigo")
    )
    expect_true(is.na(format_mma_status_label(NA)))
    expect_true(is.na(format_mma_status_label("")))
    expect_true(is.na(format_mma_status_label("ZZ")))
})

test_that("build_dwc_output injects conservation status when the model has it", {
    model_cols <- c("scientificName", "taxonID", "kingdom", "status",
                    "statusSource", "statusIUCN", "criteria", "locality")
    records <- data.frame(
        scientificName = c("Handroanthus impetiginosus", "Cedrela odorata"),
        taxonID = NA_character_, kingdom = NA_character_, status = NA_character_,
        statusSource = NA_character_, statusIUCN = NA_character_,
        criteria = NA_character_, locality = c("A", "B"),
        stringsAsFactors = FALSE
    )
    cascade <- sample_cascade()[1:2, ]
    cascade$query_name <- c("Handroanthus impetiginosus", "Cedrela odorata")
    cascade$statusMMA <- c("VU", NA_character_)
    cascade$statusSourceMMA <- c("Portaria 148/2022", NA_character_)
    cascade$iucnCategory <- c("LC", "EN")
    cascade$iucnCriteria <- c(NA_character_, "A2c")

    out <- build_dwc_output(records, cascade, model_cols = model_cols)
    expect_equal(out$status, c("(VU) Vulneravel", NA))
    expect_equal(out$statusSource, c("Portaria 148/2022", NA))
    expect_equal(out$statusIUCN, c("LC", "EN"))
    expect_equal(out$criteria, c(NA, "A2c"))
})

test_that("build_dwc_output ignores status columns the model does not carry", {
    s <- sample_records()
    cascade <- sample_cascade()
    cascade$statusMMA <- "VU"
    cascade$statusSourceMMA <- "Portaria 148/2022"
    cascade$iucnCategory <- "LC"
    cascade$iucnCriteria <- NA_character_
    out <- build_dwc_output(s$df, cascade, model_cols = s$model_cols)
    expect_false("status" %in% names(out))
    expect_false("statusIUCN" %in% names(out))
})

# ---------------------------------------------------------------------------
# Skip already-validated rows before the cascade + IUCN (perf).
# ---------------------------------------------------------------------------

# A provider that records the names it is queried with, delegating the canonical
# result to a fake lookup. Proves pre-validated names never reach the cascade.
make_spy_provider <- function(seen_env, id = "spy", priority = 1L, lookup = list()) {
    inner <- make_fake_provider(id, priority, lookup = lookup)
    inner_query <- inner$query
    inner$query <- function(names, data = NULL) {
        seen_env$seen <- c(seen_env$seen, as.character(names))
        inner_query(names, data)
    }
    inner
}

test_that("dwc_unvalidated_names returns only names from non-prefilled rows", {
    s <- sample_records()
    # Row 1 ("Already Validated") is prefilled (taxonID + kingdom); rows 2-5 new.
    expect_equal(
        dwc_unvalidated_names(s$df),
        c("Handroanthus impetiginosus", "Handroanthus impetiginosus",
          "Cedrela odorata", "Zzz nonexistus")
    )
    expect_false("Already Validated" %in% dwc_unvalidated_names(s$df))
})

test_that("dwc_unvalidated_names is empty for empty / nameless / all-validated input", {
    expect_equal(dwc_unvalidated_names(data.frame()), character(0))
    expect_equal(
        dwc_unvalidated_names(data.frame(other = "x", stringsAsFactors = FALSE)),
        character(0)
    )
    all_validated <- data.frame(
        scientificName = c("Aaa bbb", "Ccc ddd"),
        taxonID = c("t-1", "t-2"), stringsAsFactors = FALSE
    )
    expect_equal(dwc_unvalidated_names(all_validated), character(0))
})

test_that("pre-validated names never reach the cascade, validated output unchanged", {
    s <- sample_records()
    lookup <- list(
        "Handroanthus impetiginosus" = list(
            validation_status = "accepted",
            scientificName = "Handroanthus impetiginosus",
            taxonID = "t-123", kingdom = "Plantae"
        )
    )
    seen <- new.env(parent = emptyenv())
    seen$seen <- character(0)
    spy <- make_spy_provider(seen, lookup = lookup)

    cascade_new <- run_cascade(dwc_unvalidated_names(s$df), providers = list(spy))

    expect_true(normalize_scientific_name("Handroanthus impetiginosus") %in% seen$seen)
    expect_false(normalize_scientific_name("Already Validated") %in% seen$seen)

    # Old path: the whole list (including the pre-validated name) through the cascade.
    cascade_all <- run_cascade(
        as.character(s$df$scientificName),
        providers = list(make_fake_provider("plain", 1L, lookup = lookup))
    )
    out_new <- build_dwc_output(s$df, cascade_new, model_cols = s$model_cols)
    out_all <- build_dwc_output(s$df, cascade_all, model_cols = s$model_cols)

    # The already-validated row is byte-for-byte identical on both paths.
    expect_equal(out_new[1, ], out_all[1, ])
    expect_equal(out_new$scientificName[[1]], "Already Validated")
    expect_equal(out_new$taxonID[[1]], "old-1")
    expect_equal(out_new$kingdom[[1]], "Animalia")
})

test_that("a fully pre-validated sheet sends nothing to the cascade", {
    df <- data.frame(
        scientificName = c("Aaa bbb", "Ccc ddd"),
        taxonID = c("t-1", "t-2"),
        kingdom = c("Plantae", "Animalia"),
        stringsAsFactors = FALSE
    )
    seen <- new.env(parent = emptyenv())
    seen$seen <- character(0)
    spy <- make_spy_provider(seen)

    cascade <- run_cascade(dwc_unvalidated_names(df), providers = list(spy))
    expect_equal(seen$seen, character(0))
    expect_equal(nrow(cascade), 0L)
})
