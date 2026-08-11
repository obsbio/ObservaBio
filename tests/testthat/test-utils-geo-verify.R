# Geographic verification orchestration (SPEC §8, week 7 runtime). Offline: the
# GBIF network primitive is never reached (stubbed `fetch`), the providers are
# fakes, and the UF/biome layer is a hand-built sf. Exercises the composition,
# the accepted-name → query_name re-keying, and all four distributionFlag levels.

skip_if_not_installed("sf")

reset_occ_cache <- function() {
    .gbif_occ_cache$wkt <- NULL
    .gbif_occ_cache$by_name <- NULL
}

# A covering square over the SP-ish test area, as one labelled layer.
mk_layer <- function(col, val) {
    poly <- sf::st_polygon(list(rbind(
        c(-48, -23), c(-46, -23), c(-46, -21), c(-48, -21), c(-48, -23)
    )))
    df <- stats::setNames(data.frame(val, stringsAsFactors = FALSE), col)
    sf::st_sf(df, geometry = sf::st_sfc(poly, crs = 4326))
}

sample_area <- function(cx = -47, cy = -22) {
    sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(cx, cy)), crs = 4326))
}

sample_uf_biomes <- function() {
    list(states = mk_layer("uf", "SP"), biomes = mk_layer("biome", "Mata Atlântica"))
}

# A cascade whose query_name (normalized/lowercase) differs from the accepted
# scientificName, so the re-keying is actually tested.
sample_cascade <- function() {
    data.frame(
        query_name = c("aaa bbb", "ccc ddd", "eee fff", "ggg hhh"),
        scientificName = c("Aaa bbb", "Ccc ddd", "Eee fff", "Ggg hhh"),
        validation_status = c("accepted", "accepted", "synonym", "not_found"),
        stringsAsFactors = FALSE
    )
}

# Fake provider: SP for sp1/sp2 (matches area), AM for sp3 (no match), none for sp4.
fake_dist_provider <- function() {
    list(distribution = function(names, data = NULL) {
        st <- c("Aaa bbb" = "SP", "Ccc ddd" = "SP", "Eee fff" = "AM", "Ggg hhh" = NA)
        bi <- c("Aaa bbb" = "Mata Atlântica", "Ccc ddd" = "Mata Atlântica",
                "Eee fff" = "Amazônia", "Ggg hhh" = NA)
        data.frame(query_name = names, states = unname(st[names]),
                   biomes = unname(bi[names]), stringsAsFactors = FALSE)
    })
}

# Fetch stub: a GBIF point inside the buffer only for sp1.
fetch_only_sp1 <- function(nm, wkt, max_records, page_size) {
    if (identical(nm, "Aaa bbb")) {
        data.frame(species = nm, decimalLongitude = -47, decimalLatitude = -22,
                   datasetKey = "ds-1", stringsAsFactors = FALSE)
    } else {
        gbif_occ_parse(NULL)
    }
}

# ---------------------------------------------------------------------------

test_that("run_geo_verification returns NULL when no shapefile is provided", {
    expect_null(run_geo_verification(NULL, sample_cascade()))
})

test_that("area/buffer come out even with an empty cascade (map can still draw)", {
    reset_occ_cache()
    geo <- run_geo_verification(
        sample_area(), cascade = sample_cascade()[0, ],
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_only_sp1
    )
    expect_false(is.null(geo))
    expect_true(inherits(geo$area, "sfc"))
    expect_true(inherits(geo$buffer, "sfc"))
    expect_equal(geo$area_states, "SP")
    expect_equal(geo$area_biomes, "Mata Atlântica")
    expect_equal(nrow(geo$per_species), 0L)
    expect_length(geo$distribution_flags, 0L)
})

test_that("run_geo_verification classifies all four distributionFlag levels", {
    reset_occ_cache()
    geo <- run_geo_verification(
        sample_area(), sample_cascade(),
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_only_sp1
    )
    lv <- distribution_flag_levels()
    ps <- geo$per_species
    expect_equal(nrow(ps), 4L)

    flags <- stats::setNames(ps$distributionFlag, ps$scientificName)
    expect_equal(flags[["Aaa bbb"]], lv[["confirmed"]])    # GBIF point in buffer
    expect_equal(flags[["Ccc ddd"]], lv[["near_absent"]])  # no GBIF, state matches
    expect_equal(flags[["Eee fff"]], lv[["outside"]])      # no GBIF, no match
    expect_equal(flags[["Ggg hhh"]], lv[["no_data"]])      # nothing known
})

test_that("distribution_flags are keyed by (area, normalized query_name)", {
    reset_occ_cache()
    geo <- run_geo_verification(
        sample_area(), sample_cascade(),
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_only_sp1
    )
    df <- geo$distribution_flags
    # A bare area gets the default name, so the key is "<area>|<query_name>".
    expect_named(df, area_flag_key("Área de estudo",
                                   c("aaa bbb", "ccc ddd", "eee fff", "ggg hhh")))
    key <- area_flag_key("Área de estudo", "aaa bbb")
    expect_equal(unname(df[[key]]), distribution_flag_levels()[["confirmed"]])
})

test_that("occ carries only the in-buffer GBIF points and gbif_count reflects them", {
    reset_occ_cache()
    geo <- run_geo_verification(
        sample_area(), sample_cascade(),
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_only_sp1
    )
    expect_equal(nrow(geo$occ), 1L)
    expect_equal(geo$occ$species, "Aaa bbb")
    counts <- stats::setNames(geo$per_species$gbif_count, geo$per_species$scientificName)
    expect_equal(counts[["Aaa bbb"]], 1L)
    expect_equal(counts[["Ccc ddd"]], 0L)
})

test_that("process_summary derives records/species/resolved and geo alerts", {
    reset_occ_cache()
    cascade <- sample_cascade()
    geo <- run_geo_verification(
        sample_area(), cascade,
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_only_sp1
    )
    dwc <- data.frame(scientificName = cascade$scientificName, stringsAsFactors = FALSE)
    s <- process_summary(dwc, cascade, geo)
    expect_equal(s$records, 4L)
    expect_equal(s$species, 4L)
    expect_equal(s$resolved_pct, 75L)                 # 3 of 4 accepted/synonym
    expect_equal(s$alerts, 1L)                        # one "outside"
    expect_equal(unname(s$flag_counts[["confirmed"]]), 1L)
    expect_equal(unname(s$flag_counts[["no_data"]]), 1L)
})

test_that("process_summary tolerates a NULL geo (no shapefile)", {
    cascade <- sample_cascade()
    dwc <- data.frame(scientificName = cascade$scientificName, stringsAsFactors = FALSE)
    s <- process_summary(dwc, cascade, NULL)
    expect_true(is.na(s$alerts))
    expect_equal(sum(s$flag_counts), 0L)
})

test_that("process_summary counts invasive SPECIES, not invasive records", {
    cascade <- sample_cascade()
    # Two records of the same listed species, plus one that is not listed.
    dwc <- data.frame(
        scientificName = c("Aaa bbb", "Aaa bbb", "Ccc ddd"),
        invasive = c(TRUE, TRUE, NA),
        invasiveSource = c("GRIIS Brasil", "GRIIS Brasil", NA),
        stringsAsFactors = FALSE
    )
    expect_equal(process_summary(dwc, cascade, NULL)$invasive, 1L)
})

test_that("process_summary reports no invasives when the check did not run", {
    cascade <- sample_cascade()
    dwc <- data.frame(scientificName = cascade$scientificName, stringsAsFactors = FALSE)
    expect_equal(process_summary(dwc, cascade, NULL)$invasive, 0L)
})

test_that("build_results_view carries the invasive columns through to the table", {
    dwc <- data.frame(
        scientificName = c("Aaa bbb", "Ccc ddd"),
        invasive = c(TRUE, NA),
        invasiveSource = c("UCs Federais", NA),
        stringsAsFactors = FALSE
    )
    view <- build_results_view(dwc, NULL, NULL)

    expect_type(view$invasive, "logical")
    expect_equal(view$invasive, c(TRUE, NA))
    expect_equal(view$invasiveSource, c("UCs Federais", NA))
})

test_that("build_results_view joins validator/matchType/gbif_count, marks prevalidated rows", {
    reset_occ_cache()
    accepted <- c("Aaa bbb", "Ccc ddd", "Eee fff", "Ggg hhh")
    qn <- vapply(accepted, normalize_scientific_name, character(1), USE.NAMES = FALSE)
    cascade <- data.frame(
        query_name = qn, scientificName = accepted,
        validation_status = c("accepted", "accepted", "synonym", "not_found"),
        provider = c("florabr", "florabr", "faunabr", "gbif"),
        stringsAsFactors = FALSE
    )
    geo <- run_geo_verification(
        sample_area(), cascade, providers = list(fake_dist_provider()),
        uf_biomes = sample_uf_biomes(), fetch = fetch_only_sp1
    )
    dwc <- data.frame(
        scientificName = c(accepted, "Zzz zzz"),   # 5th row is prevalidated
        family = c("F1", "F2", "F3", "F4", "F5"),
        distributionFlag = c(unname(geo$distribution_flags), NA),
        stringsAsFactors = FALSE
    )
    v <- build_results_view(dwc, cascade, geo)
    expect_equal(nrow(v), 5L)
    expect_equal(v$validator[1], "florabr")
    expect_equal(v$matchType[1], "aceito")
    expect_equal(v$matchType[3], "sinônimo")
    expect_true(is.na(v$validator[5]))          # prevalidated -> no cascade match
    expect_equal(v$gbif_count[1], 1L)
    expect_equal(distribution_flag_class(v$distributionFlag[1]), "flag-confirmed")
    expect_equal(distribution_flag_class(NA), "flag-na")
})

# ---- multi-area ------------------------------------------------------------

# Two areas far apart: the GBIF stub only puts a point inside the first one, so
# the same species must come out "confirmada" in one area and not in the other.
sample_two_areas <- function() {
    list(
        list(name = "RPPN", localities = "RPPN Rio do Brasil", geom = sample_area(-47, -22)),
        list(name = "Trijuncao", localities = "Fazenda Trijunção", geom = sample_area(-46.5, -21.5))
    )
}

fetch_near_first_area <- function(nm, wkt, max_records, page_size) {
    if (identical(nm, "Aaa bbb")) {
        data.frame(species = nm, decimalLongitude = -47, decimalLatitude = -22,
                   datasetKey = "ds-1", stringsAsFactors = FALSE)
    } else {
        gbif_occ_parse(NULL)
    }
}

test_that("run_geo_verification verifies each area against its own species", {
    reset_occ_cache()
    name_map <- data.frame(
        area = c("RPPN", "RPPN", "Trijuncao"),
        query_name = c("aaa bbb", "eee fff", "ccc ddd"),
        scientificName = c("Aaa bbb", "Eee fff", "Ccc ddd"),
        stringsAsFactors = FALSE
    )
    geo <- run_geo_verification(
        sample_two_areas(), name_map, providers = list(fake_dist_provider()),
        uf_biomes = sample_uf_biomes(), fetch = fetch_near_first_area
    )

    expect_length(geo$areas, 2L)
    expect_equal(vapply(geo$areas, function(a) a$name, character(1)),
                 c("RPPN", "Trijuncao"))
    # Each area only saw the species its own records claim.
    expect_equal(sort(geo$per_species$query_name[geo$per_species$area == "RPPN"]),
                 c("aaa bbb", "eee fff"))
    expect_equal(geo$per_species$query_name[geo$per_species$area == "Trijuncao"],
                 "ccc ddd")
    # Both geometries come out combined, so the map can frame everything at once.
    expect_length(geo$area, 2L)
    expect_length(geo$buffer, 2L)
})

test_that("the same species can carry different flags in different areas", {
    reset_occ_cache()
    # "Aaa bbb" is claimed by both areas, but only the first has a GBIF point.
    name_map <- data.frame(
        area = c("RPPN", "Trijuncao"),
        query_name = c("aaa bbb", "aaa bbb"),
        scientificName = c("Aaa bbb", "Aaa bbb"),
        stringsAsFactors = FALSE
    )
    geo <- run_geo_verification(
        sample_two_areas(), name_map, providers = list(fake_dist_provider()),
        uf_biomes = sample_uf_biomes(), fetch = fetch_near_first_area
    )
    lv <- distribution_flag_levels()

    expect_equal(geo$distribution_flags[[area_flag_key("RPPN", "aaa bbb")]],
                 lv[["confirmed"]])
    # Same species, other area: no point in THAT buffer, but the state matches.
    expect_equal(geo$distribution_flags[[area_flag_key("Trijuncao", "aaa bbb")]],
                 lv[["near_absent"]])
    # The occurrences carry the area they were found for.
    expect_true(all(geo$occ$area == "RPPN"))
})

test_that("build_dwc_output joins the flag per (species, area)", {
    reset_occ_cache()
    records <- data.frame(
        scientificName = c("Aaa bbb", "Aaa bbb", "Aaa bbb"),
        locality = c("RPPN Rio do Brasil", "Fazenda Trijunção", "Sítio Sem Área"),
        stringsAsFactors = FALSE
    )
    areas <- sample_two_areas()
    record_areas <- assign_record_areas(records, areas)
    geo <- run_geo_verification(
        areas, geo_name_map_by_area(records, NULL, record_areas),
        providers = list(fake_dist_provider()), uf_biomes = sample_uf_biomes(),
        fetch = fetch_near_first_area
    )
    out <- build_dwc_output(records, NULL, distribution_flags = geo$distribution_flags,
                            record_areas = record_areas)
    lv <- distribution_flag_levels()

    expect_equal(out$distributionFlag[[1L]], lv[["confirmed"]])
    expect_equal(out$distributionFlag[[2L]], lv[["near_absent"]])
    # Row 3 is linked to no area, so it is not verified at all.
    expect_true(is.na(out$distributionFlag[[3L]]))
})

test_that("audit_attach_geo keeps one row per name and spells out the areas", {
    reset_occ_cache()
    name_map <- data.frame(
        area = c("RPPN", "Trijuncao"),
        query_name = c("aaa bbb", "aaa bbb"),
        scientificName = c("Aaa bbb", "Aaa bbb"),
        stringsAsFactors = FALSE
    )
    geo <- run_geo_verification(
        sample_two_areas(), name_map, providers = list(fake_dist_provider()),
        uf_biomes = sample_uf_biomes(), fetch = fetch_near_first_area
    )
    audit <- data.frame(queryName = "aaa bbb", stringsAsFactors = FALSE)
    out <- audit_attach_geo(audit, geo)

    expect_equal(nrow(out), 1L)
    expect_equal(out$areaName, "RPPN; Trijuncao")
    expect_match(out$distributionFlag, "^RPPN: .+; Trijuncao: ")
    expect_equal(out$gbifRecords, 1L)          # summed across the two areas
})

test_that("providers_distribution unions tokens and ignores providers without the slot", {
    p_with <- fake_dist_provider()
    p_without <- list(query = function(names, data = NULL) NULL)  # GBIF-like, no distribution
    d <- providers_distribution(c("Aaa bbb", "Eee fff"), list(p_without, p_with))
    expect_equal(nrow(d), 2L)
    expect_equal(d$states[d$query_name == "Aaa bbb"], "SP")
    expect_equal(d$states[d$query_name == "Eee fff"], "AM")
})

# ---- geo_name_map -----------------------------------------------------------

test_that("geo_name_map unions cascade (accepted) with already-validated species", {
    q_aaa <- normalize_scientific_name("Aaa bbb")
    q_ccc <- normalize_scientific_name("Original ccc")
    q_eee <- normalize_scientific_name("Eee fff")
    cascade <- data.frame(
        query_name = c(q_aaa, q_ccc),
        scientificName = c("Aaa bbb", "Ccc accepted"),   # 2nd = resolved synonym
        stringsAsFactors = FALSE
    )
    # "Eee fff" is a validated row absent from the cascade.
    records <- data.frame(
        scientificName = c("Aaa bbb", "Eee fff", "Eee fff"),
        stringsAsFactors = FALSE
    )
    m <- geo_name_map(records, cascade)
    expect_setequal(m$query_name, c(q_aaa, q_ccc, q_eee))
    expect_equal(nrow(m), 3L)                                   # deduped
    # Cascade wins on collision (keeps the accepted name it resolved).
    expect_equal(m$scientificName[m$query_name == q_aaa], "Aaa bbb")
    expect_equal(m$scientificName[m$query_name == q_ccc], "Ccc accepted")
    # A validated-only species uses its own name as the accepted name.
    expect_equal(m$scientificName[m$query_name == q_eee], "Eee fff")
})

test_that("geo_name_map covers a fully pre-validated sheet (empty cascade)", {
    records <- data.frame(
        scientificName = c("Geophagus brasiliensis", "Astyanax"),
        stringsAsFactors = FALSE
    )
    m <- geo_name_map(records, cascade = NULL)
    expect_equal(nrow(m), 2L)
    expect_setequal(m$scientificName, c("Geophagus brasiliensis", "Astyanax"))
})
