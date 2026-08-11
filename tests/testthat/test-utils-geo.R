# Pure sf geo helpers (SPEC §8 part 1): read shapefile, 10 km buffer, area
# UF/biome lookup, and the state/biome distribution cross-check.

skip_if_not_installed("sf")

# A small square polygon (approx 0.1 deg) around a Brazilian-ish coordinate.
make_area <- function(cx, cy, half = 0.05, crs = 4326) {
    ring <- rbind(
        c(cx - half, cy - half), c(cx + half, cy - half),
        c(cx + half, cy + half), c(cx - half, cy + half),
        c(cx - half, cy - half)
    )
    sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = crs))
}

# ---- geo_read_shapefile -----------------------------------------------------

test_that("geo_read_shapefile reads a shapefile and keeps its CRS", {
    area <- make_area(-47.9, -15.8)
    dir <- tempfile("shp_read")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    sf::st_write(area, file.path(dir, "area.shp"), quiet = TRUE)

    read <- geo_read_shapefile(dir)
    expect_s3_class(read, "sf")
    expect_equal(sf::st_crs(read)$epsg, 4326L)
})

test_that("geo_read_shapefile errors on a missing path", {
    expect_error(geo_read_shapefile(tempfile("nope")), "not found")
})

# ---- metric_crs_for ---------------------------------------------------------

test_that("metric_crs_for picks a SIRGAS 2000 UTM zone for a Brazilian area", {
    # Brasília is in UTM zone 23S -> SIRGAS 2000 EPSG:31983.
    expect_equal(metric_crs_for(make_area(-47.9, -15.8)), 31983L)
})

test_that("metric_crs_for falls back to Brazil Polyconic outside the UTM zones", {
    # Far from Brazil (mid-Pacific) -> whole-country metric fallback.
    expect_equal(metric_crs_for(make_area(-140, 0)), 5880L)
})

# ---- geo_buffer -------------------------------------------------------------

test_that("geo_buffer builds a ~10 km disk around a point (known area)", {
    pt <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(-47.9, -15.8)), crs = 4326))
    res <- geo_buffer(pt, dist_m = 10000)

    expect_named(res, c("area", "buffer", "crs_metric", "dist_m"))
    expect_equal(sf::st_crs(res$buffer)$epsg, 4326L)

    # A 10 km buffer of a point is a disk: area ~ pi * 10000^2.
    expected <- pi * 10000^2
    got <- as.numeric(sf::st_area(res$buffer))
    expect_lt(abs(got - expected) / expected, 0.02)
})

test_that("geo_buffer contains the original area", {
    area <- make_area(-47.9, -15.8, half = 0.02)
    res <- geo_buffer(area, dist_m = 10000)
    covered <- sf::st_covers(res$buffer, sf::st_transform(sf::st_geometry(area), 4326), sparse = FALSE)
    expect_true(covered[1, 1])
})

# ---- geo_area_uf_biomes (synthetic layer) -----------------------------------

test_that("geo_area_uf_biomes resolves UF/biome against a synthetic layer", {
    states <- sf::st_sf(
        uf = c("XX", "YY"),
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
            sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
            crs = 4326
        )
    )
    biomes <- sf::st_sf(
        biome = "TestBiome",
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 1), c(0, 1), c(0, 0)))),
            crs = 4326
        )
    )
    area <- make_area(0.5, 0.5, half = 0.1)
    res <- geo_area_uf_biomes(area, layers = list(states = states, biomes = biomes))
    expect_equal(res$states, "XX")
    expect_equal(res$biomes, "TestBiome")

    # An area straddling the two state polygons picks up both.
    straddle <- make_area(1.0, 0.5, half = 0.1)
    res2 <- geo_area_uf_biomes(straddle, layers = list(states = states, biomes = biomes))
    expect_setequal(res2$states, c("XX", "YY"))
})

# ---- geo_area_uf_biomes (real embedded layer) -------------------------------

test_that("geo_area_uf_biomes places Brasília in DF / Cerrado (embedded layer)", {
    skip_if(!file.exists(br_extdata_path(.BR_UF_BIOMES_RDS)),
            "embedded br_uf_biomes.rds not present")
    area <- make_area(-47.93, -15.78, half = 0.02)
    res <- geo_area_uf_biomes(area)
    expect_true("DF" %in% res$states)
    expect_true("Cerrado" %in% res$biomes)
})

# ---- split_distribution -----------------------------------------------------

test_that("split_distribution parses ';'-separated fields and drops sentinels", {
    expect_equal(split_distribution("BA;MG;SP"), c("BA", "MG", "SP"))
    expect_equal(split_distribution("Amazon;Unknown; "), "Amazon")
    expect_equal(split_distribution(NA), character(0))
    expect_equal(split_distribution(""), character(0))
})

# ---- geo_crosscheck_distribution --------------------------------------------

test_that("geo_crosscheck_distribution flags presence in state or biome", {
    res <- geo_crosscheck_distribution(
        area_states = "BA", area_biomes = "Cerrado",
        species_states = "BA;MG", species_biomes = "Cerrado;Caatinga"
    )
    expect_true(res$state_match)
    expect_true(res$biome_match)
    expect_true(res$present)
})

test_that("geo_crosscheck_distribution reports absence when data exists but no overlap", {
    res <- geo_crosscheck_distribution(
        area_states = "BA", area_biomes = "Cerrado",
        species_states = "RS;SC", species_biomes = "Pampa"
    )
    expect_false(res$state_match)
    expect_false(res$biome_match)
    expect_false(res$present)
})

test_that("geo_crosscheck_distribution is NA when the base has no distribution", {
    res <- geo_crosscheck_distribution(
        area_states = "BA", area_biomes = "Cerrado",
        species_states = NA, species_biomes = NA
    )
    expect_true(is.na(res$state_match))
    expect_true(is.na(res$biome_match))
    expect_true(is.na(res$present))
})

test_that("geo_crosscheck_distribution uses states only when biome is absent (fauna)", {
    # faunabr carries no biome column -> biome_match NA, presence rides on state.
    res <- geo_crosscheck_distribution(
        area_states = "SP", area_biomes = "Atlantic_Forest",
        species_states = "SP;RJ", species_biomes = NA
    )
    expect_true(res$state_match)
    expect_true(is.na(res$biome_match))
    expect_true(res$present)
})

# ---- classify_distribution_flag (SPEC §8 steps 5-6) -------------------------

test_that("classify_distribution_flag returns the four verbatim categories", {
    lv <- distribution_flag_levels()
    # GBIF inside the buffer -> confirmed, regardless of the cross-check.
    expect_equal(classify_distribution_flag(TRUE, NA), "confirmada")
    expect_equal(classify_distribution_flag(TRUE, FALSE), "confirmada")
    # No GBIF, but present in the area's UF/biome.
    expect_equal(classify_distribution_flag(FALSE, TRUE),
                 "sem registro próximo, presente no estado/bioma")
    # No GBIF and not present where data exists -> possible mis-ID.
    expect_equal(classify_distribution_flag(FALSE, FALSE),
                 "sem registro no estado/bioma")
    # No distribution data anywhere.
    expect_equal(classify_distribution_flag(FALSE, NA), "sem dados disponíveis")
    expect_setequal(unname(lv), unique(c(
        classify_distribution_flag(TRUE, NA),
        classify_distribution_flag(FALSE, TRUE),
        classify_distribution_flag(FALSE, FALSE),
        classify_distribution_flag(FALSE, NA)
    )))
})

test_that("classify_distribution_flag treats NA GBIF as no confirmation", {
    # A species with no georeferenced GBIF occurrence is judged by the
    # cross-check alone (SPEC §8 step 6).
    expect_equal(classify_distribution_flag(NA, TRUE),
                 "sem registro próximo, presente no estado/bioma")
    expect_equal(classify_distribution_flag(NA, NA), "sem dados disponíveis")
})

test_that("classify_distribution_flag is vectorized and name-preserving", {
    out <- classify_distribution_flag(
        has_gbif = c(sp1 = TRUE, sp2 = FALSE, sp3 = FALSE, sp4 = FALSE),
        present  = c(sp1 = NA,   sp2 = TRUE,  sp3 = FALSE, sp4 = NA)
    )
    expect_equal(out, c(
        sp1 = "confirmada",
        sp2 = "sem registro próximo, presente no estado/bioma",
        sp3 = "sem registro no estado/bioma",
        sp4 = "sem dados disponíveis"
    ))
})

test_that("classify_distribution_flag recycles a scalar and errors on a mismatch", {
    expect_equal(
        classify_distribution_flag(FALSE, c(a = TRUE, b = NA)),
        c(a = "sem registro próximo, presente no estado/bioma",
          b = "sem dados disponíveis")
    )
    expect_error(
        classify_distribution_flag(c(TRUE, FALSE), c(TRUE, FALSE, NA)),
        "share a length"
    )
    expect_equal(classify_distribution_flag(logical(0), logical(0)), character(0))
})
