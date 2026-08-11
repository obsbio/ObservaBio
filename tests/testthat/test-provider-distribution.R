# Provider distribution() accessor (SPEC §8 step 4 / §16): the florabr/faunabr
# providers expose per-species UF/biome distribution for the geo cross-check.
# The br_distribution() unit tests run on synthetic bases (no embedded data);
# the behavioral tests skip when the bundled bases are not prepared.

# ---- br_distribution (synthetic bases, offline) -----------------------------

flora_like <- function() {
    data.frame(
        species = c("Genus specia", "Genus specia", "Other specia"),
        states = c("BA;MG", "SP", "RS"),
        biome = c("Cerrado", "Atlantic_Forest;Cerrado", "Pampa"),
        stringsAsFactors = FALSE
    )
}

test_that("br_distribution unions states and biomes across matching rows", {
    out <- br_distribution("t_flora_union", "Genus specia", data = flora_like(),
                           filename = NA, has_biome = TRUE)
    expect_equal(out$query_name, "Genus specia")
    expect_setequal(split_distribution(out$states), c("BA", "MG", "SP"))
    expect_setequal(split_distribution(out$biomes), c("Cerrado", "Atlantic_Forest"))
})

test_that("br_distribution matching is case/accent-insensitive", {
    out <- br_distribution("t_flora_norm", "GÊNUS  SPECIA", data = flora_like(),
                           filename = NA, has_biome = TRUE)
    expect_setequal(split_distribution(out$states), c("BA", "MG", "SP"))
})

test_that("br_distribution returns NA for absent species and empty distributions", {
    out <- br_distribution("t_flora_absent", "Absent specia", data = flora_like(),
                           filename = NA, has_biome = TRUE)
    expect_true(is.na(out$states))
    expect_true(is.na(out$biomes))

    empty_states <- data.frame(species = "Sp one", states = "", biome = NA_character_,
                               stringsAsFactors = FALSE)
    out2 <- br_distribution("t_flora_empty", "Sp one", data = empty_states,
                            filename = NA, has_biome = TRUE)
    expect_true(is.na(out2$states))
    expect_true(is.na(out2$biomes))
})

test_that("br_distribution keeps biomes NA when the base has no biome (fauna)", {
    fauna_like <- data.frame(species = c("Panthera onca", "Panthera onca"),
                             states = c("SP;RJ", "AM"), stringsAsFactors = FALSE)
    out <- br_distribution("t_fauna", "Panthera onca", data = fauna_like,
                           filename = NA, has_biome = FALSE)
    expect_setequal(split_distribution(out$states), c("SP", "RJ", "AM"))
    expect_true(is.na(out$biomes))
})

test_that("br_distribution dedupes names and returns one row per unique input", {
    out <- br_distribution("t_flora_dedup", c("Genus specia", "Genus specia"),
                           data = flora_like(), filename = NA, has_biome = TRUE)
    expect_equal(nrow(out), 1L)

    empty <- br_distribution("t_flora_e0", character(0), data = flora_like(),
                             filename = NA, has_biome = TRUE)
    expect_equal(nrow(empty), 0L)
    expect_identical(names(empty), c("query_name", "states", "biomes"))
})

test_that("br_distribution output feeds geo_crosscheck_distribution", {
    dist <- br_distribution("t_flora_cc", "Genus specia", data = flora_like(),
                            filename = NA, has_biome = TRUE)
    cc <- geo_crosscheck_distribution(
        area_states = "SP", area_biomes = "Cerrado",
        species_states = dist$states, species_biomes = dist$biomes
    )
    expect_true(cc$state_match)
    expect_true(cc$biome_match)
    expect_true(cc$present)
})

# ---- provider distribution() slot (embedded bases, skip if absent) ----------

test_that("florabr provider exposes distribution with states and biomes", {
    p <- florabr_provider()
    skip_if_not(p$is_available(), "florabr base not embedded (run data-raw/prep_florabr.R)")
    expect_true(is.function(p$distribution))
    dist <- p$distribution(c("Handroanthus impetiginosus", "Zzz nonexistus"))
    expect_identical(names(dist), c("query_name", "states", "biomes"))

    row <- dist[dist$query_name == "Handroanthus impetiginosus", , drop = FALSE]
    expect_true("SP" %in% split_distribution(row$states))
    expect_true("Cerrado" %in% split_distribution(row$biomes))
    # An absent name resolves to NA on both dimensions.
    miss <- dist[dist$query_name == "Zzz nonexistus", , drop = FALSE]
    expect_true(is.na(miss$states))
})

test_that("faunabr provider exposes distribution with states only", {
    p <- faunabr_provider()
    skip_if_not(p$is_available(), "faunabr base not embedded (run data-raw/prep_faunabr.R)")
    expect_true(is.function(p$distribution))
    dist <- p$distribution("Panthera onca")
    expect_true(length(split_distribution(dist$states)) > 0L)
    expect_true(is.na(dist$biomes))
})
