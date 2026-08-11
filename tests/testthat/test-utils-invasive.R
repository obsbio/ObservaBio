# Invasive-species cross-check (SPEC §5): an offline lookup against the embedded
# union of the three national lists. Like the MMA lookup, it must be pure, must
# match the way the validator normalizes names, and must degrade to NA rather than
# abort the pipeline when the base is missing.
#
# The names asserted below are stable listings in the source lists; if the base is
# regenerated from newer sources and one of them moves, the fixture (not the
# lookup) is what changed.

test_that("invasive_lookup flags listed species and leaves the rest NA", {
    out <- invasive_lookup(c("Abrus precatorius", "Panthera onca"))

    expect_equal(nrow(out), 2L)
    expect_true(out$invasive[[1]])
    expect_true(is_non_empty(out$invasiveSource[[1]]))

    # Not listed reads as NA, never FALSE (mirrors statusMMA).
    expect_true(is.na(out$invasive[[2]]))
    expect_true(is.na(out$invasiveSource[[2]]))
})

test_that("invasive_lookup matches through authors and qualifiers", {
    # The base is keyed on the canonical name, so every spelling of the same taxon
    # lands on the same row -- authors included, which is how an already-validated
    # row arrives from a user's sheet.
    plain <- invasive_lookup("Leucaena leucocephala")
    authored <- invasive_lookup("Leucaena leucocephala (Lam.) de Wit")
    qualified <- invasive_lookup("Leucaena cf. leucocephala")

    expect_true(plain$invasive[[1]])
    expect_true(authored$invasive[[1]])
    expect_true(qualified$invasive[[1]])
    expect_equal(authored$invasiveSource[[1]], plain$invasiveSource[[1]])
    expect_equal(qualified$invasiveSource[[1]], plain$invasiveSource[[1]])
})

test_that("invasive_lookup aggregates the sources that list a taxon", {
    # Lissachatina fulica (giant African snail) is on all three lists.
    out <- invasive_lookup("Lissachatina fulica")
    sources <- strsplit(out$invasiveSource[[1]], "; ", fixed = TRUE)[[1]]

    expect_true(out$invasive[[1]])
    expect_length(sources, 3L)
    expect_setequal(sources, c("Instituto Hórus 2023", "GRIIS Brasil", "UCs Federais"))
})

test_that("invasive_lookup preserves input length and order", {
    names_in <- c("Panthera onca", "Abrus precatorius", NA, "", "Sus scrofa")
    out <- invasive_lookup(names_in)

    expect_equal(nrow(out), length(names_in))
    expect_equal(out$scientificName, as.character(names_in))
    expect_equal(which(!is.na(out$invasive)), c(2L, 5L))
})

test_that("invasive_lookup returns the schema on an empty vector", {
    out <- invasive_lookup(character(0))

    expect_equal(nrow(out), 0L)
    expect_equal(names(out), c("scientificName", "invasive", "invasiveSource"))
    expect_type(out$invasive, "logical")
})

test_that("invasive_lookup degrades to NA when the base is missing", {
    # A missing base is a deployment problem, not a reason to lose the user's run.
    .invasive_cache$reset()
    local_mocked_bindings(
        br_extdata_path = function(filename) file.path(tempdir(), "does-not-exist.rds")
    )

    out <- invasive_lookup(c("Abrus precatorius", "Panthera onca"))

    expect_equal(nrow(out), 2L)
    expect_true(all(is.na(out$invasive)))
    expect_true(all(is.na(out$invasiveSource)))
    .invasive_cache$reset()
})

test_that("resolve_invasive_flags covers new AND already-validated species", {
    # The already-validated row (taxonID filled) never reaches the cascade, but the
    # cross-check is offline -- there is no reason to skip it, and a user who
    # uploads a pre-validated sheet still wants the alert.
    records <- data.frame(
        scientificName = c("Abrus precatorius", "Panthera onca", "Cedrela odorata"),
        taxonID = c("already-1", NA, NA),
        kingdom = c("Plantae", NA, NA),
        stringsAsFactors = FALSE
    )
    cascade <- data.frame(
        query_name = c("Panthera onca", "Cedrela odorata"),
        scientificName = c("Panthera onca", "Cedrela odorata"),
        stringsAsFactors = FALSE
    )

    flags <- resolve_invasive_flags(records, cascade)

    expect_named(flags, "Abrus precatorius")
    expect_true(is_non_empty(unname(flags[["Abrus precatorius"]])))
})

test_that("resolve_invasive_flags keys on the cascade's ACCEPTED name", {
    # The user typed a synonym; the cascade resolved it to a listed invasive. The
    # flag must be keyed on the query name (so it joins records) but *found* by the
    # accepted name (so the listing is hit at all).
    records <- data.frame(
        scientificName = "Acacia glauca",
        stringsAsFactors = FALSE
    )
    cascade <- data.frame(
        query_name = "Acacia glauca",
        scientificName = "Leucaena leucocephala",
        stringsAsFactors = FALSE
    )

    flags <- resolve_invasive_flags(records, cascade)

    expect_named(flags, "Acacia glauca")
})

test_that("resolve_invasive_flags returns an empty named vector when nothing is listed", {
    records <- data.frame(scientificName = "Panthera onca", stringsAsFactors = FALSE)
    flags <- resolve_invasive_flags(records, NULL)

    expect_length(flags, 0L)
    expect_type(flags, "character")
})
