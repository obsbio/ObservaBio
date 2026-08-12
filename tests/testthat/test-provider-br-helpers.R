# br_join_base_taxonomy(): check_names() returns only a match verdict, so the
# higher taxonomy has to be joined back from the embedded base (ADR-016).

flora_base <- function() {
    data.frame(
        species = c("Cedrela odorata", "Araucaria angustifolia"),
        id = c("9992", "33971"),
        kingdom = c("Plantae", "Plantae"),
        phylum = c("Tracheophyta", "Tracheophyta"),
        class = c("Magnoliopsida", "Pinopsida"),
        order = c("Sapindales", "Pinales"),
        family = c("Meliaceae", "Araucariaceae"),
        genus = c("Cedrela", "Araucaria"),
        taxonRank = c("Species", "Species"),
        stringsAsFactors = FALSE
    )
}

# The Fauna do Brasil base ships no `kingdom` column.
fauna_base <- function() {
    data.frame(
        species = "Hoplias malabaricus",
        id = "58504",
        phylum = "Chordata",
        class = "Actinopterygii",
        order = "Characiformes",
        family = "Erythrinidae",
        genus = "Hoplias",
        taxonRank = "species",
        stringsAsFactors = FALSE
    )
}

test_that("br_join_base_taxonomy recovers taxonID + higher taxonomy by accepted name", {
    out <- data.frame(scientificName = "Cedrela odorata", stringsAsFactors = FALSE)
    got <- br_join_base_taxonomy(out, flora_base())
    expect_equal(got$taxonID, "9992")
    expect_equal(got$kingdom, "Plantae")
    expect_equal(got$class, "Magnoliopsida")
    expect_equal(got$genus, "Cedrela")
})

test_that("br_join_base_taxonomy matches case/accent-insensitively", {
    out <- data.frame(scientificName = "araucaria  ANGUSTIFOLIA", stringsAsFactors = FALSE)
    got <- br_join_base_taxonomy(out, flora_base())
    expect_equal(got$taxonID, "33971")
    expect_equal(got$order, "Pinales")
})

test_that("br_join_base_taxonomy applies default_kingdom when the base has none (faunabr)", {
    out <- data.frame(scientificName = "Hoplias malabaricus", stringsAsFactors = FALSE)
    got <- br_join_base_taxonomy(out, fauna_base(), default_kingdom = "Animalia")
    expect_equal(got$kingdom, "Animalia")
    expect_equal(got$taxonID, "58504")
    expect_equal(got$class, "Actinopterygii")
})

test_that("br_join_base_taxonomy leaves unmatched names blank (no phantom kingdom)", {
    out <- data.frame(scientificName = c("Hoplias malabaricus", "Zzz nonexistus"),
                      stringsAsFactors = FALSE)
    got <- br_join_base_taxonomy(out, fauna_base(), default_kingdom = "Animalia")
    expect_equal(got$kingdom, c("Animalia", NA_character_))
    expect_equal(got$taxonID, c("58504", NA_character_))
})

test_that("br_join_base_taxonomy never overwrites a value the verdict supplied", {
    out <- data.frame(scientificName = "Cedrela odorata",
                      family = "FromCheckNames", stringsAsFactors = FALSE)
    got <- br_join_base_taxonomy(out, flora_base())
    expect_equal(got$family, "FromCheckNames")
    expect_equal(got$kingdom, "Plantae")   # blanks still filled
})

test_that("br_join_base_taxonomy is a no-op without a usable base", {
    out <- data.frame(scientificName = "Cedrela odorata", stringsAsFactors = FALSE)
    expect_identical(br_join_base_taxonomy(out, NULL), out)
    expect_identical(br_join_base_taxonomy(out, data.frame()), out)
})

# ---- br_load_data: the supra-specific filter (embedded bases, skip if absent) ----
# Rows with `species = NA` are what made check_fauna_names() explode; see the note
# in br_load_data() and LESSONS L-017.

test_that("br_load_data drops rows without a species binomial", {
    for (provider in list(florabr_provider(), faunabr_provider())) {
        skip_if_not(provider$is_available(),
                    paste(provider$id, "base not embedded (run data-raw/prep_*.R)"))
        base <- provider$load()
        expect_true("species" %in% names(base))
        expect_equal(sum(is.na(base$species)), 0L)
        expect_gt(nrow(base), 0L)
    }
})

test_that("br_load_data keeps subspecies rows (include_subspecies stays honest)", {
    p <- faunabr_provider()
    skip_if_not(p$is_available(), "faunabr base not embedded (run data-raw/prep_faunabr.R)")
    base <- p$load()
    expect_gt(sum(base$taxonRank %in% c("subspecies", "subespecie")), 0L)
})

test_that("an unresolved name does not blow up the faunabr join", {
    skip_if_not_installed("faunabr")
    p <- faunabr_provider()
    skip_if_not(p$is_available(), "faunabr base not embedded (run data-raw/prep_faunabr.R)")

    # A name the fuzzy match cannot resolve lands as `Suggested_name = NA`, and
    # check_fauna_names() joins it with merge(all = TRUE) — where NA matches NA.
    # With supra-specific rows still in the base this single name produced ~42,660
    # raw rows (and ~485 MB); bounded here means the filter is doing its job.
    raw <- faunabr::check_fauna_names(
        data = p$load(), species = "Zzz nonexistus",
        max_distance = 0.1, include_subspecies = TRUE
    )
    expect_lt(nrow(raw), 100L)
})

# ---- br_map_check_result: deduplication and the fuzzy suggestion ---------------

test_that("a name with an accepted and a synonym row at the same distance takes the accepted one", {
    # Two authors published `Euterpe oleracea`, so the base carries both an
    # Accepted and a Synonym row and check_names() returns both at Distance 0.
    # Whichever came first used to win, and the order shifted with batch size.
    rows <- data.frame(
        input_name = c("Euterpe oleracea", "Euterpe oleracea"),
        Spelling = "Correct",
        Suggested_name = "Euterpe oleracea",
        Distance = c(0, 0),
        taxonomicStatus = c("Synonym", "Accepted"),
        stringsAsFactors = FALSE
    )
    expect_equal(br_map_check_result(rows, "florabr")$validation_status, "accepted")
    # ...and the same regardless of which row the base happened to emit first.
    expect_equal(br_map_check_result(rows[2:1, ], "florabr")$validation_status, "accepted")
})

test_that("a closer suggestion still wins over a further accepted one", {
    rows <- data.frame(
        input_name = "Cedrela fissillis",
        Spelling = "Probably_incorrect",
        Suggested_name = c("Cedrela odorata", "Cedrela fissilis"),
        Distance = c(4, 1),
        taxonomicStatus = c("Accepted", "Synonym"),
        stringsAsFactors = FALSE
    )
    got <- br_map_check_result(rows, "florabr")
    expect_equal(got$scientificName, "Cedrela fissilis")
})

# ---- br_map_check_result: the fuzzy suggestion is what gets recorded ----------

test_that("a misspelling records the suggested name, not the input", {
    # Both bases spell the column `Suggested_name`. Reading it as "Suggested name"
    # returned all-NA, so the mapper fell through to the input and every fuzzy hit
    # kept its typo while being labelled `ambiguous`.
    raw <- data.frame(
        input_name = "Cedrela fissillis",
        Spelling = "Probably_incorrect",
        Suggested_name = "Cedrela fissilis",
        Distance = 1,
        taxonomicStatus = "Accepted",
        stringsAsFactors = FALSE
    )
    got <- br_map_check_result(raw, "florabr")
    expect_equal(got$query_name, "Cedrela fissillis")
    expect_equal(got$scientificName, "Cedrela fissilis")
    expect_equal(got$validation_status, "ambiguous")
})

test_that("a suggestion that changes the name's rank is refused", {
    # agrep() answers a genus with a binomial, and those are different taxa, not
    # near misses: Psidium is the guava genus, Pisidium is a freshwater clam.
    raw <- data.frame(
        input_name = c("Psidium", "Mycena", "Amona crassiflora"),
        Spelling = "Probably_incorrect",
        Suggested_name = c("Pisidium vile", "Mycetarotes acutus", "Annona crassiflora"),
        Distance = c(2, 6, 1),
        taxonomicStatus = "Accepted",
        stringsAsFactors = FALSE
    )
    got <- br_map_check_result(raw, "florabr")
    got <- got[match(c("Psidium", "Mycena", "Amona crassiflora"), got$query_name), ]
    # The two genus inputs keep what the user wrote; only the real typo is fixed.
    expect_equal(got$scientificName,
                 c("Psidium", "Mycena", "Annona crassiflora"))
})

test_that("a name with no suggestion keeps the input as scientificName", {
    raw <- data.frame(
        input_name = "Zzz nonexistus",
        Spelling = "Probably_incorrect",
        Suggested_name = NA_character_,
        Distance = NA_real_,
        taxonomicStatus = NA_character_,
        stringsAsFactors = FALSE
    )
    got <- br_map_check_result(raw, "florabr")
    expect_equal(got$scientificName, "Zzz nonexistus")
})

test_that("the real base applies its fuzzy suggestion end to end", {
    skip_if_not_installed("florabr")
    p <- florabr_provider()
    skip_if_not(p$is_available(), "florabr base not embedded (run data-raw/prep_florabr.R)")

    res <- p$query("Cedrela fissillis")
    expect_equal(res$scientificName, "Cedrela fissilis")
})

test_that("an unresolved name carries no taxonomic status", {
    p <- faunabr_provider()
    skip_if_not(p$is_available(), "faunabr base not embedded (run data-raw/prep_faunabr.R)")

    # Same root cause as the OOM, and the reason it was worth fixing beyond memory:
    # the NA-key join let an unresolved name inherit `taxonomicStatus` from whatever
    # supra-specific row it crossed with, so build_dwc_output() — which fills
    # taxonomy for every matched row, resolved or not — shipped "valid" to the
    # exported Darwin Core for names nothing had actually validated.
    res <- p$query("Zzz nonexistus")
    expect_equal(res$validation_status, "not_found")
    expect_true(is.na(res$taxonomicStatus))
})
