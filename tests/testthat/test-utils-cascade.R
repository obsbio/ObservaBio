test_that("normalize_scientific_name strips authors and qualifiers", {
    expect_equal(normalize_scientific_name("Panthera onca (Linnaeus, 1771)"), "Panthera onca")
    expect_equal(normalize_scientific_name("handroanthus impetiginosus"), "Handroanthus impetiginosus")
    expect_equal(normalize_scientific_name("Cedrela cf. fissilis"), "Cedrela fissilis")
    # Genus + sp. marker collapses to NA (no resolvable epithet)
    expect_true(is.na(normalize_scientific_name("Inga sp.")))
    expect_true(is.na(normalize_scientific_name("")))
    expect_true(is.na(normalize_scientific_name(NA)))
})

test_that("normalize_scientific_name keeps infraspecific rank tokens", {
    expect_equal(
        normalize_scientific_name("Aus bus var. cus (Author, 1900)"),
        "Aus bus var. cus"
    )
})

test_that("normalize_scientific_name strips lowercase author particles", {
    # Capitalized author tokens are dropped, but the particle in "de Wit" is
    # lowercase and used to survive as if it were an epithet.
    expect_equal(
        normalize_scientific_name("Leucaena leucocephala (Lam.) de Wit"),
        "Leucaena leucocephala"
    )
    expect_equal(
        normalize_scientific_name("Phyllorhiza punctata von Lendenfeld, 1884"),
        "Phyllorhiza punctata"
    )
    expect_equal(
        normalize_scientific_name("Sorghum bicolor subsp. arundinaceum (Desv.) de Wet & Harlan"),
        "Sorghum bicolor subsp. arundinaceum"
    )
})

test_that("normalize_scientific_name drops a dangling rank marker (author filius)", {
    # "Hallier f." is the author, not the forma rank: the marker names nothing.
    expect_equal(
        normalize_scientific_name("Ipomoea neurocephala Hallier f."),
        "Ipomoea neurocephala"
    )
    # ...but a rank marker that does name an epithet is still kept.
    expect_equal(
        normalize_scientific_name("Malva sylvestris f. sylvestris"),
        "Malva sylvestris f. sylvestris"
    )
})

test_that("scientific_name_components derives genus/epithet/rank from the name", {
    got <- scientific_name_components(c(
        "Panthera onca",          # binomial -> species
        "Cedrela cf. odorata",    # qualifier -> epithet after the marker
        "Inga sp.",               # placeholder -> genus rank, no epithet
        "Handroanthus",           # genus only
        NA                        # blank -> all NA
    ))
    expect_equal(got$genus, c("Panthera", "Cedrela", "Inga", "Handroanthus", NA))
    expect_equal(got$specificEpithet, c("onca", "odorata", NA, NA, NA))
    expect_equal(got$taxonRank, c("species", "species", "genus", "genus", NA))
})

test_that("scientific_name_components normalizes case and never yields 'cf' as epithet", {
    got <- scientific_name_components(c("panthera ONCA", "Panthera cf."))
    expect_equal(got$genus, c("Panthera", "Panthera"))
    expect_equal(got$specificEpithet, c("onca", NA))
    expect_equal(got$taxonRank, c("species", "genus"))
})

test_that("normalize_provider_result fills genus/specificEpithet/taxonRank from the accepted name", {
    # A BR-style provider result: verdict + family only, no parsed parts.
    df <- data.frame(
        query_name = "Hoplias malabaricus",
        scientificName = "Hoplias malabaricus",
        validation_status = "accepted",
        family = "Erythrinidae",
        stringsAsFactors = FALSE
    )
    out <- normalize_provider_result(df, "faunabr")
    expect_equal(out$genus, "Hoplias")
    expect_equal(out$specificEpithet, "malabaricus")
    expect_equal(out$taxonRank, "species")
})

test_that("normalize_provider_result never overwrites parts the base did supply", {
    df <- data.frame(
        query_name = "Panthera onca",
        scientificName = "Panthera onca",
        validation_status = "accepted",
        genus = "Panthera", specificEpithet = "onca", taxonRank = "Species",
        stringsAsFactors = FALSE
    )
    out <- normalize_provider_result(df, "gbif")
    expect_equal(out$taxonRank, "Species")   # base value kept, not "species"
})

test_that("has_name_qualifier detects cf./aff./sp. markers, including pipe-joined", {
    expect_equal(
        has_name_qualifier(c("Cedrela cf. odorata", "Inga sp.", "Panthera onca")),
        c(TRUE, TRUE, FALSE)
    )
    # originalName can pipe-join several raw inputs; any qualifier counts.
    expect_true(has_name_qualifier("Panthera onca | Panthera aff. onca"))
    expect_false(has_name_qualifier(NA))
})

test_that("cascade status ranking orders accepted > synonym > ambiguous > not_found", {
    expect_equal(cascade_status_rank("accepted"), 4L)
    expect_equal(cascade_status_rank("synonym"), 3L)
    expect_equal(cascade_status_rank("ambiguous"), 2L)
    expect_equal(cascade_status_rank("not_found"), 1L)
    expect_true(should_replace_cascade_row("synonym", "accepted"))
    expect_false(should_replace_cascade_row("accepted", "not_found"))
})

test_that("collapse_cascade_results keeps the best row per name", {
    stacked <- rbind(
        normalize_provider_result(
            data.frame(query_name = "Aus bus", validation_status = "synonym",
                       provider = "florabr", stringsAsFactors = FALSE), "florabr"),
        normalize_provider_result(
            data.frame(query_name = "Aus bus", validation_status = "accepted",
                       provider = "gbif", stringsAsFactors = FALSE), "gbif")
    )
    collapsed <- collapse_cascade_results(stacked)
    expect_equal(nrow(collapsed), 1L)
    expect_equal(collapsed$validation_status, "accepted")
    expect_equal(collapsed$provider, "gbif")
})

test_that("run_cascade short-circuits on accepted and dedups input", {
    florabr <- make_fake_provider("florabr", 1L, lookup = list(
        "Panthera onca" = list(validation_status = "accepted", scientificName = "Panthera onca")
    ))
    gbif <- make_fake_provider("gbif", 3L, lookup = list(
        "Handroanthus impetiginosus" = list(validation_status = "accepted")
    ))

    res <- run_cascade(
        c("Panthera onca", "panthera onca (Linnaeus)", "Handroanthus impetiginosus"),
        providers = list(gbif, florabr)
    )
    # Two unique normalized names
    expect_equal(nrow(res), 2L)
    onca <- res[res$query_name == "Panthera onca", ]
    expect_equal(onca$provider, "florabr")
    expect_equal(onca$validation_status, "accepted")
})

test_that("run_cascade carries synonyms forward to the next provider", {
    florabr <- make_fake_provider("florabr", 1L, lookup = list(
        "Aus bus" = list(validation_status = "synonym", scientificName = "Aus bus")
    ))
    gbif <- make_fake_provider("gbif", 3L, lookup = list(
        "Aus bus" = list(validation_status = "accepted", scientificName = "Xus yus")
    ))
    res <- run_cascade("Aus bus", providers = list(florabr, gbif))
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "accepted")
    expect_equal(res$provider, "gbif")
})

test_that("run_cascade is resilient to a failing provider", {
    boom <- make_fake_provider("boom", 1L, fail = TRUE)
    gbif <- make_fake_provider("gbif", 3L, lookup = list(
        "Aus bus" = list(validation_status = "accepted")
    ))
    res <- run_cascade("Aus bus", providers = list(boom, gbif))
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "accepted")
    failures <- attr(res, "provider_failures")
    expect_true("boom" %in% failures$provider)
})

test_that("run_cascade marks unresolved names as not_found", {
    florabr <- make_fake_provider("florabr", 1L, lookup = list())
    res <- run_cascade("Unknownus taxus", providers = list(florabr))
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "not_found")
})

test_that("run_cascade returns an empty canonical frame for empty input", {
    res <- run_cascade(character(0), providers = list())
    expect_identical(names(res), canonical_schema_columns())
    expect_equal(nrow(res), 0L)
})

# ---- exact-match pre-pass ---------------------------------------------------
# A name a lower-priority base carries verbatim should not pay the higher-priority
# base's fuzzy pass first (that is the ~0.45 s/name agrep in check_names()).

test_that("run_cascade holds a name back from a base that does not carry it", {
    seen <- new.env(parent = emptyenv())
    florabr <- make_fake_provider(
        "florabr", 1L, exact = "Cedrela odorata", seen = seen,
        lookup = list("Cedrela odorata" = list(validation_status = "accepted"))
    )
    faunabr <- make_fake_provider(
        "faunabr", 2L, exact = "Panthera onca", seen = seen,
        lookup = list("Panthera onca" = list(validation_status = "accepted"))
    )

    res <- run_cascade(c("Cedrela odorata", "Panthera onca"),
                       providers = list(florabr, faunabr))

    # The animal name never reaches florabr's matcher...
    expect_equal(seen$florabr, "Cedrela odorata")
    expect_true("Panthera onca" %in% seen$faunabr)
    # ...and both still resolve, each from the base that owns it.
    expect_equal(res$provider[res$query_name == "Panthera onca"], "faunabr")
    expect_equal(res$provider[res$query_name == "Cedrela odorata"], "florabr")
})

test_that("run_cascade skips the fuzzy fallback once something cheaper resolves a name", {
    seen <- new.env(parent = emptyenv())
    florabr <- make_fake_provider("florabr", 1L, exact = "Cedrela odorata", seen = seen)
    faunabr <- make_fake_provider("faunabr", 2L, exact = "Panthera onca", seen = seen)
    gbif <- make_fake_provider(
        "gbif", 3L, seen = seen,
        lookup = list("Zzz nonexistus" = list(validation_status = "accepted"))
    )

    res <- run_cascade("Zzz nonexistus", providers = list(florabr, faunabr, gbif))

    # Neither base holds it verbatim, so pass 1 never asks them. gbif publishes no
    # exact index, so it is asked everything, and its `accepted` settles the name
    # before the ~0.38 s/name agrep in pass 2 can run (ADR-023).
    expect_null(seen$florabr)
    expect_null(seen$faunabr)
    expect_equal(seen$gbif, "Zzz nonexistus")
    expect_equal(res$provider, "gbif")
    expect_equal(res$validation_status, "accepted")
})

test_that("run_cascade still offers every base its fuzzy pass when nothing resolves a name", {
    seen <- new.env(parent = emptyenv())
    florabr <- make_fake_provider("florabr", 1L, exact = "Cedrela odorata", seen = seen)
    faunabr <- make_fake_provider("faunabr", 2L, exact = "Panthera onca", seen = seen)
    gbif <- make_fake_provider("gbif", 3L, seen = seen)

    res <- run_cascade("Zzz nonexistus", providers = list(florabr, faunabr, gbif))

    # Deferring the fuzzy pass is not dropping it: with nothing cheaper to settle
    # the name, the full fallback chain still runs.
    expect_equal(seen$florabr, "Zzz nonexistus")
    expect_equal(seen$faunabr, "Zzz nonexistus")
    expect_equal(res$validation_status, "not_found")
})

test_that("a pass 2 row does not outrank a later provider's pass 1 row on a tie", {
    # collapse_cascade_results() breaks ties with the last row, so the stacking
    # order has to stay priority order even though pass 2 runs after pass 1.
    florabr <- make_fake_provider(
        "florabr", 1L, exact = "Something else",
        lookup = list("Aus bus" = list(validation_status = "ambiguous"))
    )
    gbif <- make_fake_provider(
        "gbif", 3L,
        lookup = list("Aus bus" = list(validation_status = "ambiguous"))
    )

    res <- run_cascade("Aus bus", providers = list(florabr, gbif))

    # gbif answered in pass 1 and florabr in pass 2, but gbif is the lower-priority
    # fallback, so it must still win the tie exactly as it did before the split.
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "ambiguous")
    expect_equal(res$provider, "gbif")
})

test_that("a base without exact_match keeps being queried with everything", {
    # Contract guarantee: opting out of the pre-pass must not cost a base its
    # priority, or "add a base in one line" would stop being true.
    seen <- new.env(parent = emptyenv())
    newbase <- make_fake_provider("newbase", 1L, seen = seen)
    faunabr <- make_fake_provider(
        "faunabr", 2L, exact = "Panthera onca", seen = seen,
        lookup = list("Panthera onca" = list(validation_status = "accepted"))
    )

    run_cascade(c("Aus bus", "Panthera onca"), providers = list(newbase, faunabr))
    expect_setequal(seen$newbase, c("Aus bus", "Panthera onca"))
})
