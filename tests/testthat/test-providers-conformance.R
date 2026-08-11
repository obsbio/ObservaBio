# Provider conformance (SPEC §16.4): every registered provider must satisfy the
# contract. Behavioral (real-query) checks for the BR providers require the
# embedded bases and for GBIF require the network, so here we assert the
# structural contract + empty-input behavior, which hold offline. Real-query
# conformance runs once the bases are prepared (data-raw/prep_*.R).

test_that("the default providers are registered in cascade order", {
    providers <- get_providers()
    ids <- vapply(providers, function(p) p$id, character(1))
    expect_true(all(c("florabr", "faunabr", "gbif") %in% ids))
    # florabr (1) before faunabr (2) before gbif (3)
    expect_equal(ids[match(c("florabr", "faunabr", "gbif"), ids)], c("florabr", "faunabr", "gbif"))
})

test_that("each registered provider conforms to the contract", {
    for (provider in get_providers()) {
        expect_valid_provider(provider)
    }
})

test_that("the embedded BR providers pass behavioral conformance offline", {
    florabr <- florabr_provider()
    skip_if_not(florabr$is_available(), "florabr base not embedded (run data-raw/prep_florabr.R)")
    expect_valid_provider(florabr, sample_names = c("Handroanthus impetiginosus", "Zzz zzz"))

    faunabr <- faunabr_provider()
    skip_if_not(faunabr$is_available(), "faunabr base not embedded (run data-raw/prep_faunabr.R)")
    expect_valid_provider(faunabr, sample_names = c("Panthera onca", "Zzz zzz"))
})

test_that("Flora BR resolves an accepted plant name", {
    florabr <- florabr_provider()
    skip_if_not(florabr$is_available(), "florabr base not embedded")
    res <- florabr$query("Handroanthus impetiginosus")
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "accepted")
})

test_that("a fake provider passes behavioral conformance", {
    fake <- make_fake_provider("fake", 1L, lookup = list(
        "Panthera onca" = list(validation_status = "accepted", scientificName = "Panthera onca")
    ))
    expect_valid_provider(fake, sample_names = c("Panthera onca", "Nao existe"))
})

test_that("register_provider rejects non-providers and get_providers sorts by priority", {
    expect_error(register_provider(list(id = "x")), "validabio_provider")

    old <- get_providers()
    on.exit({
        clear_providers()
        for (p in old) register_provider(p)
    })
    clear_providers()
    register_provider(make_fake_provider("c", 3L))
    register_provider(make_fake_provider("a", 1L))
    register_provider(make_fake_provider("b", 2L))
    ids <- vapply(get_providers(), function(p) p$id, character(1))
    expect_equal(ids, c("a", "b", "c"))
})
