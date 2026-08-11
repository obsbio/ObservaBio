# The copy-me template (R/provider_template.R) must itself satisfy the provider
# contract, so anyone who copies it starts from a conforming baseline. Unlike the
# BR providers, its fixture is self-contained, so the behavioral checks run offline.

test_that("the template provider passes structural + behavioral conformance", {
    expect_valid_provider(
        template_provider(),
        sample_names = c("Examplia ficta", "Zzz zzz")
    )
})

test_that("the template resolves an accepted name and its distribution", {
    res <- template_provider()$query("Examplia ficta")
    expect_equal(nrow(res), 1L)
    expect_equal(res$validation_status, "accepted")
    expect_equal(res$scientificName, "Examplia ficta")

    dist <- template_provider()$distribution("Examplia ficta")
    expect_identical(names(dist), c("query_name", "states", "biomes"))
    expect_equal(dist$states, "SP;RJ;MG")
})

test_that("the template maps a synonym to its accepted name", {
    res <- template_provider()$query("Examplia synonyma")
    expect_equal(res$validation_status, "synonym")
    expect_equal(res$scientificName, "Examplia ficta")
})

test_that("an unknown name degrades to not_found, empty input to zero rows", {
    res <- template_provider()$query("Nao existe")
    expect_equal(res$validation_status, "not_found")

    empty <- template_provider()$query(character(0))
    expect_identical(names(empty), canonical_schema_columns())
    expect_equal(nrow(empty), 0L)
})

test_that("the template is NOT registered in the default cascade", {
    ids <- vapply(get_providers(), function(p) p$id, character(1))
    expect_false("template" %in% ids)
})
