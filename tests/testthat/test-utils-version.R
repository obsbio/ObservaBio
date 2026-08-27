# The version each embedded base reports. This is what the team reads on screen
# after the annual base update (docs/atualizacao_bases.md) to confirm the new
# data went live, so a base that cannot report must degrade visibly, never
# silently.

test_that("read_base_meta() reads the sidecar next to an RDS", {
    dir <- withr::local_tempdir()
    rds <- file.path(dir, "fake.rds")
    saveRDS(1, rds)
    jsonlite::write_json(
        list(version = "9.9", date = "2026-01-31", source = "test"),
        sub("\\.rds$", ".meta.json", rds), auto_unbox = TRUE
    )

    meta <- read_base_meta(rds)
    expect_equal(meta$version, "9.9")
    expect_equal(meta$date, "2026-01-31")
})

test_that("read_base_meta() degrades to NA when the sidecar is absent", {
    dir <- withr::local_tempdir()
    meta <- read_base_meta(file.path(dir, "absent.rds"))
    expect_true(is.na(meta$version))
    expect_true(is.na(meta$date))
})

test_that("read_base_meta() degrades to NA when the sidecar is not JSON", {
    dir <- withr::local_tempdir()
    rds <- file.path(dir, "fake.rds")
    writeLines("{ this is not json", sub("\\.rds$", ".meta.json", rds))

    meta <- read_base_meta(rds)
    expect_true(is.na(meta$version))
    expect_true(is.na(meta$date))
})

fake_versioned_provider <- function(id, version_fn) {
    new_provider(id = id, label = id, type = "test", priority = 1L,
                 query = function(names, data = NULL) empty_canonical_result(),
                 version = version_fn)
}

test_that("reference_bases_versions() reports one row per provider, label first", {
    out <- reference_bases_versions(list(
        fake_versioned_provider("a", function() "1.0"),
        fake_versioned_provider("b", function() "2.0")
    ))

    expect_true(all(c("label", "version") %in% names(out)))
    expect_equal(out$label[1:2], c("a", "b"))
    expect_equal(out$version[1:2], c("1.0", "2.0"))
})

test_that("reference_bases_versions() appends the bases that are not providers", {
    out <- reference_bases_versions(list())

    # The UF/biome layer, the invasive lists and the MMA list never join the
    # cascade registry, so they can only come from their sidecars.
    expect_setequal(
        out$label,
        c("Estados e biomas (IBGE)", "Espécies exóticas invasoras",
          "Lista de ameaçadas (MMA)")
    )
    expect_false(any(is.na(out$version)))
})

test_that("a provider that cannot report its version shows a dash, not NA", {
    out <- reference_bases_versions(list(
        fake_versioned_provider("throws", function() stop("boom")),
        fake_versioned_provider("empty", function() character(0)),
        fake_versioned_provider("blank", function() "   ")
    ))

    expect_equal(out$version[1:3], rep("—", 3))
    expect_false(anyNA(out$version))
})
