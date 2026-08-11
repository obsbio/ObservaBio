test_that("empty_canonical_result has the canonical schema and no rows", {
    empty <- empty_canonical_result()
    expect_identical(names(empty), canonical_schema_columns())
    expect_equal(nrow(empty), 0L)
})

test_that("normalize_provider_result fills missing columns and defaults", {
    raw <- data.frame(query_name = c("Aus bus", "Cus dus"),
                      stringsAsFactors = FALSE)
    out <- normalize_provider_result(raw, "florabr")

    expect_identical(names(out), canonical_schema_columns())
    expect_equal(nrow(out), 2L)
    expect_equal(out$provider, c("florabr", "florabr"))
    # scientificName falls back to the query name when blank
    expect_equal(out$scientificName, c("Aus bus", "Cus dus"))
    # validation_status defaults to not_found; match_count follows
    expect_equal(out$validation_status, c("not_found", "not_found"))
    expect_equal(out$match_count, c(0L, 0L))
})

test_that("normalize_provider_result preserves provided values", {
    raw <- data.frame(
        query_name = "Panthera onca",
        scientificName = "Panthera onca",
        validation_status = "accepted",
        family = "Felidae",
        stringsAsFactors = FALSE
    )
    out <- normalize_provider_result(raw, "gbif")
    expect_equal(out$validation_status, "accepted")
    expect_equal(out$family, "Felidae")
    expect_equal(out$match_count, 1L)
})

test_that("normalize_provider_result returns empty canonical frame for empty input", {
    out <- normalize_provider_result(data.frame(), "x")
    expect_identical(names(out), canonical_schema_columns())
    expect_equal(nrow(out), 0L)
})

test_that("normalize_provider_result requires query_name", {
    expect_error(
        normalize_provider_result(data.frame(scientificName = "x"), "p"),
        "query_name"
    )
})

test_that("new_provider validates and stamps class", {
    p <- new_provider("p", "Prov", "test", 1L, query = function(names, data = NULL) empty_canonical_result())
    expect_s3_class(p, "validabio_provider")
    expect_equal(p$priority, 1L)
    expect_error(new_provider("p", "Prov", "test", 1L, query = "notafun"), "must be a function")
})
