# Test helpers: a fake provider factory and the provider conformance check.

#' Build a fake provider backed by an in-memory lookup, for cascade/contract tests
#'
#' @param id,priority Provider identity/order.
#' @param lookup Named list mapping query_name -> a partial canonical row
#'   (list of column=value). Names not present resolve to `not_found`.
#' @param available Whether `is_available()` returns TRUE.
#' @param fail When TRUE, `query()` throws (to test cascade resilience).
#' @param exact Names this fake base carries verbatim. `NULL` (default) leaves the
#'   optional `exact_match` slot unset, i.e. the base opts out of the pre-pass.
#' @param seen Optional environment; `query()` records the names it was actually
#'   asked for under `seen[[id]]`, so tests can assert what the pre-pass held back.
make_fake_provider <- function(id, priority, lookup = list(),
                               available = TRUE, fail = FALSE,
                               exact = NULL, seen = NULL) {
    new_provider(
        id = id,
        label = id,
        type = "test",
        priority = priority,
        is_available = function() available,
        exact_match = if (is.null(exact)) {
            NULL
        } else {
            function(names, data = NULL) intersect(as.character(names), exact)
        },
        query = function(names, data = NULL) {
            if (!is.null(seen)) {
                seen[[id]] <- as.character(names)
            }
            if (fail) stop("boom")
            names <- unique(as.character(names))
            names <- names[!is.na(names) & nzchar(names)]
            if (length(names) == 0L) return(empty_canonical_result())
            # Normalize each name's row to the canonical schema before stacking,
            # so rows with different provided columns rbind cleanly.
            rows <- lapply(names, function(nm) {
                base <- list(query_name = nm)
                if (!is.null(lookup[[nm]])) {
                    base <- utils::modifyList(base, lookup[[nm]])
                } else {
                    base$validation_status <- "not_found"
                }
                normalize_provider_result(as.data.frame(base, stringsAsFactors = FALSE), id)
            })
            do.call(rbind, rows)
        }
    )
}

#' Assert a provider conforms to the contract (SPEC §16.4)
#'
#' Structural checks always run. Behavioral checks (real query) run only when
#' `sample_names` is given and the provider is available, so this stays green
#' offline / before embedded bases are prepared.
expect_valid_provider <- function(provider, sample_names = NULL) {
    testthat::expect_s3_class(provider, "ObservaBio_provider")
    for (field in c("id", "label", "type", "priority")) {
        testthat::expect_true(nzchar(as.character(provider[[field]])),
                              info = paste("missing field:", field))
    }
    for (fn in c("query", "is_available", "load", "version")) {
        testthat::expect_true(is.function(provider[[fn]]),
                              info = paste("not a function:", fn))
    }

    # Empty input must yield a zero-row canonical frame.
    empty <- provider$query(character(0))
    testthat::expect_true(is.data.frame(empty))
    testthat::expect_identical(names(empty), canonical_schema_columns())
    testthat::expect_equal(nrow(empty), 0L)

    if (!is.null(sample_names) && isTRUE(provider$is_available())) {
        res <- provider$query(sample_names)
        testthat::expect_true(is.data.frame(res))
        testthat::expect_true(all(canonical_schema_columns() %in% names(res)))
        if (nrow(res) > 0L) {
            testthat::expect_true(all(res$query_name %in% sample_names))
            testthat::expect_true(all(res$validation_status %in%
                c("accepted", "synonym", "ambiguous", "not_found")))
        }
    }

    # The distribution() slot is optional; when present it must honor its shape:
    # empty input -> zero-row query_name/states/biomes frame, and a real lookup
    # keys its rows to the requested names.
    if (is.function(provider$distribution)) {
        empty_dist <- provider$distribution(character(0))
        testthat::expect_true(is.data.frame(empty_dist))
        testthat::expect_identical(names(empty_dist), c("query_name", "states", "biomes"))
        testthat::expect_equal(nrow(empty_dist), 0L)
        if (!is.null(sample_names) && isTRUE(provider$is_available())) {
            dist <- provider$distribution(sample_names)
            testthat::expect_identical(names(dist), c("query_name", "states", "biomes"))
            testthat::expect_true(all(dist$query_name %in% sample_names))
        }
    }

    # The exact_match() slot is optional too; when present it must answer with a
    # subset of what it was asked, so run_cascade() can trust it to hold names back.
    if (is.function(provider$exact_match)) {
        testthat::expect_length(provider$exact_match(character(0)), 0L)
        if (!is.null(sample_names) && isTRUE(provider$is_available())) {
            hits <- provider$exact_match(sample_names)
            testthat::expect_true(is.character(hits))
            testthat::expect_true(all(hits %in% sample_names))
        }
    }
    invisible(TRUE)
}
