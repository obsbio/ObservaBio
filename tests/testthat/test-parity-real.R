# Opt-in real-data parity (SPEC §11). The client spreadsheet is NOT committed
# (privacy — .gitignore); this test skips unless the file is present locally, so
# CI stays green. When present, it asserts the standardization *structure* the
# reference script guarantees on the real 30-column / double-header model:
# one row per record, model column order preserved, already-validated rows kept.
# Full taxonomy-value parity against the reference OUTPUT is a manual step (needs
# that output file + the live cascade).

find_real_xlsx <- function() {
    candidates <- c(
        Sys.getenv("OBSERVABIO_PARITY_XLSX", ""),
        "Biodiversidade Darwincore 25_06.xlsx",
        file.path("..", "..", "Biodiversidade Darwincore 25_06.xlsx"),
        testthat::test_path("..", "..", "Biodiversidade Darwincore 25_06.xlsx")
    )
    candidates <- candidates[nzchar(candidates)]
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0L) NA_character_ else hit[[1]]
}

test_that("real ObservaBio spreadsheet preserves the standardization structure", {
    path <- find_real_xlsx()
    skip_if(is.na(path), "real ObservaBio spreadsheet not present (opt-in fixture)")

    parsed <- read_ObservaBio_table(path)

    # The real model carries the conservation-status columns (scope note).
    expect_true(all(
        c("scientificName", "status", "statusSource", "statusIUCN", "criteria")
        %in% parsed$model_cols
    ))

    prefilled <- dwc_prefilled_mask(parsed$records)
    expect_gt(sum(prefilled), 0L)   # already-validated rows exist
    expect_gt(sum(!prefilled), 0L)  # new rows exist

    # Empty cascade: every record kept, model order preserved, validated rows
    # untouched. This is the structural equivalence, exercised offline.
    out <- build_dwc_output(parsed$records, empty_canonical_result(),
                            model_cols = parsed$model_cols)
    expect_equal(nrow(out), nrow(parsed$records))
    expect_equal(names(out), c(parsed$model_cols, "distributionFlag"))

    val_idx <- which(prefilled)[[1]]
    expect_equal(out$taxonID[[val_idx]], parsed$records$taxonID[[val_idx]])
    expect_equal(out$scientificName[[val_idx]], parsed$records$scientificName[[val_idx]])
})
