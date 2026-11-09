# mod_process_server keeps a result for one upload only (audit A07). The
# network phases are replaced with offline stubs.

stub_phases <- function(env = parent.frame()) {
    testthat::local_mocked_bindings(
        run_cascade = function(names, ...) empty_canonical_result(),
        resolve_threat_status = function(df, ...) df,
        run_geo_verification = function(areas, ...) NULL,
        .env = env
    )
}

upload_of <- function(name) {
    records <- data.frame(scientificName = name, kingdom = "Plantae",
                          stringsAsFactors = FALSE)
    list(records = records, model_cols = names(records), pt_labels = NULL,
         areas = NULL)
}

test_that("a new upload clears the previous result (A07)", {
    stub_phases()
    up <- shiny::reactiveVal(upload_of("Paubrasilia echinata"))
    shiny::testServer(mod_process_server, args = list(upload_r = up), {
        session$setInputs(processar = 1)
        res <- session$returned()
        expect_false(is.null(res))
        expect_equal(res$dwc$scientificName, "Paubrasilia echinata")

        up(upload_of("Araucaria angustifolia"))
        session$flushReact()
        expect_null(session$returned())
    })
})

test_that("the same upload keeps the result, and a failed run clears it (A07)", {
    stub_phases()
    up <- shiny::reactiveVal(upload_of("Paubrasilia echinata"))
    shiny::testServer(mod_process_server, args = list(upload_r = up), {
        session$setInputs(processar = 1)
        up(upload_of("Paubrasilia echinata"))
        session$flushReact()
        expect_false(is.null(session$returned()))

        testthat::local_mocked_bindings(
            run_cascade = function(names, ...) stop("offline")
        )
        session$setInputs(processar = 2)
        expect_null(session$returned())
    })
})

test_that("an all-prefilled upload processes and updates the MMA status (A02, A03)", {
    stub_phases()
    up <- shiny::reactiveVal(list(
        records = data.frame(scientificName = "Paubrasilia echinata", kingdom = "Plantae",
                             status = "(EN) Em Perigo", statusSource = "Portaria 148/2022",
                             stringsAsFactors = FALSE),
        model_cols = c("scientificName", "kingdom", "status", "statusSource"),
        pt_labels = NULL, areas = NULL
    ))
    shiny::testServer(mod_process_server, args = list(upload_r = up), {
        session$setInputs(processar = 1)
        res <- session$returned()
        expect_false(is.null(res))
        expect_equal(res$dwc$status, "(CR) Criticamente Em Perigo")
        expect_equal(res$dwc$statusSource, "Portaria 1.783/2026")
        expect_equal(nrow(res$audit), 0L)
    })
})

test_that("a prefilled name with the author gets the MMA status (R01)", {
    stub_phases()
    sci <- c("Paubrasilia echinata (Lam.) Gagnon, H.C.Lima & G.P.Lewis",
             "Almeidea coerulea A.St.-Hil.")
    up <- shiny::reactiveVal(list(
        records = data.frame(scientificName = sci, kingdom = "Plantae",
                             status = "(EN) Em Perigo", statusSource = "Portaria 148/2022",
                             stringsAsFactors = FALSE),
        model_cols = c("scientificName", "kingdom", "status", "statusSource"),
        pt_labels = NULL, areas = NULL
    ))
    shiny::testServer(mod_process_server, args = list(upload_r = up), {
        session$setInputs(processar = 1)
        res <- session$returned()
        expect_equal(res$dwc$scientificName, sci)
        expect_equal(res$dwc$status, c("(CR) Criticamente Em Perigo", "(EN) Em Perigo"))
        expect_equal(res$dwc$statusSource[[1]], "Portaria 1.783/2026")
        expect_match(res$dwc$statusSource[[2]], "nome aceito")
    })
})
