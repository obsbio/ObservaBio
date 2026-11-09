# The results table has escape = FALSE for the app badges. Every value from
# the upload must arrive escaped (audit A05).

test_that("the family column reaches the table escaped (A05)", {
    seen <- NULL
    testthat::local_mocked_bindings(
        datatable = function(data, ...) {
            seen <<- data
            htmlwidgets::createWidget("datatables", list())
        },
        .package = "DT"
    )
    evil <- "<img src=x onerror=\"alert(1)\">"
    res <- shiny::reactiveVal(list(
        dwc = data.frame(scientificName = "Aus bus", family = evil, kingdom = "Plantae",
                         stringsAsFactors = FALSE),
        cascade = NULL, geo = NULL, record_areas = NULL
    ))
    shiny::testServer(mod_results_server, args = list(result_r = res), {
        output$table
        expect_false(is.null(seen))
        expect_false(grepl("<img", seen$Família, fixed = TRUE))
        expect_match(seen$Família, "&lt;img", fixed = TRUE)
    })
})
