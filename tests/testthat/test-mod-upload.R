# The Step-1 reactive bridge. Pure-function tests cannot see this layer, and the
# locality mapping panel is built inside a renderUI — where a missing `ns` only
# blows up at render time. These drive the module far enough to render it.

skip_if_not_installed("zip")

fake_shapefile_zip <- function() {
    src <- tempfile("shpsrc")
    dir.create(src)
    comps <- file.path(src, paste0("area.", c("shp", "shx", "dbf", "prj")))
    for (f in comps) writeLines("x", f)
    z <- tempfile(fileext = ".zip")
    zip::zipr(z, comps)
    z
}

sheet_csv <- function(locality = TRUE) {
    df <- data.frame(scientificName = c("Cedrela fissilis", "Panthera onca"),
                     stringsAsFactors = FALSE)
    if (locality) {
        df$locality <- c("RPPN Rio do Brasil", "Fazenda Trijuncao")
    }
    path <- tempfile(fileext = ".csv")
    utils::write.csv(df, path, row.names = FALSE)
    path
}

# Shiny hands fileInput a data frame; multiple files are multiple rows.
file_input <- function(paths, names) {
    data.frame(name = names, size = file.size(paths), type = "",
               datapath = paths, stringsAsFactors = FALSE)
}

test_that("the locality mapping panel renders one selector per uploaded area", {
    zips <- c(fake_shapefile_zip(), fake_shapefile_zip())
    csv <- sheet_csv()
    on.exit(unlink(c(zips, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(
            planilha = file_input(csv, "planilha.csv"),
            shapefile = file_input(zips, c("RPPN Rio do Brasil.zip",
                                           "Fazenda Trijuncao.zip"))
        )
        html <- output$area_mapping$html
        # One input id per area, keyed by slug (an index would shift under the
        # user when an earlier area is removed).
        expect_match(html, "area_loc_rppn_rio_do_brasil", fixed = TRUE)
        expect_match(html, "area_loc_fazenda_trijuncao", fixed = TRUE)
        # Both localities are offered as choices, and both areas are named.
        expect_match(html, "RPPN Rio do Brasil", fixed = TRUE)
        expect_match(html, "Fazenda Trijuncao", fixed = TRUE)
        expect_match(output$shapefile_status$html, "2 áreas válidas", fixed = TRUE)
    })
})

test_that("the upload payload carries the areas with the links the user made", {
    zips <- c(fake_shapefile_zip(), fake_shapefile_zip())
    csv <- sheet_csv()
    on.exit(unlink(c(zips, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(
            planilha = file_input(csv, "planilha.csv"),
            shapefile = file_input(zips, c("RPPN Rio do Brasil.zip",
                                           "Fazenda Trijuncao.zip")),
            # In the browser the selectize suggestions come back on their own.
            area_loc_rppn_rio_do_brasil = "RPPN Rio do Brasil",
            area_loc_fazenda_trijuncao = "Fazenda Trijuncao"
        )
        up <- session$getReturned()$data()
        expect_length(up$areas, 2L)
        expect_equal(vapply(up$areas, function(a) a$name, character(1)),
                     c("RPPN Rio do Brasil", "Fazenda Trijuncao"))
        expect_equal(up$areas[[1L]]$localities, "RPPN Rio do Brasil")
        # And the link resolves all the way to a per-record area.
        expect_equal(assign_record_areas(up$records, up$areas),
                     c("RPPN Rio do Brasil", "Fazenda Trijuncao"))
    })
})

test_that("areas sent in two separate uploads accumulate", {
    # The regression: fileInput replaces its selection on every upload, so the
    # second area used to silently erase the first ("1 área válida").
    zips <- c(fake_shapefile_zip(), fake_shapefile_zip())
    csv <- sheet_csv()
    on.exit(unlink(c(zips, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(planilha = file_input(csv, "planilha.csv"))
        session$setInputs(
            shapefile = file_input(zips[1], "RPPN Rio do Brasil.zip")
        )
        expect_match(output$shapefile_status$html, "1 área válida", fixed = TRUE)

        session$setInputs(
            shapefile = file_input(zips[2], "Fazenda Trijuncao.zip")
        )
        expect_match(output$shapefile_status$html, "2 áreas válidas", fixed = TRUE)
        expect_equal(
            vapply(session$getReturned()$data()$areas, function(a) a$name,
                   character(1)),
            c("RPPN Rio do Brasil", "Fazenda Trijuncao")
        )
    })
})

test_that("removing an area drops only that one, and the last leaves no panel", {
    zips <- c(fake_shapefile_zip(), fake_shapefile_zip())
    csv <- sheet_csv()
    on.exit(unlink(c(zips, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(
            planilha = file_input(csv, "planilha.csv"),
            shapefile = file_input(zips, c("RPPN Rio do Brasil.zip",
                                           "Fazenda Trijuncao.zip"))
        )
        session$setInputs(area_remove = "rppn_rio_do_brasil")
        areas <- session$getReturned()$data()$areas
        expect_length(areas, 1L)
        expect_equal(areas[[1L]]$name, "Fazenda Trijuncao")

        # Dropping the last one returns to the "no shapefile" state, even though
        # input$shapefile still holds its files.
        session$setInputs(area_remove = "fazenda_trijuncao")
        expect_null(session$getReturned()$data()$areas)
        expect_null(output$area_mapping)
        expect_match(output$shapefile_status$html, "fica limitada", fixed = TRUE)
    })
})

test_that("selections already made survive a later area being added", {
    zips <- c(fake_shapefile_zip(), fake_shapefile_zip())
    csv <- sheet_csv()
    on.exit(unlink(c(zips, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(planilha = file_input(csv, "planilha.csv"))
        session$setInputs(shapefile = file_input(zips[1], "Area A.zip"))
        # The user links area A to a locality whose name does not match it.
        session$setInputs(area_loc_area_a = "Fazenda Trijuncao")

        session$setInputs(shapefile = file_input(zips[2], "Area B.zip"))
        # Re-rendering the panel must not reset the choice already made.
        expect_match(output$area_mapping$html, "Fazenda Trijuncao", fixed = TRUE)
        expect_equal(session$getReturned()$data()$areas[[1L]]$localities,
                     "Fazenda Trijuncao")
    })
})

test_that("a sheet without locality gets the fallback note, not selectors", {
    zip_path <- fake_shapefile_zip()
    csv <- sheet_csv(locality = FALSE)
    on.exit(unlink(c(zip_path, csv)), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(
            planilha = file_input(csv, "planilha.csv"),
            shapefile = file_input(zip_path, "Area unica.zip")
        )
        html <- output$area_mapping$html
        expect_no_match(html, "area_loc_", fixed = TRUE)
        expect_match(html, "vale para todos os registros", fixed = TRUE)

        # The single area still claims every record (the fallback).
        up <- session$getReturned()$data()
        expect_equal(assign_record_areas(up$records, up$areas),
                     rep("Area unica", 2L))
    })
})

test_that("no shapefile leaves the panel empty and the payload areas NULL", {
    csv <- sheet_csv()
    on.exit(unlink(csv), add = TRUE)

    shiny::testServer(mod_upload_server, {
        session$setInputs(planilha = file_input(csv, "planilha.csv"))
        expect_null(output$area_mapping)
        expect_null(session$getReturned()$data()$areas)
    })
})
