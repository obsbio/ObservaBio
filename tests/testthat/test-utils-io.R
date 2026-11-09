# read_ObservaBio_table decodes .xlsx/.csv/.tsv/.txt and DETECTS the header row (the
# first row carrying scientificName) rather than assuming row 2. The double-header
# ObservaBio model (PT labels row 1, DwC names row 2, data row 3) must still decode.

write_double_header_fixture <- function(path) {
    frame <- data.frame(
        c1 = c("Nome cientifico", "scientificName",
               "Handroanthus impetiginosus", "Cedrela odorata", ""),
        c2 = c("Localidade", "locality", "Fazenda A", "Fazenda B", ""),
        c3 = c("", "taxonID", "", "", ""),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(frame, path, col_names = FALSE)
    path
}

# ---- .xlsx double header (regression) ---------------------------------------

test_that("read_ObservaBio_table reads DwC names from row 2 and data from row 3", {
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    write_double_header_fixture(path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols, c("scientificName", "locality", "taxonID"))
    expect_equal(parsed$pt_labels[1:2], c("Nome cientifico", "Localidade"))
    expect_equal(parsed$records$scientificName,
                 c("Handroanthus impetiginosus", "Cedrela odorata"))
})

test_that("read_ObservaBio_table drops fully empty rows", {
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    write_double_header_fixture(path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(nrow(parsed$records), 2L)
})

test_that("read_ObservaBio_table errors on a missing file", {
    expect_error(read_ObservaBio_table(tempfile(fileext = ".xlsx")), "not found")
})

# ---- delimited text ---------------------------------------------------------

test_that("read_ObservaBio_table reads a single-header .csv (header on row 1)", {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c(
        "scientificName,locality,taxonID",
        "Handroanthus impetiginosus,Fazenda A,",
        "Cedrela odorata,Fazenda B,"
    ), path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols, c("scientificName", "locality", "taxonID"))
    expect_null(parsed$pt_labels)
    expect_equal(parsed$records$scientificName,
                 c("Handroanthus impetiginosus", "Cedrela odorata"))
})

test_that("read_ObservaBio_table sniffs a semicolon separator (PT-BR export)", {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c(
        "scientificName;scientificNameAuthorship;locality",
        'Handroanthus impetiginosus;"Mart. ex DC., 1845";Fazenda A',
        "Cedrela odorata;L.;Fazenda B"
    ), path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols,
                 c("scientificName", "scientificNameAuthorship", "locality"))
    expect_equal(nrow(parsed$records), 2L)
    # The comma lives inside a quoted field, so it must not split the row.
    expect_equal(parsed$records$scientificNameAuthorship[1], "Mart. ex DC., 1845")
})

test_that("read_ObservaBio_table reads a tab-separated .tsv", {
    path <- tempfile(fileext = ".tsv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c("scientificName\tlocality", "Cichla ocellaris\tRio X"), path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols, c("scientificName", "locality"))
    expect_equal(parsed$records$scientificName, "Cichla ocellaris")
})

test_that("read_ObservaBio_table finds the header below preamble rows", {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c(
        "Inventario de Peixes,,",
        "Projeto ObservaBio,,",
        "scientificName,locality,taxonID",
        "Cichla ocellaris,Rio X,GBIF:1"
    ), path)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols, c("scientificName", "locality", "taxonID"))
    expect_equal(nrow(parsed$records), 1L)
    # The row directly above the header becomes pt_labels.
    expect_equal(parsed$pt_labels[1], "Projeto ObservaBio")
})

test_that("read_ObservaBio_table strips a UTF-8 BOM before detecting the header", {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    con <- file(path, open = "wb")
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
    writeBin(charToRaw("scientificName,locality\nCichla ocellaris,Rio X\n"), con)
    close(con)

    parsed <- read_ObservaBio_table(path)
    expect_equal(parsed$model_cols, c("scientificName", "locality"))
    expect_equal(parsed$records$scientificName, "Cichla ocellaris")
})

# ---- errors -----------------------------------------------------------------

test_that("read_ObservaBio_table errors when no header carries scientificName", {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c("especie,local", "Cichla ocellaris,Rio X"), path)
    expect_error(read_ObservaBio_table(path), "scientificName")
})

test_that("read_ObservaBio_table rejects an unsupported format", {
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path), add = TRUE)
    writeLines("x", path)
    expect_error(read_ObservaBio_table(path), "suportado")
})

# ---- Audit 2026-11-08: A13, A14 --------------------------------------------

write_lines_tmp <- function(lines, ext) {
    p <- tempfile(fileext = paste0(".", ext))
    writeLines(lines, p, useBytes = TRUE)
    p
}

test_that("commas in quoted cells do not decide the delimiter (A13)", {
    p <- write_lines_tmp(c(
        "scientificName;occurrenceRemarks",
        "Paubrasilia echinata;\"a,b,c,d,e,f\"",
        "Araucaria angustifolia;\"g,h,i,j,k,l\""
    ), "csv")
    on.exit(unlink(p), add = TRUE)
    out <- read_ObservaBio_table(p)
    expect_equal(out$model_cols, c("scientificName", "occurrenceRemarks"))
    expect_equal(out$records$occurrenceRemarks, c("a,b,c,d,e,f", "g,h,i,j,k,l"))
})

test_that("a quoted cell with a line break stays one cell (A13)", {
    p <- write_lines_tmp(c(
        "scientificName;occurrenceRemarks",
        "Paubrasilia echinata;\"linha 1, a",
        "linha 2, b, c, d\"",
        "Araucaria angustifolia;x"
    ), "csv")
    on.exit(unlink(p), add = TRUE)
    out <- read_ObservaBio_table(p)
    expect_equal(nrow(out$records), 2L)
    expect_equal(out$records$scientificName[[2]], "Araucaria angustifolia")
})

test_that("a .tsv file uses tab when tab gives the header (A13)", {
    expect_equal(sniff_delimiter(c("scientificName\tnote", "Aus bus\ta;b;c;d"),
                                 ext = "tsv"), "\t")
})

test_that("duplicate headers stop the import with names and positions (A14)", {
    p <- write_lines_tmp(c("scientificName;family;family ", "Aus bus;First;Second"), "csv")
    on.exit(unlink(p), add = TRUE)
    expect_error(read_ObservaBio_table(p), "family \\(colunas 2, 3\\)")
})
