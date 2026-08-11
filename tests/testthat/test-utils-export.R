# Export round-trip: the standardized sheet must round-trip through writexl/readxl
# with the ZHOUSE double header (row 1 PT-BR labels, row 2 DwC names), and the
# audit workbook must carry the two expected sheets.

read_blank_na <- function(path, sheet = 1) {
    df <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, function(col) {
        col <- as.character(col)
        col[is.na(col)] <- ""
        col
    })
    df
}

test_that("dwc_double_header_frame stacks PT labels, DwC names, then data", {
    dwc <- data.frame(
        scientificName = c("Handroanthus impetiginosus", "Cedrela fissilis"),
        family = c("Bignoniaceae", "Meliaceae"),
        distributionFlag = c(NA_character_, NA_character_),
        stringsAsFactors = FALSE
    )
    frame <- dwc_double_header_frame(
        dwc,
        model_cols = names(dwc),
        pt_labels = c("Nome cientifico", "Familia", "")
    )
    expect_equal(nrow(frame), 4L) # 2 header rows + 2 data rows
    expect_equal(unlist(frame[1, ], use.names = FALSE), c("Nome cientifico", "Familia", ""))
    expect_equal(unlist(frame[2, ], use.names = FALSE), c("scientificName", "family", "distributionFlag"))
    expect_equal(unlist(frame[3, ], use.names = FALSE), c("Handroanthus impetiginosus", "Bignoniaceae", ""))
})

test_that("dwc_double_header_frame blanks readxl auto-names and pads labels", {
    dwc <- data.frame(a = "1", b = "2", c = "3", stringsAsFactors = FALSE)
    frame <- dwc_double_header_frame(dwc, model_cols = names(dwc),
                                     pt_labels = c("Rotulo", "...2"))
    # "...2" auto-name blanked, and the short label vector padded to 3 columns.
    expect_equal(unlist(frame[1, ], use.names = FALSE), c("Rotulo", "", ""))
})

test_that("write_standardized_xlsx round-trips through readxl", {
    dwc <- data.frame(
        scientificName = c("Handroanthus impetiginosus", "Cedrela fissilis"),
        family = c("Bignoniaceae", "Meliaceae"),
        distributionFlag = c(NA_character_, NA_character_),
        stringsAsFactors = FALSE
    )
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    write_standardized_xlsx(dwc, path, model_cols = names(dwc),
                            pt_labels = c("Nome cientifico", "Familia", ""))
    back <- read_blank_na(path)
    expect_equal(as.character(back[1, ]), c("Nome cientifico", "Familia", ""))
    expect_equal(as.character(back[2, ]), c("scientificName", "family", "distributionFlag"))
    expect_equal(as.character(back[3, ]), c("Handroanthus impetiginosus", "Bignoniaceae", ""))
    expect_equal(nrow(back), 4L)
})

test_that("write_audit_xlsx writes the auditoria and nao_resolvidos sheets", {
    cascade <- data.frame(
        query_name = c("Handroanthus impetiginosus", "Zzz nonexistus"),
        scientificName = c("Handroanthus impetiginosus", "Zzz nonexistus"),
        taxonomicStatus = c("accepted", NA_character_),
        validation_status = c("accepted", "not_found"),
        match_count = c(1L, 0L),
        provider = c("florabr", NA_character_),
        taxonID = c("t-123", NA_character_),
        taxonRank = c("Species", NA_character_),
        acceptedNameUsageID = NA_character_,
        kingdom = c("Plantae", NA_character_),
        phylum = NA_character_, class = NA_character_, order = NA_character_,
        family = c("Bignoniaceae", NA_character_), genus = c("Handroanthus", NA_character_),
        specificEpithet = c("impetiginosus", NA_character_),
        infraspecificEpithet = NA_character_, vernacularName = NA_character_,
        stringsAsFactors = FALSE
    )
    audit <- build_audit_table(cascade)
    path <- withr::local_tempfile(fileext = ".xlsx")
    write_audit_xlsx(audit, path)

    sheets <- readxl::excel_sheets(path)
    expect_equal(sheets, c("auditoria", "nao_resolvidos"))

    auditoria <- readxl::read_excel(path, sheet = "auditoria")
    expect_equal(names(auditoria), audit_columns())
    expect_equal(nrow(auditoria), 2L)

    nao_resolvidos <- readxl::read_excel(path, sheet = "nao_resolvidos")
    expect_equal(nrow(nao_resolvidos), 1L)
    expect_equal(nao_resolvidos$queryName, "Zzz nonexistus")
})

test_that("write_audit_xlsx round-trips the geo columns when the audit carries them", {
    cascade <- data.frame(
        query_name = "Handroanthus impetiginosus",
        scientificName = "Handroanthus impetiginosus",
        taxonomicStatus = "accepted", validation_status = "accepted",
        match_count = 1L, provider = "florabr", taxonID = "t-123",
        taxonRank = "Species", acceptedNameUsageID = NA_character_,
        kingdom = "Plantae", phylum = NA_character_, class = NA_character_,
        order = NA_character_, family = "Bignoniaceae", genus = "Handroanthus",
        specificEpithet = "impetiginosus", infraspecificEpithet = NA_character_,
        vernacularName = NA_character_, stringsAsFactors = FALSE
    )
    geo <- list(
        per_species = data.frame(
            query_name = "Handroanthus impetiginosus", gbif_count = 7L,
            distributionFlag = "confirmada", stringsAsFactors = FALSE
        ),
        area_states = c("BA", "MG"), area_biomes = "Mata Atlântica"
    )
    audit <- build_audit_table(cascade, geo = geo)
    path <- withr::local_tempfile(fileext = ".xlsx")
    write_audit_xlsx(audit, path)

    auditoria <- readxl::read_excel(path, sheet = "auditoria")
    expect_true(all(audit_geo_columns() %in% names(auditoria)))
    expect_equal(auditoria$distributionFlag, "confirmada")
    expect_equal(as.integer(auditoria$gbifRecords), 7L)
    expect_equal(auditoria$areaStates, "BA; MG")
})
