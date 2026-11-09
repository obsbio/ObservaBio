# gbif_map_backbone(): offline rows shaped like rgbif::name_backbone_checklist().

backbone_row <- function(verbatim_name, canonicalName, rank, status, matchType,
                         kingdom = NA, family = NA, genus = NA, authorship = NA,
                         usageKey = "1", acceptedUsageKey = NA) {
    data.frame(verbatim_name = verbatim_name, canonicalName = canonicalName,
               rank = rank, status = status, matchType = matchType,
               kingdom = kingdom, family = family, genus = genus,
               authorship = authorship, usageKey = usageKey,
               acceptedUsageKey = acceptedUsageKey, stringsAsFactors = FALSE)
}

test_that("a HIGHERRANK answer keeps the input name and says not_found", {
    # GBIF did not find the species and answered with its genus. The genus is
    # not the name, so the name must not change and no taxonID is claimed.
    raw <- backbone_row("Hydropsalis albicollis", "Hydropsalis", "GENUS", "ACCEPTED",
                        "HIGHERRANK", kingdom = "Animalia", family = "Caprimulgidae",
                        genus = "Hydropsalis", authorship = "Wagler, 1832",
                        usageKey = "2497010")
    got <- gbif_map_backbone(raw, "Hydropsalis albicollis")
    expect_equal(got$scientificName, "Hydropsalis albicollis")
    expect_equal(got$validation_status, "not_found")
    expect_true(is.na(got$taxonID))
    expect_true(is.na(got$taxonomicStatus))
    expect_true(is.na(got$scientificNameAuthorship))
    # The accepted parent still places the name.
    expect_equal(got$family, "Caprimulgidae")
    expect_equal(got$genus, "Hydropsalis")
    expect_equal(got$specificEpithet, "albicollis")
    expect_equal(got$taxonRank, "species")
})

test_that("a HIGHERRANK synonym parent gives no classification", {
    # `Platyrhincus` is a synonym of the sea lion genus Otaria: its family would
    # put a flycatcher among the sea lions.
    raw <- backbone_row("Platyrhincus mystaceus", "Platyrhincus", "GENUS", "SYNONYM",
                        "HIGHERRANK", kingdom = "Animalia", family = "Otariidae",
                        genus = "Otaria", acceptedUsageKey = "2433454")
    got <- gbif_map_backbone(raw, "Platyrhincus mystaceus")
    expect_equal(got$validation_status, "not_found")
    expect_true(is.na(got$kingdom))
    expect_true(is.na(got$family))
    expect_true(is.na(got$acceptedNameUsageID))
    expect_equal(got$genus, "Platyrhincus")
})

test_that("a HIGHERRANK answer with the name itself is a normal match", {
    # GBIF labels some plain genus matches HIGHERRANK (`Psychotria`).
    raw <- backbone_row("Psychotria", "Psychotria", "GENUS", "ACCEPTED", "HIGHERRANK",
                        kingdom = "Plantae", family = "Rubiaceae", genus = "Psychotria")
    got <- gbif_map_backbone(raw, "Psychotria")
    expect_equal(got$validation_status, "accepted")
    expect_equal(got$taxonID, "1")
})

test_that("a synonym takes its genus from its own name, with its authorship", {
    # GBIF gives the accepted classification (genus Lissachatina), which would
    # contradict the name the row keeps.
    raw <- backbone_row("Achatina fulica", "Achatina fulica", "SPECIES", "SYNONYM",
                        "EXACT", kingdom = "Animalia", family = "Achatinidae",
                        genus = "Lissachatina", authorship = "Bowdich, 1822",
                        usageKey = "10928934", acceptedUsageKey = "8790531")
    got <- gbif_map_backbone(raw, "Achatina fulica")
    expect_equal(got$validation_status, "synonym")
    expect_equal(got$genus, "Achatina")
    expect_equal(got$family, "Achatinidae")
    expect_equal(got$scientificNameAuthorship, "Bowdich, 1822")
    expect_equal(got$acceptedNameUsageID, "8790531")
})
