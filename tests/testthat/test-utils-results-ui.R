# Results-screen presentation mappings (Step 3): biome labels/colours, threat
# levels, and taxon kingdom badges. Pure — no Shiny.

test_that("biome_label maps florabr tokens to PT-BR names, passing through unknowns", {
    expect_equal(biome_label(c("Atlantic_Forest", "Cerrado", "Amazon")),
                 c("Mata Atlântica", "Cerrado", "Amazônia"))
    expect_equal(biome_label("Sistema Costeiro"), "Sistema Costeiro")
    expect_equal(biome_label("Foobar"), "Foobar")   # unknown passes through
})

test_that("biome_class keys the colour class off the biome token", {
    expect_equal(biome_class(c("Atlantic_Forest", "Cerrado")),
                 c("biome--atlantic", "biome--cerrado"))
    expect_equal(biome_class("desconhecido"), "biome--other")
})

test_that("threat_status_class extracts the level from an MMA/IUCN label", {
    expect_equal(threat_status_class("(VU) Vulneravel"), "threat--vu")
    expect_equal(threat_status_class("(CR (PEX)) Criticamente Em Perigo"), "threat--cr")
    expect_equal(threat_status_class("(NE) Não avaliada"), "threat--ne")
    expect_equal(threat_status_class("LC"), "threat--lc")   # bare code
    expect_equal(threat_status_class(NA), "threat--ne")     # missing -> NE
})

test_that("kingdom_badge_parts labels animal vs plant and defaults otherwise", {
    expect_equal(kingdom_badge_parts("Animalia")$label, "Animal")
    expect_equal(kingdom_badge_parts("Plantae")$cls, "taxon--plant")
    expect_equal(kingdom_badge_parts("Fungi")$icon, "seedling")
    expect_null(kingdom_badge_parts(NA))
    expect_null(kingdom_badge_parts(""))
    expect_equal(kingdom_badge_parts("Bacteria")$cls, "taxon--other")
})

test_that("invasive_badge_html renders only for listed species, source in the tooltip", {
    html <- invasive_badge_html(c(TRUE, NA, FALSE),
                                c("GRIIS Brasil", NA, NA))

    expect_match(html[[1]], "badge-invasive")
    expect_match(html[[1]], "exótica invasora", fixed = TRUE)
    expect_match(html[[1]], "title='GRIIS Brasil'", fixed = TRUE)
    # Not listed (NA) and explicitly not invasive both render nothing at all.
    expect_equal(html[[2]], "")
    expect_equal(html[[3]], "")
})

test_that("invasive_badge_html escapes the source and survives a missing one", {
    expect_match(invasive_badge_html(TRUE, "A & B"), "A &amp; B", fixed = TRUE)
    expect_match(invasive_badge_html(TRUE), "badge-invasive")   # no source -> no title
    expect_equal(invasive_badge_html(logical(0), character(0)), character(0))
})

# ── Resultado filters (pure) ───────────────────────────────────────────────
# A build_results_view() frame to filter against. Row 5 is a pre-validated row:
# no cascade match (NA matchType), no kingdom, no flag.
fake_view <- function() {
    data.frame(
        scientificName = c("Panthera onca", "Cedrela fissilis", "Hovenia dulcis",
                           "Amanita muscaria", "Nomen dubium"),
        family = c("Felidae", "Meliaceae", "Rhamnaceae", "Amanitaceae", NA),
        genus = c("Panthera", "Cedrela", "Hovenia", "Amanita", NA),
        kingdom = c("Animalia", "Plantae", "Plantae", "Fungi", NA),
        taxonID = c("1", "2", "3", "4", NA),
        status = c("(VU) Vulneravel", "(EN) Em Perigo",
                   "(LC) Pouco Preocupante", NA, NA),
        validator = c("fauna", "flora", "flora", "gbif", NA),
        matchType = c("aceito", "sinônimo", "aceito", "ambíguo", NA),
        distributionFlag = c("confirmada", "sem registro no estado/bioma",
                             "confirmada", "sem dados disponíveis", ""),
        gbif_count = c(12L, 0L, 3L, NA, NA),
        invasive = c(NA, NA, TRUE, NA, NA),
        invasiveSource = c(NA, NA, "Instituto Hórus", NA, NA),
        area = c("RPPN", "RPPN", "Trijuncao", "Trijuncao", NA),
        stringsAsFactors = FALSE
    )
}

test_that("kingdom_key collapses the taxon to a filter key, NA staying unknown", {
    expect_equal(kingdom_key(c("Animalia", "plantae", " Fungi ")),
                 c("animalia", "plantae", "fungi"))
    expect_equal(kingdom_key("Bacteria"), "other")   # unknown kingdom -> other
    expect_equal(kingdom_key(c(NA, "")), c(NA_character_, NA_character_))
})

test_that("is_threatened_status is TRUE only for VU/EN/CR/EX levels", {
    expect_true(is_threatened_status("(VU) Vulneravel"))
    expect_true(is_threatened_status("(CR (PEX)) Criticamente Em Perigo"))
    expect_true(is_threatened_status("EX"))
    # Not threatened: least concern, near threatened, unevaluated, missing.
    expect_false(is_threatened_status("(LC) Pouco Preocupante"))
    expect_false(is_threatened_status("NT"))
    expect_false(is_threatened_status("(NE) Não avaliada"))
    expect_false(is_threatened_status(NA))
})

test_that("the area dimension narrows to the records linked to those areas", {
    view <- fake_view()
    expect_equal(nrow(filter_results_view(view, areas = "RPPN")), 2L)
    expect_equal(nrow(filter_results_view(view, areas = c("RPPN", "Trijuncao"))), 4L)
    # The unlinked record (area NA) belongs to no area, so it never matches.
    expect_equal(nrow(filter_results_view(view, areas = "Inexistente")), 0L)
    # Areas combine with the other dimensions by AND.
    expect_equal(
        filter_results_view(view, areas = "Trijuncao", kingdoms = "plantae")$scientificName,
        "Hovenia dulcis"
    )
})

test_that("the area filter dimension is built from the view, absent when unused", {
    dims <- results_filter_options(fake_view())
    expect_equal(dims$areas$options$key, c("RPPN", "Trijuncao"))
    expect_equal(dims$areas$options$count, c(2L, 2L))
    expect_equal(dims$areas$label, "Área")

    # A single-area (or no-area) upload offers nothing to filter by.
    view <- fake_view()
    view$area <- NA_character_
    expect_equal(nrow(results_filter_options(view)$areas$options), 0L)
})

test_that("filter_results_view with no filter returns the whole view", {
    view <- fake_view()
    expect_equal(filter_results_view(view), view)
    # An empty dimension is not a constraint either (nothing selected).
    expect_equal(nrow(filter_results_view(view, flags = character(0))), 5L)
    # map_species_r relies on flags = NULL dropping the distribution constraint,
    # so only the orthogonal dimension applies (the map ignores distributionFlag).
    expect_equal(
        filter_results_view(view, flags = NULL, kingdoms = "plantae")$scientificName,
        c("Cedrela fissilis", "Hovenia dulcis")
    )
})

test_that("filter_results_view filters each dimension on its own", {
    view <- fake_view()

    expect_equal(filter_results_view(view, flags = "confirmada")$scientificName,
                 c("Panthera onca", "Hovenia dulcis"))
    expect_equal(filter_results_view(view, kingdoms = "plantae")$scientificName,
                 c("Cedrela fissilis", "Hovenia dulcis"))
    # Threatened (VU + EN); LC, NA and the unflagged row are not claims of threat.
    expect_equal(filter_results_view(view, conservation = "ameacada")$scientificName,
                 c("Panthera onca", "Cedrela fissilis"))
    expect_equal(filter_results_view(view, conservation = "invasora")$scientificName,
                 "Hovenia dulcis")
    # The pre-validated row is the one the cascade never saw.
    expect_equal(filter_results_view(view, match_types = "já validado")$scientificName,
                 "Nomen dubium")
})

test_that("filter_results_view ORs within a dimension and ANDs across them", {
    view <- fake_view()

    # OR inside the dimension: threatened OR invasive.
    expect_equal(
        filter_results_view(view, conservation = c("ameacada", "invasora"))$scientificName,
        c("Panthera onca", "Cedrela fissilis", "Hovenia dulcis")
    )
    # AND across dimensions: confirmada AND a plant.
    expect_equal(
        filter_results_view(view, flags = "confirmada", kingdoms = "plantae")$scientificName,
        "Hovenia dulcis"
    )
    # A combination nothing satisfies comes back empty, not NULL.
    empty <- filter_results_view(view, kingdoms = "fungi", conservation = "invasora")
    expect_s3_class(empty, "data.frame")
    expect_equal(nrow(empty), 0L)
})

test_that("filter_results_view leaves an empty view alone", {
    empty <- fake_view()[0, , drop = FALSE]
    expect_equal(nrow(filter_results_view(empty, flags = "confirmada")), 0L)
})

test_that("results_filter_options counts the unfiltered view and drops zero options", {
    dims <- results_filter_options(fake_view())

    # "sem registro próximo, presente no estado/bioma" matches no record -> gone.
    flags <- dims$flags$options
    expect_equal(flags$key, c("confirmada", "sem registro no estado/bioma",
                              "sem dados disponíveis"))
    expect_equal(flags$count, c(2L, 1L, 1L))
    # The long category keeps the verbatim text in the tooltip, short in the pill.
    expect_equal(dims$flags$label, "Distribuição")

    # No "other" kingdom in the data; the NA-kingdom row counts for nothing.
    expect_equal(dims$kingdoms$options$key, c("animalia", "plantae", "fungi"))
    expect_equal(dims$kingdoms$options$count, c(1L, 2L, 1L))

    expect_equal(dims$conservation$options$key, c("ameacada", "invasora"))
    expect_equal(dims$conservation$options$count, c(2L, 1L))

    # "não encontrado" matches nothing; the NA matchType row is "já validado".
    expect_equal(dims$match_types$options$key,
                 c("aceito", "sinônimo", "ambíguo", "já validado"))
    expect_equal(dims$match_types$options$count, c(2L, 1L, 1L, 1L))
})

test_that("results_filter_options on an empty view offers nothing", {
    dims <- results_filter_options(fake_view()[0, , drop = FALSE])
    expect_equal(vapply(dims, function(d) nrow(d$options), integer(1)),
                 c(areas = 0L, flags = 0L, kingdoms = 0L, conservation = 0L,
                   match_types = 0L))
})
