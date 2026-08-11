# data-raw/prep_br_uf_biomes.R
# Regenerate the embedded UF + biome layer used by the geographic verification
# (SPEC §7, §8). Run LOCALLY (never at runtime): it downloads the IBGE state and
# biome polygons via geobr, simplifies them, harmonizes biome labels to the
# florabr English vocabulary, and writes inst/extdata/br_uf_biomes.rds + a
# version sidecar.
#
# Usage:  Rscript data-raw/prep_br_uf_biomes.R
# Notes:  - Kept small on purpose (ADR-004: 1 GB shinyapps.io bundle). The layers
#           are simplified in a metric CRS and stored in WGS84 (EPSG:4326).
#         - Stored as a list(states, biomes) of two sf layers, NOT a states x
#           biomes intersection: the "Sistema Costeiro" biome overlaps the
#           terrestrial biomes, which would make an intersection ambiguous. The
#           runtime does two cheap intersects instead (see utils_geo.R).

suppressWarnings(suppressMessages({
    library(geobr)
    library(sf)
}))

sf::sf_use_s2(TRUE)

out_rds  <- file.path("inst", "extdata", "br_uf_biomes.rds")
out_meta <- sub("\\.rds$", ".meta.json", out_rds)
dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)

# geobr data years pinned so the embedded layer is reproducible.
state_year <- 2020L
biome_year <- 2019L

# Portuguese biome names (geobr) -> florabr English vocabulary, so the area's
# biome cross-checks directly against the florabr `biome` column (ADR-008).
biome_labels <- c(
    "Amazônia"       = "Amazon",
    "Mata Atlântica" = "Atlantic_Forest",
    "Caatinga"            = "Caatinga",
    "Cerrado"             = "Cerrado",
    "Pampa"               = "Pampa",
    "Pantanal"            = "Pantanal",
    "Sistema Costeiro"    = "Coastal_System"
)

# Simplify in a metric CRS (SIRGAS 2000 / Brazil Polyconic) then store in WGS84.
simplify_layer <- function(x, tolerance_m = 500) {
    x <- sf::st_transform(x, 5880)
    x <- sf::st_simplify(x, dTolerance = tolerance_m, preserveTopology = TRUE)
    x <- sf::st_make_valid(x)
    sf::st_transform(x, 4326)
}

message("Downloading IBGE states (geobr::read_state)...")
states_raw <- geobr::read_state(year = state_year, simplified = TRUE, showProgress = FALSE)
states <- states_raw["abbrev_state"]
names(states)[names(states) == "abbrev_state"] <- "uf"
states$uf <- as.character(states$uf)
states <- simplify_layer(states)

message("Downloading IBGE biomes (geobr::read_biomes)...")
biomes_raw <- geobr::read_biomes(year = biome_year, simplified = TRUE, showProgress = FALSE)
biome_en <- unname(biome_labels[as.character(biomes_raw$name_biome)])
if (anyNA(biome_en)) {
    stop(sprintf(
        "Unmapped biome name(s): %s. Update biome_labels in prep_br_uf_biomes.R.",
        paste(unique(biomes_raw$name_biome[is.na(biome_en)]), collapse = ", ")
    ))
}
biomes <- sf::st_sf(
    data.frame(biome = biome_en, stringsAsFactors = FALSE),
    geometry = sf::st_geometry(biomes_raw)
)
biomes <- simplify_layer(biomes)

layers <- list(states = states, biomes = biomes)
saveRDS(layers, out_rds, compress = "xz")

meta <- list(
    provider = "br_uf_biomes",
    version  = sprintf("states:%d;biomes:%d", state_year, biome_year),
    date     = as.character(Sys.Date()),
    source   = "IBGE via geobr::read_state + geobr::read_biomes",
    geobr    = as.character(utils::packageVersion("geobr")),
    crs      = "EPSG:4326",
    states   = nrow(states),
    biomes   = nrow(biomes),
    biome_labels = unname(biome_labels)
)
jsonlite::write_json(meta, out_meta, auto_unbox = TRUE, pretty = TRUE)

message(sprintf(
    "Wrote %s (%d states, %d biomes, %.0f KB) and %s",
    out_rds, nrow(states), nrow(biomes),
    file.info(out_rds)$size / 1024, out_meta
))
