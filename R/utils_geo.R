# Title: Geographic Verification Helpers (part 1)
# Pure sf helpers for the geographic verification (SPEC §8, week 5): read the
# uploaded area (shapefile or KML), build a 10 km buffer in a metric CRS, and
# find which UF(s)/biome(s) the operation area falls in, then cross-check those
# against the UF/biome distribution florabr/faunabr carry per species. No Shiny
# here, and no GBIF occurrences (that is utils_gbif_occ.R + the final
# distributionFlag, week 6). The unpack step is upstream in
# utils_upload.R::unpack_area_files().

.geo_cache <- new.env(parent = emptyenv())
.BR_UF_BIOMES_RDS <- "br_uf_biomes.rds"

#' Read the operation-area shapefile from an unzipped directory
#'
#' Reads the `.shp` with `sf::st_read` and requires a defined CRS — without one
#' we cannot reproject for the metric buffer. Accepts either a directory (the
#' `unzip_shapefile()` output `dir`) or a direct `.shp` path.
#'
#' @param path Directory containing the shapefile, or a `.shp` file path.
#' @param quiet Passed to `sf::st_read`.
#' @return An `sf` object with a defined CRS.
#' @noRd
geo_read_shapefile <- function(path, quiet = TRUE) {
    if (!file.exists(path)) {
        stop(sprintf("Shapefile path not found: %s", path))
    }
    shp <- path
    if (dir.exists(path)) {
        shps <- list.files(path, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
        if (length(shps) == 0L) {
            stop(sprintf("No .shp file found in directory: %s", path))
        }
        shp <- shps[[1L]]
    }
    area <- sf::st_read(shp, quiet = quiet)
    if (is.na(sf::st_crs(area))) {
        stop("Shapefile has no CRS (.prj missing or unreadable); cannot reproject for the 10 km buffer.")
    }
    area
}

#' Read the operation area from a `.kml`
#'
#' Reads every layer and joins them. GDAL turns each KML folder into its own
#' layer, so `sf::st_read()` alone would take the first folder and drop the rest
#' with only a warning — a Google Earth file with "Talhoes" and "Reserva" would
#' silently lose the reserve.
#'
#' Two normalizations follow the read. KML carries altitude on every vertex, so
#' `st_zm()` drops Z before the geometry reaches `st_union()`. And when the file
#' holds polygons, only the polygons survive: a stray placemark mixed into the
#' area would make `st_union()` return one geometry per type, and
#' [metric_crs_for()] would then pick the CRS from a single part's centroid. A
#' file with no polygon at all keeps whatever it has — a 10 km buffer around a
#' point is still a valid operation area.
#'
#' @param path Path to a `.kml` file.
#' @param quiet Passed to `sf::st_read`.
#' @return An `sf` object with a defined CRS.
#' @noRd
geo_read_kml <- function(path, quiet = TRUE) {
    if (!file.exists(path)) {
        stop(sprintf("KML path not found: %s", path))
    }
    layers <- sf::st_layers(path)$name
    if (length(layers) == 0L) {
        stop(sprintf("KML has no readable layer: %s", path))
    }

    parts <- lapply(layers, function(layer) {
        geom <- tryCatch(
            sf::st_geometry(sf::st_read(path, layer = layer, quiet = quiet)),
            error = function(e) NULL
        )
        if (is.null(geom) || length(geom) == 0L) NULL else sf::st_zm(geom, drop = TRUE)
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) {
        stop(sprintf("KML has no geometry: %s", path))
    }
    geom <- do.call(c, parts)

    polygons <- as.character(sf::st_geometry_type(geom)) %in%
        c("POLYGON", "MULTIPOLYGON")
    if (any(polygons)) {
        geom <- geom[polygons]
    }
    # KML is WGS84 by definition of the format, so a missing CRS cannot be
    # ambiguous here the way a shapefile without a .prj is.
    if (is.na(sf::st_crs(geom))) {
        sf::st_crs(geom) <- 4326
    }
    sf::st_sf(geometry = geom)
}

#' Read the operation area from any accepted source
#'
#' Dispatches on the extension: a `.kml` goes to [geo_read_kml()], and everything
#' else (a directory or a `.shp`) to [geo_read_shapefile()]. A `.kmz` never
#' reaches here — [unpack_kmz()] already extracted the `.kml` at upload time.
#'
#' @param path Directory, `.shp` path, or `.kml` path.
#' @return An `sf` object with a defined CRS.
#' @noRd
geo_read_area <- function(path) {
    if (tolower(tools::file_ext(path)) == "kml") {
        return(geo_read_kml(path))
    }
    geo_read_shapefile(path)
}

#' Pick a metric CRS (SIRGAS 2000 / UTM zone) for a geometry
#'
#' Uses the geometry centroid to choose the local SIRGAS 2000 UTM zone so the
#' 10 km buffer is a planar metric buffer with minimal distortion at farm scale.
#' Falls back to SIRGAS 2000 / Brazil Polyconic (EPSG:5880) — metric, whole
#' country — for centroids outside Brazil's UTM zones (ADR-008).
#'
#' @param x An `sf`/`sfc` geometry (any CRS).
#' @return Integer EPSG code of a metric CRS.
#' @noRd
metric_crs_for <- function(x) {
    coords <- suppressWarnings(
        sf::st_coordinates(
            sf::st_centroid(sf::st_union(sf::st_transform(sf::st_geometry(x), 4326)))
        )
    )
    lon <- coords[1L, "X"]
    lat <- coords[1L, "Y"]
    zone <- floor((lon + 180) / 6) + 1L
    if (lat >= 0) {
        # SIRGAS 2000 / UTM North zones 18N-22N span EPSG 31972-31976.
        if (zone >= 18L && zone <= 22L) return(as.integer(31954L + zone))
    } else {
        # SIRGAS 2000 / UTM South zones 17S-25S span EPSG 31977-31985.
        if (zone >= 17L && zone <= 25L) return(as.integer(31960L + zone))
    }
    5880L
}

#' Build a 10 km buffer around the operation area
#'
#' Dissolves the input features, reprojects to a metric CRS, buffers by
#' `dist_m`, and returns the original area and the buffer in WGS84 (ready for the
#' week-6 GBIF query and the leaflet map). SPEC §8 step 2.
#'
#' @param area An `sf`/`sfc` geometry with a defined CRS.
#' @param dist_m Buffer distance in metres (default 10 km).
#' @param metric_crs Optional EPSG to force; otherwise chosen by `metric_crs_for`.
#' @return List with `area` and `buffer` (both `sfc`, EPSG:4326), the
#'   `crs_metric` used, and `dist_m`.
#' @noRd
geo_buffer <- function(area, dist_m = 10000, metric_crs = NULL) {
    geom <- sf::st_make_valid(sf::st_geometry(area))
    dissolved <- sf::st_union(geom)
    if (is.null(metric_crs)) {
        metric_crs <- metric_crs_for(dissolved)
    }
    metric <- sf::st_transform(dissolved, metric_crs)
    buffered <- sf::st_buffer(metric, dist = dist_m)
    list(
        area = sf::st_transform(dissolved, 4326),
        buffer = sf::st_transform(buffered, 4326),
        crs_metric = as.integer(metric_crs),
        dist_m = dist_m
    )
}

#' Load and cache the embedded UF + biome layer
#'
#' Reads `inst/extdata/br_uf_biomes.rds` (a `list(states, biomes)` of two WGS84
#' `sf` layers) produced offline by `data-raw/prep_br_uf_biomes.R`.
#'
#' @param force_reload Bypass the session cache.
#' @return List with `states` (column `uf`) and `biomes` (column `biome`).
#' @noRd
load_br_uf_biomes <- function(force_reload = FALSE) {
    if (!force_reload && !is.null(.geo_cache$uf_biomes)) {
        return(.geo_cache$uf_biomes)
    }
    path <- br_extdata_path(.BR_UF_BIOMES_RDS)
    if (!file.exists(path)) {
        stop(sprintf(
            "Embedded UF+biome layer not found at '%s'. Run data-raw/prep_br_uf_biomes.R.",
            path
        ))
    }
    layers <- readRDS(path)
    .geo_cache$uf_biomes <- layers
    layers
}

#' Find which UF(s) and biome(s) the operation area falls in
#'
#' Intersects the (dissolved) area with the UF and biome layers and returns the
#' distinct labels it touches. Biome labels use the florabr English vocabulary
#' (harmonized in the prep script) so they cross-check directly against the
#' florabr `biome` column. SPEC §8 step 4.
#'
#' @param area An `sf`/`sfc` geometry with a defined CRS.
#' @param layers Optional `list(states, biomes)`; defaults to the embedded layer.
#' @return List with `states` and `biomes` character vectors (sorted, unique).
#' @noRd
geo_area_uf_biomes <- function(area, layers = NULL) {
    if (is.null(layers)) {
        layers <- load_br_uf_biomes()
    }
    geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(area)))
    pick <- function(layer, col) {
        if (is.null(layer) || nrow(layer) == 0L) {
            return(character(0))
        }
        g <- sf::st_transform(geom, sf::st_crs(layer))
        hit <- lengths(sf::st_intersects(layer, g)) > 0L
        vals <- as.character(layer[[col]][hit])
        sort(unique(vals[!is.na(vals) & nzchar(vals)]))
    }
    list(
        states = pick(layers$states, "uf"),
        biomes = pick(layers$biomes, "biome")
    )
}

#' Split a distribution field into clean tokens
#'
#' florabr/faunabr store UF and biome distribution as `;`-separated strings with
#' `NA`/`""`/`"Unknown"` sentinels. This parses them into a clean, unique vector.
#'
#' @param x A character scalar/vector of distribution values.
#' @return Character vector of tokens (possibly empty).
#' @noRd
split_distribution <- function(x) {
    if (length(x) == 0L) {
        return(character(0))
    }
    toks <- unlist(strsplit(as.character(x), "[;,]"), use.names = FALSE)
    toks <- trimws(toks)
    toks <- toks[!is.na(toks) & nzchar(toks)]
    toks <- toks[!tolower(toks) %in% c("unknown", "na")]
    unique(toks)
}

#' Cross-check a species' UF/biome distribution against the operation area
#'
#' Compares the area's UF(s)/biome(s) (from `geo_area_uf_biomes`) against the
#' distribution the base carries for one species (florabr `states`/`biome`,
#' faunabr `states`). SPEC §8 step 4. Each dimension is `TRUE` (overlap),
#' `FALSE` (data present, no overlap), or `NA` (no data on that dimension).
#' `present` is `TRUE` if the species occurs in any of the area's UF or biome,
#' `FALSE` if data exists but never overlaps, `NA` if nothing is known. The final
#' 4-category `distributionFlag` (which also weighs GBIF-in-buffer) is assembled
#' in week 6.
#'
#' @param area_states,area_biomes Character vectors from `geo_area_uf_biomes`.
#' @param species_states,species_biomes Distribution field(s) for one species.
#' @return List with `state_match`, `biome_match`, `present` (each TRUE/FALSE/NA).
#' @noRd
geo_crosscheck_distribution <- function(area_states, area_biomes,
                                        species_states, species_biomes = NA) {
    area_states <- split_distribution(area_states)
    area_biomes <- split_distribution(area_biomes)
    sp_states <- split_distribution(species_states)
    sp_biomes <- split_distribution(species_biomes)

    match_dim <- function(area_vals, sp_vals) {
        if (length(sp_vals) == 0L || length(area_vals) == 0L) {
            return(NA)
        }
        any(area_vals %in% sp_vals)
    }
    state_match <- match_dim(area_states, sp_states)
    biome_match <- match_dim(area_biomes, sp_biomes)

    known <- c(state_match, biome_match)
    known <- known[!is.na(known)]
    present <- if (length(known) == 0L) NA else any(known)

    list(state_match = state_match, biome_match = biome_match, present = present)
}

# ---------------------------------------------------------------------------
# distributionFlag classifier (SPEC §8 steps 5-6) — the week-6 verdict
# ---------------------------------------------------------------------------

#' The four verbatim distributionFlag categories (SPEC §8 step 5, PT-BR)
#' @noRd
distribution_flag_levels <- function() {
    c(
        confirmed  = "confirmada",
        near_absent = "sem registro próximo, presente no estado/bioma",
        outside    = "sem registro no estado/bioma",
        no_data    = "sem dados disponíveis"
    )
}

#' Classify the distributionFlag for one or more species
#'
#' Combines the GBIF-in-buffer signal (`has_gbif`, from
#' [gbif_occ_presence()]) with the state/biome cross-check (`present`, from
#' [geo_crosscheck_distribution()]) into exactly one of the four SPEC §8 step-5
#' categories. Precedence (ADR-010): a GBIF record inside the 10 km buffer is the
#' strongest confirmation, so it wins outright; otherwise the cross-check decides,
#' and only a *complete* absence of distribution data yields "sem dados
#' disponíveis". This is an ALERT, not a verdict (SPEC §1/§8 note 6).
#'
#' Edge cases (SPEC §8 step 6): a species with no georeferenced GBIF occurrence
#' has `has_gbif = FALSE` and is judged by the cross-check alone; a species with
#' no distribution data anywhere has `present = NA` -> "sem dados disponíveis".
#'
#' Pure and deterministic: vectorized and name-preserving, so a whole upload's
#' per-species flags come from one call. `has_gbif`/`present` are recycled from
#' length 1.
#'
#' @param has_gbif Logical (TRUE/FALSE/NA): a GBIF point inside the buffer.
#' @param present Logical (TRUE/FALSE/NA): the cross-check `present` field.
#' @return Character vector of flags, names carried from the inputs.
#' @noRd
classify_distribution_flag <- function(has_gbif, present) {
    lv <- distribution_flag_levels()
    n <- max(length(has_gbif), length(present))
    if (n == 0L) {
        return(character(0))
    }
    # Capture species names from the length-n input before coercion strips them.
    nm <- names(present)
    if (is.null(nm) || length(nm) != n) nm <- names(has_gbif)
    if (is.null(nm) || length(nm) != n) nm <- NULL

    if (length(has_gbif) == 1L) has_gbif <- rep(has_gbif, n)
    if (length(present) == 1L) present <- rep(present, n)
    if (length(has_gbif) != n || length(present) != n) {
        stop("classify_distribution_flag(): has_gbif and present must share a length.")
    }
    has_gbif <- as.logical(has_gbif)
    present <- as.logical(present)

    out <- character(n)
    gbif_yes <- !is.na(has_gbif) & has_gbif
    out[gbif_yes] <- lv[["confirmed"]]
    rest <- !gbif_yes
    out[rest & !is.na(present) & present]  <- lv[["near_absent"]]
    out[rest & !is.na(present) & !present] <- lv[["outside"]]
    out[rest & is.na(present)]             <- lv[["no_data"]]

    names(out) <- nm
    out
}
