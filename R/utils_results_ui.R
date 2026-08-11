# Title: Results-screen presentation helpers (Step 3)
# Pure mappings from data values to display labels + CSS modifier classes for the
# Resultado UI: Brazilian biome tokens, MMA/IUCN threat levels, and taxon
# kingdom. No Shiny here — mod_results.R / mod_geo_verify.R render from these.

#' Normalize a biome token to a lookup key (lowercase alphanumerics only).
#' @noRd
.biome_key <- function(biome) {
    gsub("[^a-z0-9]", "", tolower(as.character(biome)))
}

#' PT-BR display label for a Brazilian biome token (florabr English set)
#'
#' Unknown tokens pass through unchanged.
#' @noRd
biome_label <- function(biome) {
    map <- c(
        amazon = "Amazônia", atlanticforest = "Mata Atlântica",
        caatinga = "Caatinga", cerrado = "Cerrado", pampa = "Pampa",
        pantanal = "Pantanal", sistemacosteiro = "Sistema Costeiro",
        coastal = "Sistema Costeiro"
    )
    orig <- as.character(biome)
    out <- unname(map[.biome_key(biome)])
    out[is.na(out)] <- orig[is.na(out)]
    out
}

#' CSS modifier class giving a biome its representative colour
#'
#' Unknown tokens get the neutral `biome--other`.
#' @noRd
biome_class <- function(biome) {
    map <- c(
        amazon = "biome--amazon", atlanticforest = "biome--atlantic",
        caatinga = "biome--caatinga", cerrado = "biome--cerrado",
        pampa = "biome--pampa", pantanal = "biome--pantanal",
        sistemacosteiro = "biome--coastal", coastal = "biome--coastal"
    )
    out <- unname(map[.biome_key(biome)])
    out[is.na(out)] <- "biome--other"
    out
}

#' Threat-level modifier class from an MMA/IUCN status label
#'
#' Accepts `"(VU) Vulneravel"`, `"(CR (PEX)) ..."`, `"(NE) Não avaliada"` or a
#' bare code like `"VU"`; unknown/`NA` falls back to `threat--ne`.
#' @noRd
threat_status_class <- function(status) {
    s <- as.character(status)
    code <- ifelse(grepl("^\\s*\\(", s), sub("^\\s*\\(([^)]*)\\).*$", "\\1", s), s)
    code <- toupper(trimws(code))
    code <- sub("\\s.*$", "", code)   # "CR (PEX" -> "CR"
    map <- c(
        EX = "threat--ex", EW = "threat--ex", CR = "threat--cr",
        EN = "threat--en", VU = "threat--vu", NT = "threat--nt",
        LC = "threat--lc", DD = "threat--dd", NE = "threat--ne"
    )
    out <- unname(map[code])
    out[is.na(out)] <- "threat--ne"
    out
}

#' Collapse a kingdom to one of the four filter keys
#'
#' The three kingdoms ZHOUSE actually resolves get their own key; anything else
#' lands in `"other"`. A missing kingdom stays `NA` — a pre-validated row with no
#' kingdom matches no taxon filter (it is not "other", it is unknown).
#' @noRd
kingdom_key <- function(kingdom) {
    k <- tolower(trimws(as.character(kingdom)))
    out <- ifelse(k %in% c("animalia", "plantae", "fungi"), k, "other")
    out[is.na(k) | !nzchar(k)] <- NA_character_
    out
}

#' TRUE where an MMA/IUCN status is a threatened (or extinct) level
#'
#' VU/EN/CR plus EX/EW. NT and LC are not threatened, and `NA`/DD/NE (not
#' evaluated, no data) are not claims of threat — they all come back FALSE.
#' @noRd
is_threatened_status <- function(status) {
    threat_status_class(status) %in%
        c("threat--vu", "threat--en", "threat--cr", "threat--ex")
}

#' Filter the Step-3 results view (pure)
#'
#' Four independent dimensions over [build_results_view()]. Values *within* a
#' dimension are an OR (any selected flag matches); the dimensions themselves
#' combine with AND. A `NULL`/empty dimension does not constrain, so no filter at
#' all returns the view untouched.
#'
#' @param view A [build_results_view()] frame.
#' @param flags Verbatim `distributionFlag` levels (SPEC §8).
#' @param kingdoms Taxon keys from [kingdom_key()]: animalia/plantae/fungi/other.
#' @param conservation `"ameacada"` (MMA/IUCN threatened) and/or `"invasora"`
#'   (listed as an invasive alien species).
#' @param match_types PT-BR `matchType` labels; `"já validado"` selects the rows
#'   the cascade never saw (`matchType` is `NA` there).
#' @param areas Study-area names ([assign_record_areas()]); selects the records
#'   linked to those areas.
#' @return The filtered frame, row order preserved.
#' @noRd
filter_results_view <- function(view, flags = NULL, kingdoms = NULL,
                                conservation = NULL, match_types = NULL,
                                areas = NULL) {
    if (!is.data.frame(view) || nrow(view) == 0L) {
        return(view)
    }
    keep <- rep(TRUE, nrow(view))

    if (length(areas) > 0L) {
        av <- if ("area" %in% names(view)) as.character(view$area) else rep(NA_character_, nrow(view))
        keep <- keep & !is.na(av) & av %in% areas
    }

    if (length(flags) > 0L) {
        keep <- keep & as.character(view$distributionFlag) %in% flags
    }
    if (length(kingdoms) > 0L) {
        keep <- keep & kingdom_key(view$kingdom) %in% kingdoms
    }
    if (length(conservation) > 0L) {
        hit <- rep(FALSE, nrow(view))
        if ("ameacada" %in% conservation) {
            hit <- hit | is_threatened_status(view$status)
        }
        if ("invasora" %in% conservation) {
            hit <- hit | (!is.na(view$invasive) & as.logical(view$invasive))
        }
        keep <- keep & hit
    }
    if (length(match_types) > 0L) {
        mt <- as.character(view$matchType)
        mt[is.na(mt)] <- "já validado"
        keep <- keep & mt %in% match_types
    }

    out <- view[keep, , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' The static filter universe for the results screen (pure)
#'
#' Every option the Resultado filter bar can ever show, in a fixed order. One
#' Shiny input per dimension (the list name), one checkbox value per option key.
#' `results_filter_options()` adds the counts; the module only renders what it is
#' handed.
#'
#' The `distributionFlag` pill labels are the verbatim SPEC §8 categories, except
#' the 45-character one, which is shortened for the pill and carries the verbatim
#' text in its tooltip (`title`) — the vocabulary is unchanged, only the chrome.
#'
#' `areas` is the one dynamic dimension: the study areas come from the upload,
#' not from a fixed vocabulary. It leads the bar because it is the broadest cut,
#' and it disappears entirely (zero options) on a single-area upload.
#'
#' @param areas Study-area names present in the view, or `NULL`.
#' @noRd
results_filter_dimensions <- function(areas = NULL) {
    lv <- distribution_flag_levels()
    area_keys <- unique(as.character(areas))
    area_keys <- area_keys[!is.na(area_keys) & nzchar(area_keys)]
    dims <- list(
        areas = list(
            label = "Área",
            options = data.frame(
                key = area_keys,
                label = area_keys,
                title = if (length(area_keys) == 0L) {
                    character(0)
                } else {
                    sprintf("Registros vinculados à área \"%s\"", area_keys)
                },
                class = rep("filter-pill--area", length(area_keys)),
                stringsAsFactors = FALSE
            )
        ),
        flags = list(
            label = "Distribuição",
            options = data.frame(
                key = unname(lv),
                label = c("confirmada", "presente no estado/bioma",
                          "sem registro no estado/bioma", "sem dados disponíveis"),
                title = unname(lv),
                class = c("filter-pill--confirmed", "filter-pill--present",
                          "filter-pill--outside", "filter-pill--nodata"),
                stringsAsFactors = FALSE
            )
        ),
        kingdoms = list(
            label = "Táxon",
            options = data.frame(
                key = c("animalia", "plantae", "fungi", "other"),
                label = c("Animal", "Planta", "Fungo", "Outros"),
                title = c("Animalia", "Plantae", "Fungi", "Outros reinos"),
                class = c("filter-pill--animal", "filter-pill--plant",
                          "filter-pill--fungi", "filter-pill--other"),
                stringsAsFactors = FALSE
            )
        ),
        conservation = list(
            label = "Conservação",
            options = data.frame(
                key = c("ameacada", "invasora"),
                label = c("ameaçada", "exótica invasora"),
                title = c("Ameaçada segundo MMA/IUCN (VU, EN, CR, EX/EW)",
                          "Consta de uma lista nacional de espécies exóticas invasoras"),
                class = c("filter-pill--threat", "filter-pill--invasive"),
                stringsAsFactors = FALSE
            )
        ),
        match_types = list(
            label = "Status taxonômico",
            options = data.frame(
                key = c("aceito", "sinônimo", "ambíguo",
                        "não encontrado", "já validado"),
                label = c("aceito", "sinônimo", "ambíguo",
                          "não encontrado", "já validado"),
                title = c("Nome aceito pela base", "Sinônimo resolvido para o nome aceito",
                          "Mais de uma correspondência possível",
                          "Nenhuma base reconheceu o nome",
                          "Linha já validada na planilha de entrada"),
                class = c("filter-pill--accepted", "filter-pill--synonym",
                          "filter-pill--ambiguous", "filter-pill--notfound",
                          "filter-pill--validated"),
                stringsAsFactors = FALSE
            )
        )
    )
    dims
}

#' Filter options with their record counts, zero-count options dropped (pure)
#'
#' Counts come from the *unfiltered* view, so the pills (and their numbers) hold
#' still while the user toggles them.
#'
#' @param view A [build_results_view()] frame.
#' @return [results_filter_dimensions()] with a `count` column added to each
#'   option table and the options that match no record removed.
#' @noRd
results_filter_options <- function(view) {
    n <- if (is.data.frame(view)) nrow(view) else 0L
    view_areas <- if (n > 0L && "area" %in% names(view)) as.character(view$area) else character(0)
    dims <- results_filter_dimensions(areas = view_areas)

    tally <- function(values, keys) {
        vapply(keys, function(k) sum(values == k, na.rm = TRUE), integer(1),
               USE.NAMES = FALSE)
    }
    match_type <- if (n > 0L) as.character(view$matchType) else character(0)
    match_type[is.na(match_type)] <- "já validado"

    counts <- list(
        areas = tally(view_areas, dims$areas$options$key),
        flags = if (n > 0L) {
            unname(geo_flag_counts(view$distributionFlag))
        } else {
            integer(nrow(dims$flags$options))
        },
        kingdoms = tally(if (n > 0L) kingdom_key(view$kingdom) else character(0),
                         dims$kingdoms$options$key),
        conservation = c(
            if (n > 0L) sum(is_threatened_status(view$status)) else 0L,
            if (n > 0L) sum(!is.na(view$invasive) & as.logical(view$invasive)) else 0L
        ),
        match_types = tally(match_type, dims$match_types$options$key)
    )

    for (nm in names(dims)) {
        opts <- dims[[nm]]$options
        opts$count <- as.integer(counts[[nm]])
        dims[[nm]]$options <- opts[opts$count > 0L, , drop = FALSE]
    }
    dims
}

#' Colour for the nth study-area polygon (design.md tokens, cycled)
#'
#' The first area keeps the historic forest green, so a single-area upload looks
#' exactly as it did before. The rest are dark, high-contrast tokens that stay
#' distinct from the terracotta of the GBIF occurrence markers.
#'
#' @param i Area index (1-based); vectorized.
#' @return Character vector of hex colours.
#' @noRd
area_colour <- function(i) {
    pal <- c("#1E4620", "#192353", "#1F6688", "#8A5A00", "#6B675C")
    pal[((as.integer(i) - 1L) %% length(pal)) + 1L]
}

#' Taxon (kingdom) → PT-BR label + Font Awesome icon + modifier class
#'
#' Returns `NULL` for a missing kingdom; unknown kingdoms get a generic badge.
#' @noRd
kingdom_badge_parts <- function(kingdom) {
    k <- tolower(trimws(as.character(kingdom)))
    if (length(k) == 0L || is.na(k) || !nzchar(k)) {
        return(NULL)
    }
    switch(
        k,
        animalia = list(label = "Animal", icon = "paw",      cls = "taxon--animal"),
        plantae  = list(label = "Planta", icon = "leaf",     cls = "taxon--plant"),
        fungi    = list(label = "Fungo",  icon = "seedling", cls = "taxon--fungi"),
        list(label = tools::toTitleCase(k), icon = "dna", cls = "taxon--other")
    )
}
