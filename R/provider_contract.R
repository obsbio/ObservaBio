# Title: Provider Contract
# The "socket" every taxonomic base plugs into (SPEC §16). The cascade engine,
# UI, and export know only this contract, never the interior of a base.

#' Canonical result schema every provider's query() must return
#'
#' Column order is stable; downstream code relies on these names. Derived from
#' the Saira `normalize_provider_result` schema plus the DwC fields the ZHOUSE
#' scripts emit.
#'
#' @return Character vector of the 18 canonical column names.
#' @noRd
canonical_schema_columns <- function() {
    c(
        "query_name", "scientificName", "taxonomicStatus", "validation_status",
        "match_count", "provider", "taxonID", "taxonRank", "acceptedNameUsageID",
        "kingdom", "phylum", "class", "order", "family", "genus",
        "specificEpithet", "infraspecificEpithet", "vernacularName"
    )
}

#' An empty data frame in the canonical schema (0 rows, correct types)
#'
#' @return Zero-row data frame with all canonical columns.
#' @noRd
empty_canonical_result <- function() {
    out <- data.frame(
        query_name = character(0), scientificName = character(0),
        taxonomicStatus = character(0), validation_status = character(0),
        match_count = integer(0), provider = character(0),
        stringsAsFactors = FALSE
    )
    for (col_name in setdiff(canonical_schema_columns(), names(out))) {
        out[[col_name]] <- character(0)
    }
    out[, canonical_schema_columns(), drop = FALSE]
}

#' Coerce a provider's raw/partial result into the canonical schema
#'
#' Generalises Saira's `normalize_brprovider_result`: the base-specific mapping
#' (e.g. florabr `Spelling` -> `validation_status`) is done by the provider; this
#' helper only guarantees the schema — fills missing columns with NA, defaults
#' `validation_status`/`match_count`, falls back `scientificName` to the query,
#' stamps `provider`, and returns columns in canonical order. So a base author
#' does not have to memorise the full 18-column schema.
#'
#' @param df Data frame with at least `query_name`; other canonical columns
#'   optional.
#' @param provider Provider id to stamp into the `provider` column.
#' @return Data frame in the canonical schema.
#' @noRd
normalize_provider_result <- function(df, provider) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
        out <- empty_canonical_result()
        return(out)
    }

    names(df) <- trimws(names(df))
    if (!"query_name" %in% names(df)) {
        stop("normalize_provider_result(): df must include a 'query_name' column.")
    }

    n <- nrow(df)
    for (col_name in setdiff(canonical_schema_columns(), names(df))) {
        df[[col_name]] <- NA
    }

    df$provider <- as.character(provider)

    df$query_name <- as.character(df$query_name)

    df$validation_status <- as.character(df$validation_status)
    df$validation_status[is.na(df$validation_status) | !nzchar(df$validation_status)] <- "not_found"

    sci <- as.character(df$scientificName)
    blank_sci <- is.na(sci) | !nzchar(trimws(sci))
    sci[blank_sci] <- df$query_name[blank_sci]
    df$scientificName <- sci

    mc <- suppressWarnings(as.integer(df$match_count))
    mc[is.na(mc)] <- as.integer(df$validation_status != "not_found")
    df$match_count <- mc

    # The BR bases answer with a match verdict (accepted name + family), not the
    # parsed name parts, so derive them from the accepted name here — at the
    # contract boundary, so every provider benefits. Fills blanks only: a base
    # that does supply these keeps its own values.
    parts <- scientific_name_components(df$scientificName)
    df$genus <- fill_blank_values(df$genus, parts$genus)
    df$specificEpithet <- fill_blank_values(df$specificEpithet, parts$specificEpithet)
    df$taxonRank <- fill_blank_values(df$taxonRank, parts$taxonRank)

    df <- df[, canonical_schema_columns(), drop = FALSE]
    rownames(df) <- NULL
    df
}

#' Construct a provider object
#'
#' Every taxonomic base is created by this constructor and exposes the fixed
#' interface the engine relies on (SPEC §16.2). Adding a base means writing one
#' of these and registering it — nothing in the engine changes.
#'
#' @param id Internal identifier (e.g. "florabr").
#' @param label PT-BR display name shown in the UI.
#' @param type "br" (embedded), "global" (live API), or a custom tag.
#' @param priority Cascade order; lower is queried first.
#' @param query Function `(names, data = NULL)` returning the canonical schema.
#' @param is_available Function `()` -> logical: data present / API reachable.
#' @param load Function `()` loading embedded data (no-op for live APIs).
#' @param version Function `()` -> character base version.
#' @param distribution Optional function `(names)` returning a data frame
#'   `query_name`/`states`/`biomes` (one row per unique input name; `;`-separated
#'   UF/biome tokens, `NA` when the base has no data). Feeds the geographic
#'   verification's `geo_crosscheck_distribution()`. `NULL` when the base carries
#'   no distribution (e.g. GBIF).
#' @param exact_match Optional function `(names)` returning the subset of `names`
#'   the base holds verbatim, cheaply and without fuzzy matching. Lets
#'   `run_cascade()` skip a higher-priority base's expensive fuzzy pass for a name
#'   a lower-priority base already carries. `NULL` (the default) simply opts the
#'   base out — it is then queried with everything, exactly as before.
#' @return An object of class "ObservaBio_provider".
#' @noRd
new_provider <- function(id, label, type, priority, query,
                         is_available = function() TRUE,
                         load = function() invisible(NULL),
                         version = function() NA_character_,
                         distribution = NULL,
                         exact_match = NULL) {
    if (!is.function(query)) {
        stop("new_provider(): 'query' must be a function.")
    }
    provider <- list(
        id = as.character(id),
        label = as.character(label),
        type = as.character(type),
        priority = as.integer(priority),
        query = query,
        is_available = is_available,
        load = load,
        version = version,
        distribution = distribution,
        exact_match = exact_match
    )
    class(provider) <- "ObservaBio_provider"
    provider
}
