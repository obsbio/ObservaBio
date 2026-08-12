# Title: Provider Template — copy this file to add a new taxonomic base
#
# This is a WORKED, self-contained example of the provider contract (SPEC
# §16.4/§16.5), not a live base: it plugs a tiny in-memory fictional base
# ("Examplia") into `new_provider()` so `expect_valid_provider()` passes as-is.
# It is deliberately NOT listed in `register_default_providers()`
# (R/provider_registry.R) — it is a skeleton to copy, not a shipped provider.
#
# To add a real base:
#   1. Copy this file to R/provider_<name>.R and rename template_* -> <name>_*.
#   2. Replace every `TODO(real base)` piece (load the base, run its lookup, map
#      its raw columns onto the canonical schema).
#   3. If the base is embedded, add data-raw/prep_<name>.R that writes the RDS +
#      *.meta.json to inst/extdata/ (mirror data-raw/prep_florabr.R).
#   4. Register it with one line in register_default_providers().
#   5. Run devtools::test() (includes expect_valid_provider); if green, deploy.
#
# The ONE hard rule: query() must return the canonical 18-column schema. Build a
# partial data frame with whatever columns your base has and let
# normalize_provider_result() fill the rest — you never memorise the schema.
# Mirror R/provider_florabr.R + R/provider_br_helpers.R for the real patterns.

#' Stand-in for the base a real provider would load
#'
#' TODO(real base): delete this. An embedded base is read from
#' `inst/extdata/<name>.rds` via `br_load_data()`; a live-API base needs no
#' fixture (query the API directly in `<name>_query()`).
#'
#' @return A data frame in the fictional base's own (non-canonical) shape.
#' @noRd
.template_fixture <- function() {
    data.frame(
        species       = c("Examplia ficta", "Examplia synonyma"),
        status        = c("valid", "synonym"),          # the base's own vocabulary
        accepted_name = c("Examplia ficta", "Examplia ficta"),
        family        = c("Exampliaceae", "Exampliaceae"),
        states        = c("SP;RJ;MG", "BA"),
        biome         = c("Mata Atlantica;Cerrado", "Caatinga"),
        stringsAsFactors = FALSE
    )
}

#' Map the base's own status vocabulary to the canonical validation_status
#'
#' TODO(real base): translate your base's status column into exactly these four
#' values: "accepted", "synonym", "ambiguous", "not_found". Unknown/NA -> "not_found".
#'
#' @param status Character vector of the base's raw status tokens.
#' @return Character vector of canonical `validation_status` values (NA-safe).
#' @noRd
template_map_status <- function(status) {
    s <- tolower(as.character(status))
    out <- rep("not_found", length(s))
    out[s %in% "valid"] <- "accepted"
    out[s %in% "synonym"] <- "synonym"
    out[s %in% "uncertain"] <- "ambiguous"
    out
}

#' Query the fictional base and return the canonical schema
#'
#' The contract's only hard requirement. Handles empty input, degrades
#' gracefully on failure (returns an empty canonical frame instead of erroring),
#' and finalises the 18-column schema via `normalize_provider_result()`.
#'
#' @param names Character vector of normalized scientific names (from the cascade).
#' @param data Pre-loaded base, or NULL to use the default source.
#' @return Canonical-schema data frame.
#' @noRd
template_query <- function(names, data = NULL) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(empty_canonical_result())
    }

    # A network/data failure inside a base must never crash the app: the cascade
    # treats an empty result as "this provider found nothing" and moves on.
    tryCatch(
        {
            # TODO(real base): replace with your base's lookup, e.g.
            #   base <- data %||% br_load_data("<name>", "<name>.rds")
            #   raw  <- <yourpkg>::check_names(base, names_chr)   # base's own call
            base <- data %||% .template_fixture()
            idx <- match(names_chr, base$species)   # TODO(real base): fuzzy/your own match

            # Build only the columns the base provides; the helper fills the rest.
            mapped <- data.frame(
                query_name        = names_chr,
                scientificName    = base$accepted_name[idx],
                taxonomicStatus   = base$status[idx],
                validation_status = template_map_status(base$status[idx]),
                family            = base$family[idx],
                stringsAsFactors  = FALSE
            )
            normalize_provider_result(mapped, "template")
        },
        error = function(e) empty_canonical_result()
    )
}

#' UF/biome distribution for the fictional base (OPTIONAL slot)
#'
#' Feeds the geographic verification's state/biome cross-check (SPEC §8). Returns
#' one row per unique input name with `;`-separated UF/biome tokens (NA when the
#' base has no data). Mirrors `br_distribution()`.
#'
#' TODO(real base): delete this whole function AND the `distribution =` line in
#' the constructor if your base carries no distribution (GBIF passes
#' `distribution = NULL`).
#'
#' @param names Character vector of species names.
#' @param data Pre-loaded base, or NULL to use the default source.
#' @return Data frame `query_name`/`states`/`biomes` (one row per unique name).
#' @noRd
template_distribution <- function(names, data = NULL) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(data.frame(query_name = character(0), states = character(0),
                          biomes = character(0), stringsAsFactors = FALSE))
    }

    base <- data %||% .template_fixture()
    key_base <- normalize_for_matching(base$species)
    key_in <- normalize_for_matching(names_chr)
    agg <- function(vals) {
        toks <- split_distribution(vals)
        if (length(toks) == 0L) NA_character_ else paste(toks, collapse = ";")
    }
    lookup <- function(col) {
        vapply(key_in, function(k) {
            hit <- which(key_base == k)
            if (length(hit) == 0L) NA_character_ else agg(col[hit])
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    }

    data.frame(query_name = names_chr,
               states = lookup(base$states),
               biomes = lookup(base$biome),
               stringsAsFactors = FALSE)
}

#' Construct the example provider
#'
#' TODO(real base): set a real `id`/`label`/`type`/`priority`, and point
#' `is_available`/`load`/`version` at your base. For an embedded base:
#'   is_available = function() file.exists(br_extdata_path("<name>.rds")),
#'   load         = function() br_load_data("<name>", "<name>.rds"),
#'   version      = function() read_base_meta(br_extdata_path("<name>.rds"))$version
#' For a live-API base:
#'   is_available = function() requireNamespace("<yourpkg>", quietly = TRUE),
#'   load         = function() invisible(NULL),
#'   version      = function() "<API> (live)"
#'
#' @return A `ObservaBio_provider`.
#' @noRd
template_provider <- function() {
    new_provider(
        id = "template",
        label = "Base de Exemplo (template)",
        type = "br",
        priority = 99L,   # far end of the cascade, in case it is ever registered
        query = function(names, data = NULL) template_query(names, data = data),
        is_available = function() TRUE,             # fixture is always present
        load = function() invisible(NULL),          # no-op: nothing to preload
        version = function() "template-example v0",
        distribution = function(names, data = NULL) template_distribution(names, data = data)
    )
}
