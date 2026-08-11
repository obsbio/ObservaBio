# Title: Fauna do Brasil Provider (embedded)
# Reads the embedded Fauna BR base from inst/extdata and queries it with
# faunabr::check_fauna_names(). No download at runtime — the base ships in the
# bundle and is regenerated offline by data-raw/prep_faunabr.R.

.FAUNABR_RDS <- "faunabr_validabio.rds"

#' Query the Fauna BR base with normalized names
#'
#' @param names Character vector of normalized scientific names.
#' @param data Pre-loaded base, or NULL to use the cache.
#' @param max_distance Levenshtein fraction for fuzzy matching.
#' @return Canonical-schema data frame.
#' @noRd
faunabr_query <- function(names, data = NULL, max_distance = 0.1) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(empty_canonical_result())
    }
    if (!requireNamespace("faunabr", quietly = TRUE)) {
        stop("Package 'faunabr' is not installed.")
    }
    if (is.null(data)) {
        data <- br_load_data("faunabr", .FAUNABR_RDS)
    }
    raw <- faunabr::check_fauna_names(
        data = data,
        species = names_chr,
        max_distance = max_distance,
        include_subspecies = TRUE
    )
    # The Fauna do Brasil base carries no `kingdom` column — it is fauna-only.
    br_map_check_result(as.data.frame(raw, stringsAsFactors = FALSE), "faunabr",
                        data = data, default_kingdom = "Animalia")
}

#' Construct the Fauna BR provider
#'
#' @return A `validabio_provider` (priority 2).
#' @noRd
faunabr_provider <- function() {
    new_provider(
        id = "faunabr",
        label = "Fauna do Brasil",
        type = "br",
        priority = 2L,
        query = function(names, data = NULL) faunabr_query(names, data = data),
        is_available = function() file.exists(br_extdata_path(.FAUNABR_RDS)),
        load = function() br_load_data("faunabr", .FAUNABR_RDS),
        version = function() read_base_meta(br_extdata_path(.FAUNABR_RDS))$version,
        distribution = function(names, data = NULL) {
            br_distribution("faunabr", names, data = data,
                            filename = .FAUNABR_RDS, has_biome = FALSE)
        },
        exact_match = function(names, data = NULL) {
            br_exact_match("faunabr", names, data = data, filename = .FAUNABR_RDS)
        }
    )
}
