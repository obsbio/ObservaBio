# Title: Flora do Brasil Provider (embedded)
# Reads the embedded Flora BR base from inst/extdata and queries it with
# florabr::check_names(). No download/future/polling — the base ships in the
# bundle and is regenerated offline by data-raw/prep_florabr.R.

.FLORABR_RDS <- "florabr_validabio.rds"

#' Query the Flora BR base with normalized names
#'
#' @param names Character vector of normalized scientific names.
#' @param data Pre-loaded base, or NULL to use the cache.
#' @param max_distance Levenshtein fraction for fuzzy matching.
#' @return Canonical-schema data frame.
#' @noRd
florabr_query <- function(names, data = NULL, max_distance = 0.1) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(empty_canonical_result())
    }
    if (!requireNamespace("florabr", quietly = TRUE)) {
        stop("Package 'florabr' is not installed.")
    }
    if (is.null(data)) {
        data <- br_load_data("florabr", .FLORABR_RDS)
    }
    raw <- florabr::check_names(
        data = data,
        species = names_chr,
        max_distance = max_distance,
        include_subspecies = TRUE,
        include_variety = FALSE,
        parallel = FALSE,
        progress_bar = FALSE
    )
    br_map_check_result(as.data.frame(raw, stringsAsFactors = FALSE), "florabr",
                        data = data)
}

#' Construct the Flora BR provider
#'
#' @return A `ObservaBio_provider` (priority 1).
#' @noRd
florabr_provider <- function() {
    new_provider(
        id = "florabr",
        label = "Flora do Brasil",
        type = "br",
        priority = 1L,
        query = function(names, data = NULL) florabr_query(names, data = data),
        is_available = function() file.exists(br_extdata_path(.FLORABR_RDS)),
        load = function() br_load_data("florabr", .FLORABR_RDS),
        version = function() read_base_meta(br_extdata_path(.FLORABR_RDS))$version,
        distribution = function(names, data = NULL) {
            br_distribution("florabr", names, data = data,
                            filename = .FLORABR_RDS, has_biome = TRUE)
        },
        exact_match = function(names, data = NULL) {
            br_exact_match("florabr", names, data = data, filename = .FLORABR_RDS)
        }
    )
}
