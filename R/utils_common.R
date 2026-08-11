# Title: Common Utility Functions
# Ported from Saira (R/utils_common.R). Shared, side-effect-free helpers.

#' NULL-coalescing operator
#'
#' @param x,y Values; returns `x` unless it is NULL, otherwise `y`.
#' @return `x` when not NULL, else `y`.
#' @noRd
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Create an in-process cache for RDS data
#'
#' Factory returning a list with $get(), $set(), $reset(), $state(). Eliminates
#' boilerplate across provider data caches (Saira ADR-014 pattern).
#'
#' @param name Character label for debug/logging purposes only.
#' @return Named list with cache operations.
#' @noRd
create_rds_cache <- function(name = "unnamed") {
    env <- new.env(parent = emptyenv())
    env$value <- NULL
    env$path <- NULL
    env$load_count <- 0L

    list(
        get = function() env$value,
        set = function(value, path = NULL) {
            env$value <- value
            if (!is.null(path)) env$path <- path
            env$load_count <- env$load_count + 1L
            invisible(value)
        },
        reset = function() {
            env$value <- NULL
            env$path <- NULL
            env$load_count <- 0L
            invisible(TRUE)
        },
        state = function() {
            list(
                has_value = !is.null(env$value),
                path = env$path,
                load_count = as.integer(env$load_count)
            )
        }
    )
}

#' Check if a value is blank (NULL, NA, empty, or whitespace-only)
#'
#' @param x A scalar value to check.
#' @return TRUE if x is NULL, length-0, NA, or whitespace-only; FALSE otherwise.
#' @noRd
is_blank_value <- function(x) {
    if (is.null(x) || length(x) == 0) {
        return(TRUE)
    }
    if (is.na(x)) {
        return(TRUE)
    }
    nchar(trimws(as.character(x))) == 0
}

#' Vectorized non-empty test
#'
#' Element-wise counterpart to [is_blank_value()] used by the DwC layer to detect
#' pre-filled taxonomy and populated audit fields.
#'
#' @param x Vector coercible to character.
#' @return Logical vector, same length as `x`; NA or whitespace-only is FALSE.
#' @noRd
is_non_empty <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(logical(0))
    }
    x_chr <- as.character(x)
    !is.na(x_chr) & nzchar(trimws(x_chr))
}

#' Fill only the blank entries of `x` from `fallback`
#'
#' Never overwrites a value a provider already supplied — the fallback is a
#' derived best-effort (see `scientific_name_components()`).
#'
#' @param x,fallback Vectors of the same length, coercible to character.
#' @return Character vector: `x` with its blanks taken from `fallback`.
#' @noRd
fill_blank_values <- function(x, fallback) {
    x_chr <- as.character(x)
    blank <- !is_non_empty(x_chr)
    x_chr[blank] <- as.character(fallback)[blank]
    x_chr
}

#' Normalize a string for fuzzy matching
#'
#' Lowercases, transliterates accented characters to ASCII, and replaces all
#' non-alphanumeric characters with spaces. NA inputs propagate as NA.
#'
#' @param x Character scalar or vector to normalize.
#' @return Character scalar/vector of the same length with normalized values.
#' @noRd
normalize_for_matching <- function(x) {
    x_chr <- as.character(x)
    normalized <- tolower(x_chr)
    translit <- iconv(normalized, to = "ASCII//TRANSLIT")
    normalized[!is.na(translit)] <- translit[!is.na(translit)]
    normalized <- gsub("[^a-z0-9]+", " ", normalized)
    trimws(normalized)
}
