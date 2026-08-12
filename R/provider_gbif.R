# Title: GBIF Backbone Provider (live)
# The cascade fallback (SPEC ADR-002). Queries the GBIF backbone live via
# rgbif::name_backbone_checklist(); no local store (taxadb removed). Only the
# residual names that Flora/Fauna BR did not accept reach this provider.

.gbif_cache <- new.env(parent = emptyenv())

#' Map a name_backbone_checklist row's GBIF status to validation_status
#' @noRd
gbif_validation_status <- function(status, match_type) {
    status <- toupper(as.character(status %||% ""))
    match_type <- toupper(as.character(match_type %||% ""))
    if (identical(match_type, "NONE") || !nzchar(status)) {
        return("not_found")
    }
    if (identical(status, "ACCEPTED")) return("accepted")
    if (identical(status, "SYNONYM")) return("synonym")
    "ambiguous"
}

#' Query the GBIF backbone for a vector of normalized names
#'
#' @param names Character vector of normalized scientific names.
#' @return Canonical-schema data frame. On a network/API error the caller
#'   (the cascade) catches and logs it; these names then fall to `not_found`.
#' @noRd
gbif_query <- function(names) {
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(empty_canonical_result())
    }
    if (!requireNamespace("rgbif", quietly = TRUE)) {
        stop("Package 'rgbif' is not installed.")
    }

    cached <- names_chr[vapply(names_chr, function(n) !is.null(.gbif_cache[[n]]), logical(1))]
    to_query <- setdiff(names_chr, cached)

    if (length(to_query) > 0L) {
        raw <- rgbif::name_backbone_checklist(to_query)
        raw <- as.data.frame(raw, stringsAsFactors = FALSE)
        mapped <- gbif_map_backbone(raw, to_query)
        for (nm in to_query) {
            .gbif_cache[[nm]] <- mapped[mapped$query_name == nm, , drop = FALSE]
        }
    }

    parts <- lapply(names_chr, function(n) .gbif_cache[[n]])
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0L) {
        return(empty_canonical_result())
    }
    do.call(rbind, parts)
}

#' Map a name_backbone_checklist result to the canonical schema
#'
#' @param raw Data frame from `rgbif::name_backbone_checklist()`.
#' @param queried Character vector of names sent (to recover any dropped rows).
#' @return Canonical-schema data frame.
#' @noRd
gbif_map_backbone <- function(raw, queried) {
    if (!is.data.frame(raw) || nrow(raw) == 0L) {
        return(build_cascade_placeholder(queried))
    }
    n <- nrow(raw)
    col <- function(name) if (name %in% names(raw)) as.character(raw[[name]]) else rep(NA_character_, n)

    query_name <- col("verbatim_name")
    if (all(is.na(query_name)) && length(queried) == n) {
        query_name <- as.character(queried)
    }

    status <- col("status")
    match_type <- col("matchType")
    validation_status <- vapply(seq_len(n), function(i) {
        gbif_validation_status(status[[i]], match_type[[i]])
    }, FUN.VALUE = character(1))

    scientific <- col("canonicalName")
    fallback_sci <- col("scientificName")
    blank <- is.na(scientific) | !nzchar(scientific)
    scientific[blank] <- fallback_sci[blank]

    out <- data.frame(
        query_name = query_name,
        scientificName = scientific,
        taxonomicStatus = tolower(status),
        validation_status = validation_status,
        taxonID = col("usageKey"),
        acceptedNameUsageID = col("acceptedUsageKey"),
        taxonRank = tolower(col("rank")),
        kingdom = col("kingdom"),
        phylum = col("phylum"),
        class = col("class"),
        order = col("order"),
        family = col("family"),
        genus = col("genus"),
        stringsAsFactors = FALSE
    )
    normalize_provider_result(out, "gbif")
}

#' Construct the GBIF backbone provider
#'
#' @return A `ObservaBio_provider` (priority 3, type "global").
#' @noRd
gbif_provider <- function() {
    new_provider(
        id = "gbif",
        label = "GBIF (backbone global)",
        type = "global",
        priority = 3L,
        query = function(names, data = NULL) gbif_query(names),
        is_available = function() requireNamespace("rgbif", quietly = TRUE),
        load = function() invisible(NULL),
        version = function() "GBIF Backbone Taxonomy (live)"
    )
}
