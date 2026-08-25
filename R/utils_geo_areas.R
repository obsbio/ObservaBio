# Title: Study-Area Linking (locality <-> area file)
# Pure helpers that decide WHICH study area each uploaded record belongs to. One
# file is one area; the user links each area to one or more `locality` values
# in Step 1 (mod_upload). The geographic verification then runs per area, over
# only the species that area actually claims — so the distributionFlag becomes a
# per-(species, area) answer instead of a per-species one. Records the user did
# not link are NOT verified (a deliberate client rule), except for the
# single-area fallback below. No Shiny here.

#' Normalize an area/locality label to a matching key
#'
#' Thin wrapper over [normalize_for_matching()]: the link between an area file's
#' name and a `locality` value must survive case, accents and punctuation
#' ("RPPN Rio do Brasil" == "rppn_rio_do_brasil").
#' @noRd
area_key <- function(x) {
    normalize_for_matching(x)
}

#' Stable per-area identifier, safe to embed in an input id
#'
#' [area_key()] with spaces folded to underscores. Two areas share a slug exactly
#' when they are the same area by name, so the slug is what identifies an area
#' across uploads (see [merge_areas()]) and what keys its row's inputs — an
#' index would shift under the user when an earlier area is removed.
#' @noRd
area_slug <- function(x) {
    slug <- gsub(" ", "_", area_key(x), fixed = TRUE)
    slug[is.na(slug) | !nzchar(slug)] <- "area"
    slug
}

#' Merge a fresh upload into the areas already held (pure)
#'
#' Shiny's `fileInput` replaces its selection on every upload, so sending a
#' second area in a second action would silently drop the first. The module
#' therefore keeps what it has and merges each upload in: an incoming area whose
#' slug is already held **replaces** it (re-sending a corrected file is the
#' natural way to fix an area, and replacing keeps its row in place), and
#' anything new is appended in upload order.
#'
#' @param held List of areas already accumulated.
#' @param incoming List of areas from this upload (from [unpack_area_files()]).
#' @return The merged list, held order first.
#' @noRd
merge_areas <- function(held, incoming) {
    if (length(incoming) == 0L) {
        return(held)
    }
    out <- held
    slugs <- vapply(out, function(a) area_slug(a$name), character(1))
    for (a in incoming) {
        at <- match(area_slug(a$name), slugs)
        if (is.na(at)) {
            out[[length(out) + 1L]] <- a
            slugs <- c(slugs, area_slug(a$name))
        } else {
            out[[at]] <- a
        }
    }
    out
}

#' Drop one area by slug (pure) — the panel's remove control.
#' @noRd
drop_area <- function(held, slug) {
    if (length(held) == 0L || is.null(slug) || !nzchar(slug)) {
        return(held)
    }
    keep <- vapply(held, function(a) !identical(area_slug(a$name), slug), logical(1))
    held[keep]
}

#' The `locality` values an uploaded area claims, cleaned.
#' @noRd
.area_localities <- function(area) {
    loc <- as.character(area$localities %||% character(0))
    loc[!is.na(loc) & nzchar(trimws(loc))]
}

#' Map every uploaded area's claimed localities to a lookup key -> area name
#'
#' A `locality` claimed by two different areas is a configuration mistake, not a
#' many-to-many relation: the key is dropped so those records stay unlinked and
#' surface in the Step-2 warning instead of being silently assigned to whichever
#' area came first.
#' @noRd
.area_locality_lookup <- function(areas) {
    keys <- character(0)
    owners <- character(0)
    for (a in areas) {
        loc <- .area_localities(a)
        if (length(loc) == 0L) next
        keys <- c(keys, area_key(loc))
        owners <- c(owners, rep(as.character(a$name), length(loc)))
    }
    if (length(keys) == 0L) {
        return(stats::setNames(character(0), character(0)))
    }
    lookup <- stats::setNames(owners, keys)
    lookup <- lookup[!duplicated(paste(keys, owners))]     # same claim twice is fine
    conflicted <- names(lookup)[duplicated(names(lookup))]
    lookup[!names(lookup) %in% conflicted]
}

#' Assign a study area to every uploaded record (pure)
#'
#' Rules, in order:
#'   1. `locality` present and at least one area claims localities -> match by
#'      [area_key()]; anything unclaimed stays `NA`.
#'   2. Fallback — exactly one area and nothing to link it by (no `locality`
#'      column, or the user linked nothing) -> that area claims every record.
#'      This preserves the pre-multi-area behaviour for the simple upload.
#'   3. Otherwise `NA`: not linked, so not geo-verified.
#'
#' @param records Uploaded records (one row per record).
#' @param areas List of areas (`name`, and optionally `localities`).
#' @param locality_col Column carrying the locality label. Default "locality".
#' @return Character vector, `nrow(records)` long: the area name per record, or
#'   `NA` where the record is not linked to any area.
#' @noRd
assign_record_areas <- function(records, areas, locality_col = "locality") {
    n <- if (is.data.frame(records)) nrow(records) else 0L
    out <- rep(NA_character_, n)
    if (n == 0L || length(areas) == 0L) {
        return(out)
    }

    lookup <- .area_locality_lookup(areas)
    has_col <- locality_col %in% names(records)

    if (!has_col || length(lookup) == 0L) {
        if (length(areas) == 1L) {
            return(rep(as.character(areas[[1L]]$name), n))
        }
        return(out)
    }

    unname(lookup[area_key(records[[locality_col]])])
}

#' The `locality` values that ended up with no study area (pure)
#'
#' Feeds the Step-2 warning: these records exist but will not be geo-verified.
#'
#' @param records Uploaded records.
#' @param record_areas Result of [assign_record_areas()].
#' @param locality_col Column carrying the locality label.
#' @return Sorted character vector of distinct unlinked locality values (empty
#'   when every record is linked, or when there is no locality column to name).
#' @noRd
unlinked_localities <- function(records, record_areas, locality_col = "locality") {
    if (!is.data.frame(records) || nrow(records) == 0L ||
        !locality_col %in% names(records)) {
        return(character(0))
    }
    loc <- as.character(records[[locality_col]])
    orphan <- is.na(record_areas) & !is.na(loc) & nzchar(trimws(loc))
    sort(unique(trimws(loc[orphan])))
}

#' Build the per-area (query_name, accepted scientificName) set to verify
#'
#' The per-area counterpart of [geo_name_map()]: instead of one row per species
#' for the whole upload, one row per (area, species) — so each area's GBIF and
#' distribution lookups run over only the species its own records carry. The
#' accepted-name resolution is not duplicated here: [geo_name_map()] builds the
#' `query_name -> accepted name` lookup once (cascade wins over the raw sheet
#' name, so synonyms are resolved), and this function only slices it by area.
#'
#' @param records Uploaded records (one row per record).
#' @param cascade Cascade result (`query_name` + `scientificName`), or NULL.
#' @param record_areas Result of [assign_record_areas()].
#' @param name_col Name column in `records`. Default "scientificName".
#' @return Data frame `area`/`query_name`/`scientificName`, one row per
#'   (area, species). Unlinked records contribute nothing.
#' @noRd
geo_name_map_by_area <- function(records, cascade, record_areas,
                                 name_col = "scientificName") {
    empty <- data.frame(area = character(0), query_name = character(0),
                        scientificName = character(0), stringsAsFactors = FALSE)
    if (!is.data.frame(records) || nrow(records) == 0L ||
        !name_col %in% names(records) || length(record_areas) != nrow(records)) {
        return(empty)
    }

    raw <- as.character(records[[name_col]])
    keys <- vapply(raw, normalize_scientific_name,
                   FUN.VALUE = character(1), USE.NAMES = FALSE)
    keep <- !is.na(record_areas) & nzchar(record_areas) &
        !is.na(keys) & nzchar(keys)
    if (!any(keep)) {
        return(empty)
    }

    lookup <- geo_name_map(records, cascade, name_col = name_col)
    idx <- match(keys[keep], lookup$query_name)
    accepted <- lookup$scientificName[idx]
    blank <- is.na(accepted) | !nzchar(accepted)
    accepted[blank] <- raw[keep][blank]

    out <- data.frame(
        area = as.character(record_areas[keep]),
        query_name = keys[keep],
        scientificName = accepted,
        stringsAsFactors = FALSE
    )
    out <- out[!duplicated(paste(out$area, out$query_name)), , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Composite join key for the per-(species, area) distributionFlag
#'
#' The flag stopped being a per-species answer once one upload can carry several
#' areas, so every join that used to key on `query_name` alone now keys on the
#' area too. `NA`/unlinked areas produce `NA` — those records get no flag.
#'
#' @param area_name Area name(s); recycled against `query_name`.
#' @param query_name Normalized query name(s).
#' @return Character vector of keys, `NA` where the area is missing.
#' @noRd
area_flag_key <- function(area_name, query_name) {
    # paste0() recycles a zero-length argument to "", which would invent a key.
    if (length(area_name) == 0L || length(query_name) == 0L) {
        return(character(0))
    }
    a <- area_key(area_name)
    q <- as.character(query_name)
    out <- paste0(a, "|", q)
    out[is.na(a) | !nzchar(a) | is.na(q) | !nzchar(q)] <- NA_character_
    out
}
