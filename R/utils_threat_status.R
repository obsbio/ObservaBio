# Title: Conservation Status (MMA + IUCN)
# Two independent status sources (SPEC §5):
#   - MMA: offline lookup against the embedded sensitive_species.rds (Portaria
#     148/2022 + correlatas), matched by normalized key; most restrictive wins.
#   - IUCN: global Red List category via GBIF's keyless species API (ported from
#     Saira utils_threat_status.R). Every IUCN path degrades to NA — httr2 error,
#     offline, or an unexpected payload must never break the pipeline.
# Both are memoized in the package environment — which every session in the R
# process shares, so anything key-dependent must be namespaced (ADR-005).

# ---------------------------------------------------------------------------
# MMA (Portaria 148/2022 + correlatas) — offline
# ---------------------------------------------------------------------------

.mma_cache <- create_rds_cache("mma_sensitive")

# Severity order, most restrictive first (Portaria categories).
.mma_severity <- c("CR (PEX)" = 5L, "CR" = 4L, "EN" = 3L, "VU" = 2L, "NT" = 1L)

#' Load and cache the embedded MMA sensitive-species base
#'
#' @return Data frame with `scientificName`, `match_key`, `category`, `source`.
#' @noRd
mma_load_data <- function() {
    cached <- .mma_cache$get()
    if (!is.null(cached)) {
        return(cached)
    }
    path <- br_extdata_path("sensitive_species.rds")
    if (!file.exists(path)) {
        stop(sprintf("MMA base not found at '%s'. Run data-raw/generate_sensitive_species.R.", path))
    }
    data <- readRDS(path)
    .mma_cache$set(data)
    data
}

#' MMA threat status for a vector of scientific names
#'
#' Matches on the normalized key; when a name maps to several rows (different
#' portarias) the most restrictive category wins and its source is reported.
#'
#' @param scientific_names Character vector.
#' @return Data frame with `scientificName`, `statusMMA`, `statusSourceMMA`
#'   (NA where the species is not MMA-listed), same length/order as input.
#' @noRd
mma_lookup <- function(scientific_names) {
    n <- length(scientific_names)
    out <- data.frame(
        scientificName  = as.character(scientific_names),
        statusMMA       = rep(NA_character_, n),
        statusSourceMMA = rep(NA_character_, n),
        stringsAsFactors = FALSE
    )
    if (n == 0L) {
        return(out)
    }

    data <- tryCatch(mma_load_data(), error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0L) {
        return(out)
    }

    keys <- normalize_for_matching(scientific_names)
    data_sev <- unname(.mma_severity[data$category])
    data_sev[is.na(data_sev)] <- 0L

    for (i in seq_len(n)) {
        if (is.na(keys[[i]]) || !nzchar(keys[[i]])) {
            next
        }
        hits <- which(data$match_key == keys[[i]])
        if (length(hits) == 0L) {
            next
        }
        best <- hits[[which.max(data_sev[hits])]]
        out$statusMMA[[i]] <- data$category[[best]]
        out$statusSourceMMA[[i]] <- data$source[[best]]
    }
    out
}

# ---------------------------------------------------------------------------
# IUCN Red List category — keyless GBIF species API (ported from Saira)
# ---------------------------------------------------------------------------

.gbif_iucn_cache <- create_rds_cache("gbif_iucn")
.gbif_match_cache <- create_rds_cache("gbif_match")

GBIF_API_BASE <- "https://api.gbif.org/v1"
GBIF_API_TIMEOUT_S <- 10

# The feature is opt-in through httr2: absent httr2 disables every call.
has_httr2 <- function() requireNamespace("httr2", quietly = TRUE)

# GET against the GBIF API; parsed JSON list or NULL on any failure.
gbif_api_get <- function(segments, query = NULL) {
    if (!has_httr2()) {
        return(NULL)
    }
    tryCatch(
        {
            req <- httr2::request(GBIF_API_BASE)
            req <- do.call(httr2::req_url_path_append, c(list(req), as.list(segments)))
            if (length(query) > 0L) {
                req <- do.call(httr2::req_url_query, c(list(req), query))
            }
            req <- httr2::req_timeout(req, GBIF_API_TIMEOUT_S)
            req <- httr2::req_user_agent(req, "ObservaBio R package")
            req <- httr2::req_error(req, is_error = function(resp) FALSE)
            resp <- httr2::req_perform(req)
            if (httr2::resp_status(resp) != 200L) {
                return(NULL)
            }
            httr2::resp_body_json(resp)
        },
        error = function(e) NULL
    )
}

# Read a single scalar string field from a parsed GBIF body, or NA.
gbif_body_field <- function(body, field) {
    if (!is.list(body)) {
        return(NA_character_)
    }
    value <- body[[field]]
    if (length(value) != 1L) {
        return(NA_character_)
    }
    value <- as.character(value)
    if (is.na(value) || !nzchar(value)) NA_character_ else value
}

#' IUCN Red List category codes for GBIF usage keys
#'
#' Reads `GET /species/{key}/iucnRedListCategory` and returns the short `code`
#' (e.g. "NT", "VU", "EN"). Missing keys, unassessed taxa, absent httr2, offline,
#' or any API error yield NA. Memoized per session.
#'
#' @param usage_keys Character/numeric vector of GBIF usage keys (NA allowed).
#' @return Character vector of IUCN codes (NA where unavailable), same length.
#' @noRd
fetch_gbif_iucn_category <- function(usage_keys) {
    n <- length(usage_keys)
    if (n == 0L) {
        return(character(0))
    }
    keys <- as.character(usage_keys)
    out <- rep(NA_character_, n)
    valid <- !is.na(keys) & nzchar(keys)
    if (!any(valid) || !has_httr2()) {
        return(out)
    }
    memo <- .gbif_iucn_cache$get()
    if (is.null(memo)) {
        memo <- character(0)
    }
    misses <- setdiff(unique(keys[valid]), names(memo))
    for (k in misses) {
        body <- gbif_api_get(c("species", k, "iucnRedListCategory"))
        memo[[k]] <- gbif_body_field(body, "code")
    }
    if (length(misses) > 0L) {
        .gbif_iucn_cache$set(memo)
    }
    out[valid] <- unname(memo[keys[valid]])
    out
}

#' Resolve scientific names to GBIF usage keys (match fallback)
#'
#' For names lacking a GBIF-resolved `taxonID` (e.g. rows a Brazilian provider
#' matched). Calls the keyless `GET /species/match?name=`. Same degradation and
#' memoization as [fetch_gbif_iucn_category()].
#'
#' @param names Character vector of scientific names (NA allowed).
#' @return Character vector of GBIF usage keys (NA where unmatched), same length.
#' @noRd
gbif_match_usage_keys <- function(names) {
    n <- length(names)
    if (n == 0L) {
        return(character(0))
    }
    nm <- as.character(names)
    out <- rep(NA_character_, n)
    valid <- !is.na(nm) & nzchar(trimws(nm))
    if (!any(valid) || !has_httr2()) {
        return(out)
    }
    memo <- .gbif_match_cache$get()
    if (is.null(memo)) {
        memo <- character(0)
    }
    misses <- setdiff(unique(nm[valid]), names(memo))
    for (q in misses) {
        body <- gbif_api_get(c("species", "match"), query = list(name = q))
        memo[[q]] <- gbif_body_field(body, "usageKey")
    }
    if (length(misses) > 0L) {
        .gbif_match_cache$set(memo)
    }
    out[valid] <- unname(memo[nm[valid]])
    out
}

#' IUCN category for names, using taxonID when present and matching otherwise
#'
#' @param scientific_names Character vector of names.
#' @param taxon_ids GBIF usage keys aligned to `scientific_names` (NA allowed).
#' @return Character vector of IUCN codes (NA where unavailable).
#' @noRd
iucn_category <- function(scientific_names, taxon_ids = NA) {
    n <- length(scientific_names)
    if (n == 0L) {
        return(character(0))
    }
    keys <- rep(NA_character_, n)
    if (length(taxon_ids) == n) {
        keys <- as.character(taxon_ids)
    }
    need_match <- is.na(keys) | !nzchar(keys)
    if (any(need_match)) {
        keys[need_match] <- gbif_match_usage_keys(scientific_names[need_match])
    }
    fetch_gbif_iucn_category(keys)
}

# ---------------------------------------------------------------------------
# IUCN Red List category + criteria — rredlist key route (ADR-005)
# ---------------------------------------------------------------------------
# The keyless GBIF route above returns the category but not `criteria`. The
# ZHOUSE output model has a `criteria` column, so when a key is available and
# rredlist is installed, we query `rredlist::rl_species_latest()` to fill both.
# Two key sources (ADR-005): the user's own key, pasted in the optional Step 1
# field and living only in that Shiny session, or the server's `IUCN_KEY` secret
# as fallback. Absent key/package/network, this degrades to NA and the caller
# falls back to the keyless category.

.iucn_rredlist_cache <- create_rds_cache("iucn_rredlist")

# rredlist is an Imports dependency (ADR-005: Suggests silently dropped it from
# the deploy manifest). The guard stays as cheap defence: absent, the key route
# is off and every row falls back to the keyless category.
has_rredlist <- function() requireNamespace("rredlist", quietly = TRUE)

# The IUCN Red List API key configured on the server (shinyapps.io secret).
iucn_key <- function() Sys.getenv("IUCN_KEY", unset = "")

# Normalize a key input (NULL, NA, whitespace, vector) to a single string.
as_iucn_key <- function(key) {
    if (is.null(key) || length(key) == 0L) {
        return("")
    }
    key <- as.character(key)[[1]]
    if (is.na(key)) {
        return("")
    }
    trimws(key)
}

# The key to use for one run: the session key the user pasted in Step 1 when
# supplied, else the server secret. Never persisted or logged.
iucn_key_for_run <- function(key = NULL) {
    key <- as_iucn_key(key)
    if (nzchar(key)) key else iucn_key()
}

# Whether the rredlist key route is usable (package present + a key to use).
iucn_key_route_enabled <- function(key = iucn_key()) {
    has_rredlist() && nzchar(as_iucn_key(key))
}

# The rredlist memo lives in the package environment, so every session in the R
# process shares it (shinyapps.io serves many users from one process). Entries
# are therefore namespaced by a non-reversible fingerprint of the key: a run
# under one key can neither serve nor — when the key is invalid and every lookup
# fails to NA — poison a run under another. The key itself never enters the cache.
iucn_cache_ns <- function(key) substr(rlang::hash(key), 1L, 12L)

# Pull (category, criteria) out of an rl_species_latest() payload; NA on any
# unexpected shape. Mirrors the extraction in the reference scripts.
extract_rredlist_assessment <- function(res) {
    empty <- list(category = NA_character_, criteria = NA_character_)
    if (is.null(res)) {
        return(empty)
    }
    if (is.data.frame(res) && nrow(res) > 0L) {
        return(list(
            category = if ("category" %in% names(res)) as.character(res$category[[1]]) else NA_character_,
            criteria = if ("criteria" %in% names(res)) as.character(res$criteria[[1]]) else NA_character_
        ))
    }
    if (is.list(res)) {
        category <- NA_character_
        if (!is.null(res$category)) {
            category <- as.character(res$category[[1]])
        } else if (!is.null(res$red_list_category) && !is.null(res$red_list_category$code)) {
            category <- as.character(res$red_list_category$code[[1]])
        }
        criteria <- if (!is.null(res$criteria)) as.character(res$criteria[[1]]) else NA_character_
        return(list(category = category, criteria = criteria))
    }
    empty
}

#' IUCN category + criteria via the rredlist key route
#'
#' For each `genus`/`specific` (+ optional `infra`) triple, calls
#' `rredlist::rl_species_latest()` once, memoized by the triple **and** the key's
#' fingerprint. Rows without a genus+species, or every row when the key route is
#' disabled, get NA.
#'
#' @param genus,specific,infra Character vectors, aligned and equal length.
#' @param key IUCN Red List API key. Defaults to the server secret.
#' @return Data frame with `iucnCategory`, `iucnCriteria`, same length/order.
#' @noRd
fetch_iucn_rredlist <- function(genus, specific, infra = NA, key = iucn_key()) {
    n <- length(genus)
    out <- data.frame(
        iucnCategory = rep(NA_character_, n),
        iucnCriteria = rep(NA_character_, n),
        stringsAsFactors = FALSE
    )
    key <- as_iucn_key(key)
    if (n == 0L || !iucn_key_route_enabled(key)) {
        return(out)
    }
    g <- as.character(genus)
    s <- as.character(specific)
    i <- if (length(infra) == n) as.character(infra) else rep(NA_character_, n)
    valid <- is_non_empty(g) & is_non_empty(s)
    if (!any(valid)) {
        return(out)
    }

    memo <- .iucn_rredlist_cache$get()
    if (is.null(memo)) {
        memo <- list()
    }
    triples <- paste(iucn_cache_ns(key), g, s, ifelse(is.na(i), "", i), sep = "")
    for (idx in which(valid)) {
        tkey <- triples[[idx]]
        if (is.null(memo[[tkey]])) {
            res <- tryCatch(
                rredlist::rl_species_latest(
                    genus = g[[idx]], species = s[[idx]],
                    infra = if (is_non_empty(i[[idx]])) i[[idx]] else NULL,
                    key = key, parse = TRUE
                ),
                error = function(e) NULL
            )
            memo[[tkey]] <- extract_rredlist_assessment(res)
        }
        out$iucnCategory[[idx]] <- memo[[tkey]]$category
        out$iucnCriteria[[idx]] <- memo[[tkey]]$criteria
    }
    .iucn_rredlist_cache$set(memo)
    out
}

# ---------------------------------------------------------------------------
# Combined
# ---------------------------------------------------------------------------

#' Attach MMA + IUCN status columns to a cascade result
#'
#' MMA is always resolved offline. For IUCN, the rredlist **key route** is used
#' when a key is available and rredlist is installed (fills both `iucnCategory`
#' and `iucnCriteria` from genus/specificEpithet); otherwise it falls back to the
#' keyless GBIF category (criteria stays NA). All IUCN paths degrade to NA.
#'
#' @param df Data frame with at least `scientificName`; `taxonID`,
#'   `genus`/`specificEpithet`/`infraspecificEpithet` used when present.
#' @param include_iucn Whether to resolve IUCN status. Default TRUE.
#' @param iucn_key The user's IUCN Red List API key for this run (the optional
#'   Step 1 field). NULL/empty falls back to the server's `IUCN_KEY` secret.
#' @return `df` with `statusMMA`, `statusSourceMMA`, `iucnCategory`,
#'   `iucnCriteria` added.
#' @noRd
resolve_threat_status <- function(df, include_iucn = TRUE, iucn_key = NULL) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
        return(df)
    }
    sci <- as.character(df$scientificName)
    mma <- mma_lookup(sci)
    df$statusMMA <- mma$statusMMA
    df$statusSourceMMA <- mma$statusSourceMMA

    df$iucnCriteria <- NA_character_
    if (!isTRUE(include_iucn)) {
        df$iucnCategory <- NA_character_
        return(df)
    }

    # `taxonID` is the *source base's* identifier, and only the GBIF provider's
    # is a GBIF usageKey. Handing a florabr/faunabr `id` to the GBIF species API
    # would look up an unrelated taxon, so pass it through only for GBIF rows —
    # everything else is matched by name (`gbif_match_usage_keys()`).
    tax <- if ("taxonID" %in% names(df)) as.character(df$taxonID) else rep(NA_character_, nrow(df))
    prov <- if ("provider" %in% names(df)) as.character(df$provider) else rep(NA_character_, nrow(df))
    tax[is.na(prov) | prov != "gbif"] <- NA_character_

    key <- iucn_key_for_run(iucn_key)
    if (iucn_key_route_enabled(key)) {
        genus <- if ("genus" %in% names(df)) df$genus else rep(NA_character_, nrow(df))
        specific <- if ("specificEpithet" %in% names(df)) df$specificEpithet else rep(NA_character_, nrow(df))
        infra <- if ("infraspecificEpithet" %in% names(df)) df$infraspecificEpithet else NA
        iu <- fetch_iucn_rredlist(genus, specific, infra, key = key)
        df$iucnCategory <- iu$iucnCategory
        df$iucnCriteria <- iu$iucnCriteria

        # The key route is keyed on genus+specificEpithet and can miss per
        # species (name absent from the Red List, API hiccup). Never let that
        # erase a category the keyless GBIF route can still supply: only
        # `criteria` is exclusive to the key route.
        gap <- !is_non_empty(df$iucnCategory)
        if (any(gap)) {
            df$iucnCategory[gap] <- iucn_category(sci[gap], tax[gap])
        }
    } else {
        df$iucnCategory <- iucn_category(sci, tax)
    }
    df
}
