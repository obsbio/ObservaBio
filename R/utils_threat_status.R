# Title: Conservation Status (MMA + IUCN)
# Two independent status sources (SPEC §5):
#   - MMA: offline lookup against the embedded sensitive_species.rds (Portarias
#     1.783/2026, 1.704/2026 and 1.667/2026), matched by normalized key; most
#     restrictive wins. A failure to load the base stops the run (A16).
#   - IUCN: global Red List category via GBIF's keyless species API (ported from
#     Saira utils_threat_status.R). Every IUCN path degrades to NA — httr2 error,
#     offline, or an unexpected payload must never break the pipeline.
# Both are memoized in the package environment — which every session in the R
# process shares, so anything key-dependent must be namespaced (ADR-005).

# ---------------------------------------------------------------------------
# MMA (Portarias 1.783/2026 + 1.704/2026 + 1.667/2026) — offline
# ---------------------------------------------------------------------------

.mma_cache <- create_rds_cache("mma_sensitive")

# Severity order, most restrictive first (Portaria categories).
.mma_severity <- c("CR (PEX)" = 5L, "CR" = 4L, "EN" = 3L, "VU" = 2L, "NT" = 1L)

#' Load and cache the embedded MMA sensitive-species base
#'
#' Stops when the file is missing, unreadable or has the wrong shape. An empty
#' answer would make every species look not listed, and the run would look
#' complete (audit A16).
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
    data <- tryCatch(readRDS(path), error = function(e) {
        stop(sprintf("MMA base at '%s' is not readable: %s", path, conditionMessage(e)))
    })
    needed <- c("scientificName", "match_key", "category", "source")
    if (!is.data.frame(data) || nrow(data) == 0L || !all(needed %in% names(data))) {
        stop(sprintf("MMA base at '%s' is empty or has no %s column.", path,
                     paste(needed, collapse = "/")))
    }
    .mma_cache$set(data)
    data
}

#' The federal portarias that the embedded MMA base replaced
#'
#' Only these references show that an input status is out of date (see
#' [dwc_stale_mma_source()]). A portaria that is not in this list can be a
#' state list or a list that the app does not know, and it stays.
#'
#' - 148/2022: Portaria 1.783/2026 revokes it (Art. 2).
#' - 443/2014: Portaria 1.783/2026 replaces its flora annex.
#' - 444/2014: Portaria 1.704/2026 replaces its fauna annexes.
#' - 445/2014: Portaria 1.667/2026 revokes it.
#'
#' If a new portaria replaces one of the embedded lists, add the old number
#' here (docs/atualizacao_bases.md).
#' @noRd
mma_replaced_sources <- function() {
    c("148/2022", "443/2014", "444/2014", "445/2014")
}

#' MMA threat status for a vector of scientific names
#'
#' Matches on the normalized key; when a name maps to several rows (different
#' portarias) the most restrictive category wins and its source is reported.
#' A name with no row of its own can match through its accepted name (audit
#' A10). The source then names the accepted name, so the status is traceable.
#' A failure to load the base stops here: see [mma_load_data()].
#'
#' @param scientific_names Character vector.
#' @param accepted_names Optional accepted name per input (same length, NA
#'   where none), from [providers_accepted_names()].
#' @return Data frame with `scientificName`, `statusMMA`, `statusSourceMMA`
#'   (NA where the species is not MMA-listed), same length/order as input.
#' @noRd
mma_lookup <- function(scientific_names, accepted_names = NULL) {
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

    data <- mma_load_data()

    keys <- normalize_for_matching(scientific_names)
    alt <- if (length(accepted_names) == n) as.character(accepted_names) else rep(NA_character_, n)
    alt_keys <- normalize_for_matching(alt)
    data_sev <- unname(.mma_severity[data$category])
    data_sev[is.na(data_sev)] <- 0L
    by_key <- split(seq_len(nrow(data)), data$match_key)

    for (i in seq_len(n)) {
        via <- NA_character_
        hits <- if (is_non_empty(keys[[i]])) by_key[[keys[[i]]]] else NULL
        if (length(hits) == 0L && is_non_empty(alt_keys[[i]]) &&
            !identical(alt_keys[[i]], keys[[i]])) {
            hits <- by_key[[alt_keys[[i]]]]
            via <- alt[[i]]
        }
        if (length(hits) == 0L) {
            next
        }
        best <- hits[[which.max(data_sev[hits])]]
        out$statusMMA[[i]] <- data$category[[best]]
        out$statusSourceMMA[[i]] <- if (is.na(via)) {
            data$source[[best]]
        } else {
            sprintf("%s (nome aceito: %s)", data$source[[best]], via)
        }
    }
    out
}

#' Accepted names that the providers know for a set of names
#'
#' Asks every provider with an `accepted_name()` accessor (the BR bases). A
#' name gets an accepted name only when the answers agree on one name. Two
#' different answers are a homonym or a split taxon, and a guess there could
#' give a species the status of another one.
#'
#' @param names Character vector of scientific names.
#' @param providers Providers to ask.
#' @return Character vector, same length as `names`, NA where no single
#'   accepted name is known.
#' @noRd
providers_accepted_names <- function(names, providers = get_providers()) {
    nm <- as.character(names)
    out <- rep(NA_character_, length(nm))
    uniq <- unique(nm[is_non_empty(nm)])
    if (length(uniq) == 0L) {
        return(out)
    }
    answers <- list()
    for (p in providers) {
        if (!is.function(p$accepted_name)) {
            next
        }
        a <- tryCatch(p$accepted_name(uniq), error = function(e) NULL)
        if (!is.data.frame(a) || nrow(a) == 0L) {
            next
        }
        a <- a[is_non_empty(a$acceptedName), , drop = FALSE]
        for (i in seq_len(nrow(a))) {
            q <- as.character(a$query_name[[i]])
            answers[[q]] <- unique(c(answers[[q]], as.character(a$acceptedName[[i]])))
        }
    }
    single <- vapply(answers, function(x) {
        if (length(unique(normalize_for_matching(x))) == 1L) x[[1L]] else NA_character_
    }, character(1))
    hit <- nm %in% names(single)
    out[hit] <- unname(single[nm[hit]])
    out
}

#' MMA status for names, with the accepted-name route of [mma_lookup()]
#'
#' The names of rows that arrive already validated come straight from the
#' sheet, often with the author ("Paubrasilia echinata (Lam.) Gagnon"). The
#' base keys have no author, so the lookup uses the cascade normalizer first.
#' Thus a validated row and a cascade row of one species get one key, as in
#' [invasive_match_key()]. The output keeps the names of the input.
#'
#' @param scientific_names Character vector.
#' @param providers Providers that know synonyms ([providers_accepted_names()]).
#' @return Data frame as in [mma_lookup()].
#' @noRd
mma_status_for_names <- function(scientific_names, providers = get_providers()) {
    query <- vapply(as.character(scientific_names), normalize_scientific_name,
                    FUN.VALUE = character(1), USE.NAMES = FALSE)
    out <- mma_lookup(query, accepted_names = providers_accepted_names(query, providers))
    out$scientificName <- as.character(scientific_names)
    out
}

# ---------------------------------------------------------------------------
# IUCN Red List category — keyless GBIF species API (ported from Saira)
# ---------------------------------------------------------------------------

.gbif_iucn_cache <- create_rds_cache("gbif_iucn")
.gbif_match_cache <- create_rds_cache("gbif_match")

GBIF_API_BASE <- "https://api.gbif.org/v1"
GBIF_API_TIMEOUT_S <- 10

# GBIF requests in flight at once, so their network waits overlap (ADR-031).
# The rate limits below, not this number, set the pace.
GBIF_MAX_ACTIVE <- 4L

# Requests per second for each kind of GBIF call, measured on 2026-09-18. The
# species lookups took 20 per second with no 429. The occurrence search has no
# fixed limit: GBIF lowers it when its servers are busy. At 2 per second, GBIF
# answered 429 to 36 of 40 searches. Thus the geo step sends few searches
# (ADR-031), one or two at a time.
GBIF_SPECIES_RATE <- 20
GBIF_OCC_RATE <- 2
GBIF_OCC_MAX_ACTIVE <- 2L

# Tries per request. After a 429, GBIF asks for a pause (Retry-After), and
# httr2 stops all the requests of the batch for that time.
GBIF_MAX_TRIES <- 5L

# The feature is opt-in through httr2: absent httr2 disables every call.
has_httr2 <- function() requireNamespace("httr2", quietly = TRUE)

#' One GET request to the GBIF API
#'
#' The throttle is per `realm`, so the light species lookups and the heavy
#' occurrence searches each keep their own rate. httr2 tries a 429 or a 5xx
#' again, up to `GBIF_MAX_TRIES` answers. A network failure (timeout, no
#' answer) is final: the caller does not cache it, so the next run asks again.
#'
#' @param segments Path segments after the API base.
#' @param query Named list of query parameters, or NULL.
#' @param rate Maximum requests per second in this realm.
#' @param realm Throttle pool name.
#' @param timeout Seconds before the request fails.
#' @return An httr2 request.
#' @noRd
gbif_request <- function(segments, query = NULL, rate = GBIF_SPECIES_RATE,
                         realm = "gbif-species", timeout = GBIF_API_TIMEOUT_S) {
    req <- httr2::request(GBIF_API_BASE)
    req <- do.call(httr2::req_url_path_append, c(list(req), as.list(segments)))
    if (length(query) > 0L) {
        req <- do.call(httr2::req_url_query, c(list(req), query))
    }
    req <- httr2::req_timeout(req, timeout)
    req <- httr2::req_user_agent(req, "ObservaBio R package")
    req <- httr2::req_throttle(req, capacity = rate, fill_time_s = 1, realm = realm)
    # httr2 tries again only a request that failed. Thus a 429 or a 5xx must
    # stay an HTTP error here, and gbif_perform_parallel() reads its status.
    # In parallel, httr2 1.2.3 ignores max_tries and tries again with no end,
    # so this counter stops the retries (LESSONS L-035).
    answers <- 0L
    httr2::req_retry(req, max_tries = GBIF_MAX_TRIES, is_transient = function(resp) {
        answers <<- answers + 1L
        status <- httr2::resp_status(resp)
        (status == 429L || status >= 500L) && answers < GBIF_MAX_TRIES
    })
}

#' Whether a GBIF answer is worth one more try
#'
#' A rate limit (429), a server error (5xx) or a network failure (timeout, no
#' answer) can pass. Any other status is the final answer.
#' @noRd
gbif_is_transient <- function(res) {
    if (!inherits(res, "httr2_response")) {
        return(TRUE)
    }
    status <- httr2::resp_status(res)
    status == 429L || status >= 500L
}

#' Perform GBIF requests in parallel
#'
#' Up to `max_active` requests are in flight at once. httr2 tries a transient
#' failure again ([gbif_request()]). After a 429, httr2 stops all the requests
#' of the batch for the pause that GBIF asks for. Retry rounds after the batch
#' did not stop the other requests, and GBIF refused 36 of 40 of them.
#' A request that still fails stays failed, and the caller reports it as
#' "could not check", never as "absent" (L-023).
#'
#' @param reqs List of requests from [gbif_request()].
#' @param max_active Requests in flight at once.
#' @return List the length of `reqs`: an `httr2_response`, or an error
#'   condition when the request never got an answer.
#' @noRd
gbif_perform_parallel <- function(reqs, max_active = GBIF_MAX_ACTIVE) {
    # Without this, each pause for a 429 writes a progress bar to the server log.
    old <- options(httr2_progress = FALSE)
    on.exit(options(old), add = TRUE)
    res <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                       progress = FALSE, max_active = max_active)
    # An HTTP error carries its response. The callers read the status from it.
    lapply(res, function(r) if (inherits(r, "httr2_http")) r$resp else r)
}

#' The parsed JSON body of a GBIF answer, or NULL when it is not a 200
#' @noRd
gbif_response_body <- function(res) {
    if (!inherits(res, "httr2_response") || httr2::resp_status(res) != 200L) {
        return(NULL)
    }
    tryCatch(httr2::resp_body_json(res), error = function(e) NULL)
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
#' (e.g. "NT", "VU", "EN"; "NE" for a taxon the IUCN never assessed). Missing
#' keys, absent httr2, offline, or any API error yield NA. The requests go out in
#' parallel ([gbif_perform_parallel()]). Memoized per session, except a failed
#' request, so the next run asks again.
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
    if (length(misses) > 0L) {
        reqs <- lapply(misses, function(k) gbif_request(c("species", k, "iucnRedListCategory")))
        res <- tryCatch(gbif_perform_parallel(reqs),
                        error = function(e) vector("list", length(misses)))
        for (i in seq_along(misses)) {
            if (gbif_is_transient(res[[i]])) {
                next
            }
            memo[[misses[[i]]]] <- gbif_body_field(gbif_response_body(res[[i]]), "code")
        }
        .gbif_iucn_cache$set(memo)
    }
    out[valid] <- unname(memo[keys[valid]])
    out
}

#' Resolve scientific names to GBIF keys
#'
#' Calls the keyless `GET /species/match?name=`, in parallel. Memoized per
#' session, except a failed request, so the next run asks again.
#'
#' Two keys per name. `usageKey` is the name itself, for the IUCN route.
#' `occurrenceKey` is the taxon that holds the GBIF records of the name: GBIF
#' files the records of a synonym under its accepted taxon, so a search on the
#' synonym key finds none (ADR-031).
#'
#' @param names Character vector of scientific names (NA allowed).
#' @return Data frame, one row per name: `usageKey` and `occurrenceKey` (NA
#'   where GBIF has no match), and `known` (FALSE when the lookup failed).
#' @noRd
gbif_match_keys <- function(names) {
    nm <- as.character(names)
    out <- data.frame(usageKey = rep(NA_character_, length(nm)),
                      occurrenceKey = rep(NA_character_, length(nm)),
                      known = rep(FALSE, length(nm)), stringsAsFactors = FALSE)
    valid <- !is.na(nm) & nzchar(trimws(nm))
    if (!any(valid) || !has_httr2()) {
        return(out)
    }
    memo <- .gbif_match_cache$get()
    if (is.null(memo)) {
        memo <- list(usage = character(0), occurrence = character(0))
    }
    misses <- setdiff(unique(nm[valid]), names(memo$usage))
    if (length(misses) > 0L) {
        reqs <- lapply(misses, function(q) {
            gbif_request(c("species", "match"), query = list(name = q))
        })
        res <- tryCatch(gbif_perform_parallel(reqs),
                        error = function(e) vector("list", length(misses)))
        for (i in seq_along(misses)) {
            if (gbif_is_transient(res[[i]])) {
                next
            }
            body <- gbif_response_body(res[[i]])
            # A HIGHERRANK answer is a parent taxon, not the name (LESSONS L-031).
            # Its IUCN category and its records are not those of the name.
            higher <- identical(gbif_body_field(body, "matchType"), "HIGHERRANK")
            usage <- if (higher) NA_character_ else gbif_body_field(body, "usageKey")
            accepted <- gbif_body_field(body, "acceptedUsageKey")
            memo$usage[[misses[[i]]]] <- usage
            memo$occurrence[[misses[[i]]]] <- if (is.na(usage) || is.na(accepted)) usage else accepted
        }
        .gbif_match_cache$set(memo)
    }
    hit <- valid & nm %in% names(memo$usage)
    out$usageKey[hit] <- unname(memo$usage[nm[hit]])
    out$occurrenceKey[hit] <- unname(memo$occurrence[nm[hit]])
    out$known[hit] <- TRUE
    out
}

#' GBIF usage keys for names (the IUCN route of [gbif_match_keys()])
#'
#' For names lacking a GBIF-resolved `taxonID` (e.g. rows a Brazilian provider
#' matched).
#'
#' @param names Character vector of scientific names (NA allowed).
#' @return Character vector of GBIF usage keys (NA where unmatched), same length.
#' @noRd
gbif_match_usage_keys <- function(names) {
    gbif_match_keys(names)$usageKey
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
# ObservaBio output model has a `criteria` column, so when a key is available and
# rredlist is installed, we query `rredlist::rl_species_latest()` to fill both.
# Two key sources (ADR-005): the user's own key, pasted in the optional Step 1
# field and living only in that Shiny session, or the server's `IUCN_KEY` secret
# as fallback. Absent key/package/network, this degrades to NA and the caller
# falls back to the keyless category.

.iucn_rredlist_cache <- create_rds_cache("iucn_rredlist")

# An answer stays in the shared cache for one day, so a new IUCN assessment
# reaches a process that runs for days. The entry cap keeps the cache from
# growing without limit in that process.
IUCN_CACHE_TTL_S <- 24 * 3600
IUCN_CACHE_MAX <- 5000L

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
#' disabled, get NA. The calls go one at a time with a pause between them: the
#' IUCN asks for at least 0.5 s, and it can block a key that calls faster.
#'
#' Only a real answer goes into the cache: an assessment, or an HTTP 404 (the
#' IUCN has no such name). A timeout, a 5xx or any other error stays out of the
#' cache, so the next run asks again (audit A15). Entries expire after
#' `IUCN_CACHE_TTL_S`.
#'
#' @param genus,specific,infra Character vectors, aligned and equal length.
#' @param key IUCN Red List API key. Defaults to the server secret.
#' @param pause Seconds between two live calls.
#' @param sleep Sleep seam for the pause.
#' @param fetch Seam for `rredlist::rl_species_latest()` (tests).
#' @param now Clock seam for the cache expiry (tests).
#' @return Data frame with `iucnCategory`, `iucnCriteria`, same length/order.
#' @noRd
fetch_iucn_rredlist <- function(genus, specific, infra = NA, key = iucn_key(),
                                pause = 0.5, sleep = Sys.sleep,
                                fetch = rredlist::rl_species_latest,
                                now = Sys.time) {
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
    t_now <- as.numeric(now())
    fresh <- function(entry) {
        !is.null(entry) && is.numeric(entry$at) && t_now - entry$at < IUCN_CACHE_TTL_S
    }
    live <- 0L
    for (idx in which(valid)) {
        tkey <- triples[[idx]]
        if (!fresh(memo[[tkey]])) {
            if (live > 0L) {
                sleep(pause)
            }
            live <- live + 1L
            res <- tryCatch(
                fetch(
                    genus = g[[idx]], species = s[[idx]],
                    infra = if (is_non_empty(i[[idx]])) i[[idx]] else NULL,
                    key = key, parse = TRUE
                ),
                error = function(e) e
            )
            if (inherits(res, "error")) {
                # rredlist reports "no such name" as an error with HTTP 404.
                # Every other error can pass, so it must not look like an answer.
                if (!grepl("HTTP 404", conditionMessage(res), fixed = TRUE)) {
                    next
                }
                res <- NULL
            }
            memo[[tkey]] <- c(extract_rredlist_assessment(res), list(at = t_now))
        }
        out$iucnCategory[[idx]] <- memo[[tkey]]$category
        out$iucnCriteria[[idx]] <- memo[[tkey]]$criteria
    }
    if (length(memo) > IUCN_CACHE_MAX) {
        at <- vapply(memo, function(e) as.numeric(e$at %||% 0), numeric(1))
        memo <- memo[order(at, decreasing = TRUE)[seq_len(IUCN_CACHE_MAX)]]
    }
    .iucn_rredlist_cache$set(memo)
    out
}

# ---------------------------------------------------------------------------
# Combined
# ---------------------------------------------------------------------------

#' Attach MMA + IUCN status columns to a cascade result
#'
#' MMA is always resolved offline. For IUCN, the keyless GBIF route gives the
#' category of every species. When a key is available and rredlist is
#' installed, the rredlist **key route** then fills `iucnCriteria` (and the
#' IUCN's own category) for the species that can have criteria, see
#' [iucn_criteria_categories()], plus the MMA-listed ones and the ones GBIF gave
#' no answer for (ADR-031). All IUCN paths degrade to NA.
#'
#' @param df Data frame with at least `scientificName`; `taxonID`,
#'   `genus`/`specificEpithet`/`infraspecificEpithet` used when present.
#' @param include_iucn Whether to resolve IUCN status. Default TRUE.
#' @param iucn_key The user's IUCN Red List API key for this run (the optional
#'   Step 1 field). NULL/empty falls back to the server's `IUCN_KEY` secret.
#' @param providers Providers that know synonyms, for the MMA accepted-name
#'   route ([mma_status_for_names()]).
#' @return `df` with `statusMMA`, `statusSourceMMA`, `iucnCategory`,
#'   `iucnCriteria` added.
#' @noRd
resolve_threat_status <- function(df, include_iucn = TRUE, iucn_key = NULL,
                                  providers = get_providers()) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
        return(df)
    }
    sci <- as.character(df$scientificName)
    mma <- mma_status_for_names(sci, providers)
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

    # The keyless GBIF route answers every species, several at a time.
    df$iucnCategory <- iucn_category(sci, tax)

    # The key route is slow: two calls per species, 0.5 s apart. It only adds
    # `criteria`, which only the categories in iucn_criteria_categories() have.
    # So ask it for those, for every MMA-listed species (a name GBIF cannot place
    # or keeps outdated), and for a species GBIF gave no answer for (ADR-031).
    key <- iucn_key_for_run(iucn_key)
    if (iucn_key_route_enabled(key)) {
        ask <- which(df$iucnCategory %in% iucn_criteria_categories() |
                         is_non_empty(df$statusMMA) | !is_non_empty(df$iucnCategory))
        if (length(ask) > 0L) {
            col_or_na <- function(name) {
                if (!name %in% names(df)) {
                    return(rep(NA_character_, nrow(df)))
                }
                as.character(df[[name]])
            }
            iu <- fetch_iucn_rredlist(col_or_na("genus")[ask],
                                      col_or_na("specificEpithet")[ask],
                                      col_or_na("infraspecificEpithet")[ask], key = key)
            # The IUCN itself wins over the copy that GBIF keeps. A key-route miss
            # (name absent from the Red List, API failure) keeps the GBIF category.
            got <- is_non_empty(iu$iucnCategory)
            df$iucnCategory[ask[got]] <- iu$iucnCategory[got]
            df$iucnCriteria[ask] <- iu$iucnCriteria
        }
    }
    df
}

#' IUCN categories whose assessment carries criteria
#'
#' LC, DD and NE have no criteria, so the key route has nothing to add for them.
#' @noRd
iucn_criteria_categories <- function() {
    c("CR", "EN", "VU", "NT", "LR/nt", "LR/cd")
}
