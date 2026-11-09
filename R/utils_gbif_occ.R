# Title: GBIF Occurrences Inside the Buffer (part 2 of the geo verification)
# Pure helpers (no Shiny) for SPEC §8 step 3: for each species, ask GBIF for
# georeferenced occurrences that fall inside the 10 km buffer built in
# utils_geo.R. GBIF limits the occurrence search more when its servers are
# busy, so this step sends few searches (ADR-031). Per query polygon, one facet
# search lists the taxa with records, and grouped searches then fetch the
# points of those taxa only. A lookup that still fails is tagged (gbif_error /
# the "failed" attribute) so callers can tell "could not check" from
# "genuinely absent" instead of collapsing both to zero points. The final
# distributionFlag classifier is classify_distribution_flag() in utils_geo.R.
#
# GBIF geometry limit: the WKT travels in the query string, and GBIF cuts the
# request line at 4 KB. Past that the API answers 400 and rgbif reports it as
# "500 - Server error" (LESSONS L-022). So we do NOT send the raw buffer ring.
# gbif_occ_wkt() coarsens it to a query polygon that is guaranteed to fit and to
# *contain* the buffer (buffer -> metric simplify -> bbox), then
# gbif_occ_in_buffer() refines the returned points against the exact buffer with
# sf::st_intersects. So the query polygon only trims transfer; the "within 10 km"
# decision is always the exact sf test (ADR-009).

.gbif_occ_cache <- new.env(parent = emptyenv())

# Areas whose occurrences stay cached at once (see gbif_occ_store()).
.GBIF_OCC_CACHE_MAX_AREAS <- 10L

# Character ceiling for the query polygon's WKT. GBIF cuts the request line at
# 4 KB, and percent-encoding grows the WKT by about a third on the way into the
# URL, so 4 KB of request line is roughly 2.7 KB of WKT. 2000 leaves room for
# the other query parameters, and for the taxon keys of a grouped search
# (.GBIF_URL_MAX_CHARS).
#
# Measure characters, not vertices: the two do not convert. The old 300-vertex
# cap passed a 5224-character WKT (a 6864-character URL) straight into the 400.
.GBIF_WKT_MAX_CHARS <- 2000L

# Simplify tolerances for the query polygon, in metres, tightest first. A 10 km
# buffer at 500 m keeps the shape within about 8 percent of the exact area, so
# the ladder gives up fidelity slowly.
.GBIF_WKT_SIMPLIFY_M <- c(200, 500, 1000, 2000, 5000)

#' Force a (multi)polygon's rings to the GBIF winding
#'
#' GBIF reads a clockwise exterior ring as *everything outside it*, so a
#' wrongly wound polygon returns the complement and the exact-buffer refine then
#' drops every real point — silent zero results. It also rejects a polygon whose
#' **interior** ring runs anticlockwise ("Polygon with anticlockwise interior
#' ring"), which fails the lookup outright. sf guarantees neither winding, so
#' both are set here: exterior anticlockwise, holes clockwise.
#'
#' @param geom An `sfc` with one POLYGON or MULTIPOLYGON.
#' @return The same geometry, exterior ring(s) CCW and interior ring(s) CW.
#' @noRd
.geo_force_ccw <- function(geom) {
    orient <- function(ring, ccw) {
        # Shoelace: negative signed area == clockwise in x/y (lon/lat) space.
        x <- ring[, 1L]
        y <- ring[, 2L]
        i <- seq_len(nrow(ring) - 1L)
        signed2 <- sum(x[i] * y[i + 1L] - x[i + 1L] * y[i])
        wrong <- if (ccw) signed2 < 0 else signed2 > 0
        if (wrong) ring[rev(seq_len(nrow(ring))), , drop = FALSE] else ring
    }
    reorient <- function(poly) {
        rings <- unclass(poly)
        for (i in seq_along(rings)) {
            rings[[i]] <- orient(rings[[i]], ccw = i == 1L)
        }
        rings
    }
    g <- sf::st_geometry(geom)[[1L]]
    if (inherits(g, "POLYGON")) {
        out <- sf::st_polygon(reorient(unclass(g)))
    } else if (inherits(g, "MULTIPOLYGON")) {
        out <- sf::st_multipolygon(lapply(unclass(g), reorient))
    } else {
        return(geom)
    }
    sf::st_sfc(out, crs = sf::st_crs(geom))
}

#' Build a GBIF-safe query polygon (WKT) from the 10 km buffer
#'
#' Coarsens the buffer until its WKT fits `max_chars`, preferring shape fidelity:
#' the buffer itself if it already fits, then growing simplify tolerances, then
#' the bounding box. Every candidate *contains* the buffer, so the exact-buffer
#' refine downstream never loses a real occurrence — a coarser candidate only
#' over-fetches (ADR-009).
#'
#' Two traps this navigates. `sf::st_simplify` does nothing useful on lon/lat, so
#' the simplification runs in a metric CRS. And Douglas-Peucker cuts corners
#' *inward*, which would make the candidate a subset: the result is widened again
#' by the same tolerance, whose value bounds the error Douglas-Peucker can
#' introduce. `sf::st_covers` confirms containment per candidate, because a
#' tolerance large enough to swallow a small parcel would otherwise cut the
#' buffer in silence.
#'
#' The bounding box closes the ladder: it always contains the buffer and its WKT
#' is around 120 characters, so some candidate always fits. It is last because it
#' over-fetches badly for an area with detached parcels — the box spans the gap
#' between them, and a species with more than `max_records` hits inside the box
#' could crowd out the ones actually near the area.
#'
#' @param buffer An `sfc`/`sf` polygon (the `buffer` slot from `geo_buffer()`).
#' @param max_chars Character ceiling for the emitted WKT.
#' @return A WKT string (EPSG:4326, CCW), or `NA_character_` if `buffer` is empty.
#' @noRd
gbif_occ_wkt <- function(buffer, max_chars = .GBIF_WKT_MAX_CHARS) {
    geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(buffer)))
    if (length(geom) == 0L || all(sf::st_is_empty(geom))) {
        return(NA_character_)
    }
    geom <- sf::st_transform(geom, 4326)

    as_wkt <- function(g) sf::st_as_text(.geo_force_ccw(g)[[1L]])
    wkt <- as_wkt(geom)
    if (nchar(wkt) <= max_chars) {
        return(wkt)
    }

    metric <- metric_crs_for(geom)
    exact <- sf::st_transform(geom, metric)
    for (tol in .GBIF_WKT_SIMPLIFY_M) {
        candidate <- sf::st_make_valid(sf::st_buffer(
            sf::st_simplify(exact, dTolerance = tol), dist = tol, nQuadSegs = 2
        ))
        if (all(sf::st_is_empty(candidate)) ||
            !sf::st_covers(candidate, exact, sparse = FALSE)[1L, 1L]) {
            next
        }
        wkt <- as_wkt(sf::st_transform(candidate, 4326))
        if (nchar(wkt) <= max_chars) {
            return(wkt)
        }
    }
    as_wkt(sf::st_as_sfc(sf::st_bbox(geom)))
}

#' Ceiling on how much bigger the query polygon may be than what it covers
#'
#' The query polygon is a coarse superset, and the exact refine drops whatever
#' falls outside the buffer. That is only safe while the polygon stays close to
#' the buffer: GBIF returns at most `max_records` per species, in no particular
#' order, so a polygon many times too big can fill the page with distant points
#' and hide the ones actually near the area. The result is a silent "sem registro
#' próximo" for a species recorded 2 km away (LESSONS L-023).
.GBIF_QUERY_MAX_OVERFETCH <- 1.5

#' How much bigger a query polygon is than the geometry it must cover
#' @noRd
.gbif_occ_overfetch <- function(geom) {
    wkt <- gbif_occ_wkt(geom)
    if (is.na(wkt)) {
        return(Inf)
    }
    covered <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(geom))))
    if (!is.finite(covered) || covered <= 0) {
        return(Inf)
    }
    as.numeric(sf::st_area(sf::st_as_sfc(wkt, crs = 4326))) / covered
}

#' Split a buffer into query blocks, each tight enough to query on its own
#'
#' A buffer of one compact area is one block, which is the whole upload for most
#' users — they pay nothing for this. A buffer of detached parcels (a farm plus
#' its reserve, a dozen fragments) cannot be covered by one tight polygon: the
#' single polygon spans the gaps and over-fetches, measured at 11x for twelve
#' parcels. Grouping neighbouring parcels holds every block near
#' `.GBIF_QUERY_MAX_OVERFETCH` and costs about one request per five parcels
#' instead of one per parcel.
#'
#' Parts are visited west to east so a block collects neighbours, and a block
#' closes as soon as adding the next part would push it past the ceiling.
#'
#' @param buffer The `buffer` slot from `geo_buffer()`.
#' @param max_overfetch Area ratio a block may not exceed.
#' @return List of `sfc` blocks whose union is the buffer.
#' @noRd
gbif_occ_query_blocks <- function(buffer, max_overfetch = .GBIF_QUERY_MAX_OVERFETCH) {
    geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(buffer)))
    if (length(geom) == 0L || all(sf::st_is_empty(geom))) {
        return(list())
    }
    parts <- suppressWarnings(sf::st_cast(geom, "POLYGON"))
    if (length(parts) <= 1L) {
        return(list(geom))
    }

    centroids <- suppressWarnings(sf::st_coordinates(sf::st_centroid(parts)))
    order_ew <- order(centroids[, "X"], centroids[, "Y"])

    blocks <- list()
    current <- integer(0)
    for (i in order_ew) {
        trial <- c(current, i)
        if (length(current) > 0L &&
            .gbif_occ_overfetch(sf::st_union(parts[trial])) > max_overfetch) {
            blocks[[length(blocks) + 1L]] <- sf::st_union(parts[current])
            current <- i
        } else {
            current <- trial
        }
    }
    blocks[[length(blocks) + 1L]] <- sf::st_union(parts[current])
    blocks
}

#' Tag a point table with whether its GBIF lookup failed (vs. genuinely empty)
#' @noRd
.gbif_occ_flag_error <- function(points, failed) {
    attr(points, "gbif_error") <- isTRUE(failed)
    points
}

#' Parse a GBIF occurrence result into a tidy point table
#'
#' Keeps only rows carrying both coordinates, plus the dataset source. Takes the
#' table from [gbif_occ_results()], a bare data frame from a test seam, or a
#' list with a `$data` slot (the old `rgbif::occ_data` shape).
#'
#' `key` is GBIF's own occurrence identifier. It is carried so overlapping query
#' polygons can be deduplicated by record identity: two distinct observations
#' often share a rounded coordinate and a dataset, so coordinates cannot tell a
#' repeat apart from a neighbour (LESSONS L-024).
#'
#' @param res A data frame of occurrence records, a list with `$data`, or NULL.
#' @return Data frame `species`/`decimalLongitude`/`decimalLatitude`/
#'   `datasetKey`/`key` (zero rows when nothing usable is present).
#' @noRd
gbif_occ_parse <- function(res) {
    empty <- data.frame(
        species = character(0), decimalLongitude = numeric(0),
        decimalLatitude = numeric(0), datasetKey = character(0),
        key = character(0), stringsAsFactors = FALSE
    )
    dat <- if (is.list(res) && !is.data.frame(res) && "data" %in% names(res)) {
        res$data
    } else {
        res
    }
    if (!is.data.frame(dat) || nrow(dat) == 0L) {
        return(empty)
    }
    col <- function(name) {
        if (name %in% names(dat)) dat[[name]] else rep(NA, nrow(dat))
    }
    lon <- suppressWarnings(as.numeric(col("decimalLongitude")))
    lat <- suppressWarnings(as.numeric(col("decimalLatitude")))
    sci <- as.character(col("scientificName"))
    keep <- !is.na(lon) & !is.na(lat)
    if (!any(keep)) {
        return(empty)
    }
    data.frame(
        species = sci[keep],
        decimalLongitude = lon[keep],
        decimalLatitude = lat[keep],
        datasetKey = as.character(col("datasetKey"))[keep],
        key = as.character(col("key"))[keep],
        stringsAsFactors = FALSE
    )
}

# The taxon keys of an occurrence record. A search on a taxon key also finds
# the records of the children of that taxon, so one of these fields holds the key.
.GBIF_OCC_TAXON_FIELDS <- c("taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey",
                            "classKey", "orderKey", "familyKey", "genusKey", "speciesKey")

# Character ceiling for a whole search URL, because GBIF cuts the request line
# at 4 KB (LESSONS L-022). It sets how many taxon keys one grouped search holds.
.GBIF_URL_MAX_CHARS <- 3800L

# Taxa per page of the facet search.
.GBIF_FACET_PAGE <- 10000L

#' The occurrence records of one GBIF search answer, as a data frame
#'
#' Reads only the fields that [gbif_occ_parse()] and the split by taxon use.
#'
#' @param body The parsed JSON body of an occurrence search.
#' @return Data frame, or NULL when the answer has no records.
#' @noRd
gbif_occ_results <- function(body) {
    recs <- body$results
    if (length(recs) == 0L) {
        return(NULL)
    }
    fields <- c("scientificName", "decimalLongitude", "decimalLatitude", "datasetKey",
                "key", .GBIF_OCC_TAXON_FIELDS)
    cols <- lapply(fields, function(field) {
        vapply(recs, function(r) {
            v <- r[[field]]
            if (length(v) != 1L) NA_character_ else as.character(v)
        }, FUN.VALUE = character(1))
    })
    as.data.frame(stats::setNames(cols, fields), stringsAsFactors = FALSE)
}

#' One GBIF occurrence search inside a query polygon
#' @noRd
gbif_occ_request <- function(wkt, query) {
    gbif_request(c("occurrence", "search"),
                 query = c(list(geometry = wkt, country = "BR", hasCoordinate = "true"), query),
                 rate = GBIF_OCC_RATE, realm = "gbif-occurrence", timeout = 60)
}

#' Records per taxon inside a query polygon, from one facet search
#'
#' The `taxonKey` facet counts, for each taxon, the records that a search on
#' its key finds: the records of the taxon, of its synonyms and of its children.
#' Thus one search tells which taxa have records near the area, and how many.
#'
#' @param wkt Query polygon.
#' @return Named integer vector (taxon key to records), or NULL when the
#'   search failed.
#' @noRd
gbif_occ_taxon_counts <- function(wkt) {
    counts <- integer(0)
    offset <- 0L
    repeat {
        req <- gbif_occ_request(wkt, list(limit = 0L, facet = "taxonKey",
                                          facetLimit = .GBIF_FACET_PAGE,
                                          facetOffset = offset))
        res <- tryCatch(gbif_perform_parallel(list(req), max_active = 1L)[[1L]],
                        error = function(e) NULL)
        body <- gbif_response_body(res)
        if (is.null(body)) {
            return(NULL)
        }
        page <- if (length(body$facets) > 0L) body$facets[[1L]]$counts else list()
        counts <- c(counts, stats::setNames(
            vapply(page, function(x) as.integer(x$count), integer(1)),
            vapply(page, function(x) as.character(x$name), character(1))
        ))
        if (length(page) < .GBIF_FACET_PAGE) {
            return(counts)
        }
        offset <- offset + .GBIF_FACET_PAGE
    }
}

#' Group taxa into occurrence searches of one page each
#'
#' A taxon with more records than one page gets a search of its own, and keeps
#' the first page: the same cap as one search per species. The other taxa
#' share searches (first-fit decreasing), and each search holds at most
#' `limit` records and `max_keys` keys.
#'
#' @param counts Named integer vector: records per taxon key, each above 0.
#' @param limit Records per page.
#' @param max_keys Keys that fit in one search URL.
#' @return List of character vectors of taxon keys, one per search.
#' @noRd
gbif_occ_plan <- function(counts, limit, max_keys) {
    big <- names(counts)[counts > limit]
    small <- sort(counts[counts <= limit], decreasing = TRUE)
    groups <- list()
    room <- integer(0)
    for (k in names(small)) {
        fit <- which(room >= small[[k]] & lengths(groups) < max_keys)[1L]
        if (is.na(fit)) {
            groups[[length(groups) + 1L]] <- k
            room <- c(room, limit - small[[k]])
        } else {
            groups[[fit]] <- c(groups[[fit]], k)
            room[[fit]] <- room[[fit]] - small[[k]]
        }
    }
    c(as.list(big), groups)
}

#' Fetch GBIF occurrences for many (species, block) pairs with few searches
#'
#' The live path of [gbif_occ_in_buffer()]. One search per species hit the GBIF
#' limit: 1924 searches for three areas ran for more than 40 minutes (ADR-031).
#' Per query polygon, this path sends one facet search
#' ([gbif_occ_taxon_counts()]), and then grouped searches for the taxa with
#' records only ([gbif_occ_plan()]). A name goes through its GBIF key
#' ([gbif_match_keys()]), and the records of a grouped search are split by
#' their taxon keys.
#'
#' Degrades gracefully: a pair whose lookup fails (offline, still rate-limited
#' after the retries, httr2 missing) gives an empty table flagged as failed,
#' never an abort (LESSONS L-003). A name that GBIF does not know has no
#' records: an empty table, not a failure.
#'
#' @param names,wkts Character vectors of the same length, one pair each.
#' @param max_records Records per taxon, capped at 300 (one GBIF page).
#' @return List of tables in the [gbif_occ_parse()] schema, one per pair, each
#'   tagged with a `"gbif_error"` attribute: `TRUE` when the lookup failed,
#'   `FALSE` when it succeeded (possibly with 0 rows). This lets callers tell
#'   "we could not check" from "genuinely absent".
#' @noRd
gbif_occ_fetch_grouped <- function(names, wkts, max_records = 300L) {
    empty <- gbif_occ_parse(NULL)
    failed <- .gbif_occ_flag_error(empty, TRUE)
    if (length(names) == 0L) {
        return(list())
    }
    if (!has_httr2()) {
        warning("Package 'httr2' is not installed; skipping GBIF occurrences.")
        return(rep(list(failed), length(names)))
    }
    limit <- min(as.integer(max_records), 300L)
    uniq <- unique(names)
    keys <- gbif_match_keys(uniq)
    key_of <- stats::setNames(keys$occurrenceKey, uniq)
    known <- stats::setNames(keys$known, uniq)
    blocks <- unique(wkts)
    counts <- lapply(blocks, gbif_occ_taxon_counts)

    # One entry per search: its block, its keys, and whether the page holds
    # all the records of those keys.
    plan <- list()
    for (b in seq_along(blocks)) {
        mine <- unique(key_of[names[wkts == blocks[[b]]]])
        cnt <- counts[[b]][intersect(mine[!is.na(mine)], names(counts[[b]]))]
        if (length(cnt) == 0L) {
            next
        }
        base <- nchar(gbif_occ_request(blocks[[b]], list(limit = limit, offset = 0L))$url)
        per_key <- nchar("&taxonKey=") + max(nchar(names(cnt)))
        max_keys <- max(1L, (.GBIF_URL_MAX_CHARS - base) %/% per_key)
        for (grp in gbif_occ_plan(cnt, limit, max_keys)) {
            plan[[length(plan) + 1L]] <- list(block = b, keys = grp,
                                              whole = sum(cnt[grp]) <= limit)
        }
    }
    reqs <- lapply(plan, function(p) {
        gbif_occ_request(blocks[[p$block]], list(taxonKey = p$keys, limit = limit,
                                                 offset = 0L, .multi = "explode"))
    })
    res <- tryCatch(gbif_perform_parallel(reqs, max_active = GBIF_OCC_MAX_ACTIVE),
                    error = function(e) vector("list", length(reqs)))

    # Points per (block, key). If a search that holds all its records by the
    # facet counts has more records, some of them are lost: its taxa fail.
    got <- list()
    lost <- character(0)
    for (i in seq_along(plan)) {
        ids <- paste(plan[[i]]$block, plan[[i]]$keys)
        body <- gbif_response_body(res[[i]])
        if (is.null(body) || (plan[[i]]$whole && !isTRUE(body$endOfRecords))) {
            lost <- c(lost, ids)
            next
        }
        rec <- gbif_occ_results(body)
        keymat <- if (is.null(rec)) NULL else as.matrix(rec[.GBIF_OCC_TAXON_FIELDS])
        for (j in seq_along(ids)) {
            mine <- rec
            # A search with one key needs no split.
            if (!is.null(rec) && length(ids) > 1L) {
                hit <- rowSums(keymat == plan[[i]]$keys[[j]], na.rm = TRUE) > 0L
                mine <- rec[hit, , drop = FALSE]
            }
            got[[ids[[j]]]] <- gbif_occ_parse(mine)
        }
    }

    lapply(seq_along(names), function(i) {
        b <- match(wkts[[i]], blocks)
        key <- key_of[[names[[i]]]]
        if (!known[[names[[i]]]]) {
            return(failed)
        }
        if (is.na(key)) {
            return(.gbif_occ_flag_error(empty, FALSE))
        }
        id <- paste(b, key)
        if (is.null(counts[[b]]) || id %in% lost) {
            return(failed)
        }
        .gbif_occ_flag_error(got[[id]] %||% empty, FALSE)
    })
}

#' Drop a record two overlapping query polygons both returned
#'
#' The blocks are disjoint, but their coarse query polygons are not, so one
#' record can come back from two blocks and double a map marker. Identity is
#' GBIF's `key`: deduplicating on coordinates would delete distinct observations
#' that share a rounded coordinate and a dataset (LESSONS L-024). Without a
#' usable `key` — a test stub, an older cached table — the whole row is the only
#' identity available.
#' @noRd
.gbif_occ_dedupe <- function(points) {
    if (nrow(points) == 0L) {
        return(points)
    }
    id <- if ("key" %in% names(points) && !anyNA(points$key) && all(nzchar(points$key))) {
        points$key
    } else {
        do.call(paste, c(points, sep = "\r"))
    }
    out <- points[!duplicated(id), , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Keep only points that fall inside the exact 10 km buffer
#' @noRd
.gbif_occ_refine <- function(points, buffer) {
    if (nrow(points) == 0L) {
        return(points)
    }
    pts_sf <- sf::st_as_sf(
        points, coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326, remove = FALSE
    )
    buf <- sf::st_transform(sf::st_union(sf::st_geometry(buffer)), 4326)
    inside <- lengths(sf::st_intersects(pts_sf, buf)) > 0L
    out <- points[inside, , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' GBIF occurrences inside the buffer, for a set of species
#'
#' Dedupes names (LESSONS L-001), queries only what is not already memoized for
#' this buffer, and refines every returned point against the exact buffer. A
#' buffer of detached parcels is split into query blocks first
#' ([gbif_occ_query_blocks()]), so each request carries a tight polygon and the
#' `max_records` page cannot fill with distant points (LESSONS L-023). The
#' caller passes only the species that still need checking (e.g.
#' `dwc_unvalidated_names()`), keeping the geo step aligned with the cascade's
#' skip-already-validated optimization. An empty `names` never touches the
#' network. Session-memoized per area ([gbif_occ_store()]), so a second run over
#' the same areas asks GBIF nothing again.
#'
#' @param names Character vector of scientific names.
#' @param buffer The `buffer` slot from `geo_buffer()` (`sfc`, EPSG:4326).
#' @param max_records Per-species record cap.
#' @param page_size Records per request (per-species seam only).
#' @param fetch Per-species fetch seam `(name, wkt, max_records, page_size)`, one
#'   request at a time (tests). `NULL`, the default, sends the live searches of
#'   [gbif_occ_fetch_grouped()] (ADR-031).
#' @param throttle Seconds to pause between two `fetch` calls (GBIF etiquette).
#'   The live path has its own throttle in [gbif_request()].
#' @param sleep Sleep seam for the `fetch` throttle.
#' @return Data frame in the [gbif_occ_parse()] schema (all species combined),
#'   tagged with a `"failed"` attribute: the character vector of names whose
#'   lookup failed (rate-limited/offline). Failed names are NOT cached, so a
#'   later re-run retries them.
#' @noRd
gbif_occ_in_buffer <- function(names, buffer, max_records = 300L,
                               page_size = 300L, fetch = NULL,
                               throttle = 0.25, sleep = Sys.sleep) {
    empty <- gbif_occ_parse(NULL)
    names_chr <- unique(as.character(names))
    names_chr <- names_chr[!is.na(names_chr) & nzchar(names_chr)]
    if (length(names_chr) == 0L) {
        return(empty)
    }

    wkts <- vapply(gbif_occ_query_blocks(buffer), gbif_occ_wkt, character(1))
    wkts <- wkts[!is.na(wkts) & nzchar(wkts)]
    if (length(wkts) == 0L) {
        return(empty)
    }
    # The cache is keyed on the whole block set, not one polygon: changing how
    # the buffer splits changes what a species lookup means.
    store <- gbif_occ_store(paste(wkts, collapse = "\n"))

    todo <- names_chr[vapply(names_chr, function(nm) is.null(store[[nm]]), logical(1))]
    # One job per (species, block): a fragmented area has one per block.
    job_name <- rep(todo, each = length(wkts))
    job_wkt <- rep(wkts, times = length(todo))
    pages <- if (length(todo) == 0L) {
        list()
    } else if (is.null(fetch)) {
        gbif_occ_fetch_grouped(job_name, job_wkt, max_records)
    } else {
        # The throttle spaces REQUESTS, not species. A fragmented area sends one
        # request per block, and firing a species' blocks back-to-back trips
        # GBIF's 429 just as fast as firing one request per species did.
        lapply(seq_along(job_name), function(i) {
            if (i > 1L && throttle > 0) {
                sleep(throttle)
            }
            fetch(job_name[[i]], job_wkt[[i]], max_records, page_size)
        })
    }

    failed <- character(0)
    for (nm in todo) {
        mine <- pages[job_name == nm]
        # One failed block is enough to miss a real occurrence, so the species
        # stays "could not check" and uncached instead of being called absent.
        if (any(vapply(mine, function(p) isTRUE(attr(p, "gbif_error")), logical(1)))) {
            failed <- c(failed, nm)
            next
        }
        pts <- do.call(rbind, mine)
        if (nrow(pts) > 0L) {
            pts$species <- nm
        }
        store[[nm]] <- .gbif_occ_dedupe(.gbif_occ_refine(pts, buffer))
    }

    parts <- lapply(names_chr, function(nm) store[[nm]])
    parts <- parts[vapply(parts, function(p) !is.null(p) && nrow(p) > 0L, logical(1))]
    out <- if (length(parts) == 0L) empty else do.call(rbind, parts)
    attr(out, "failed") <- unique(failed)
    out
}

#' The occurrence cache of one area (one block set)
#'
#' One cache per area, so a run over three areas, and a second run, reuse what
#' GBIF already answered. A single shared cache was reset by each new area, so
#' a second run searched every area but the last one again. The number of
#' areas is capped: one R process serves every session and lives for days.
#'
#' @param key The area key: its query polygons, pasted.
#' @return The environment that maps a species name to its point table.
#' @noRd
gbif_occ_store <- function(key) {
    stores <- .gbif_occ_cache$stores
    if (is.null(stores)) {
        stores <- list()
    }
    if (is.null(stores[[key]])) {
        stores[[key]] <- new.env(parent = emptyenv())
        if (length(stores) > .GBIF_OCC_CACHE_MAX_AREAS) {
            stores <- stores[-1L]
        }
        .gbif_occ_cache$stores <- stores
    }
    stores[[key]]
}

#' Per-species "has a GBIF occurrence inside the buffer" flags
#'
#' @param occ Data frame from [gbif_occ_in_buffer()].
#' @param names Species to report on (order/names preserved).
#' @return Named logical vector (`TRUE` == at least one point inside the buffer).
#' @noRd
gbif_occ_presence <- function(occ, names) {
    keys <- as.character(names)
    hit <- if (is.data.frame(occ) && nrow(occ) > 0L) unique(as.character(occ$species)) else character(0)
    stats::setNames(keys %in% hit, keys)
}
