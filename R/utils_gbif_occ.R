# Title: GBIF Occurrences Inside the Buffer (part 2 of the geo verification)
# Pure helpers (no Shiny) for SPEC §8 step 3: for each species, ask GBIF for
# georeferenced occurrences that fall inside the 10 km buffer built in
# utils_geo.R. Requests are spaced (throttle) and transient rate-limit errors
# (HTTP 429) are retried with exponential backoff, so a long species list does
# not trip GBIF's "Too many requests" storm. A lookup that still fails is tagged
# (gbif_error / the "failed" attribute) so callers can tell "could not check"
# from "genuinely absent" instead of collapsing both to zero points. The final
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

# Character ceiling for the query polygon's WKT. GBIF cuts the request line at
# 4 KB, and percent-encoding grows the WKT by about a third on the way into the
# URL, so 4 KB of request line is roughly 2.7 KB of WKT. 2000 leaves room for
# rgbif's other query parameters and for its own headers.
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

#' One live GBIF occurrence page (the only network line)
#'
#' Thin wrapper over `rgbif::occ_data`. Isolated so the pure layers above it are
#' testable offline: `gbif_occ_fetch_species()` takes a `query =` seam that
#' defaults to this and that tests replace with a stub.
#'
#' @param name Scientific name to filter on.
#' @param wkt Query polygon (from `gbif_occ_wkt`).
#' @param start Zero-based record offset (paging).
#' @param limit Page size.
#' @return The `rgbif::occ_data` result (a list with a `$data` slot).
#' @noRd
gbif_occ_query_page <- function(name, wkt, start, limit) {
    rgbif::occ_data(
        scientificName = name,
        geometry = wkt,
        country = "BR",
        hasCoordinate = TRUE,
        limit = limit,
        start = start
    )
}

#' TRUE when an error looks like a transient GBIF rate-limit / server issue
#'
#' HTTP 429 ("Too many requests"), throttling, and 5xx/timeouts are worth a
#' retry; a bad request, a geometry error, or "offline" is not.
#' @noRd
.gbif_occ_is_transient <- function(e) {
    msg <- tolower(conditionMessage(e))
    grepl("too many requests|429|rate.?limit|throttl|timed? ?out|timeout|502|503|504|service unavailable",
          msg)
}

#' Call a GBIF page function, retrying transient rate-limit errors with backoff
#'
#' rgbif already pauses on HTTP 429, but a long species list can still exhaust
#' its retries and start failing. This adds a bounded exponential backoff on top
#' and stays quiet (rgbif's rate-limit notices are messages/warnings we expect
#' during a retry). Non-transient errors are re-raised so the caller degrades.
#' The `sleep` seam lets tests drive the retry path without waiting.
#'
#' @param query Page function with the [gbif_occ_query_page()] signature.
#' @param name,wkt,start,limit Passed through to `query`.
#' @param sleep Sleep function (seconds); defaults to `Sys.sleep`.
#' @param max_retries Maximum extra attempts after the first.
#' @param backoff,backoff_cap Backoff base and per-wait ceiling, in seconds.
#' @return The `query` result.
#' @noRd
.gbif_occ_query_retry <- function(query, name, wkt, start, limit,
                                  sleep = Sys.sleep, max_retries = 4L,
                                  backoff = 2, backoff_cap = 30) {
    attempt <- 0L
    repeat {
        res <- tryCatch(
            suppressWarnings(suppressMessages(query(name, wkt, start, limit))),
            error = function(e) e
        )
        if (!inherits(res, "error")) {
            return(res)
        }
        if (attempt >= max_retries || !.gbif_occ_is_transient(res)) {
            stop(res)
        }
        sleep(min(backoff * (2^attempt), backoff_cap))
        attempt <- attempt + 1L
    }
}

#' Tag a point table with whether its GBIF lookup failed (vs. genuinely empty)
#' @noRd
.gbif_occ_flag_error <- function(points, failed) {
    attr(points, "gbif_error") <- isTRUE(failed)
    points
}

#' Parse a `rgbif::occ_data` result into a tidy point table
#'
#' Keeps only rows carrying both coordinates, plus the dataset source. Robust to
#' the several shapes `occ_data` returns: the `$data` tibble, a bare data frame,
#' or the "no data" sentinel.
#'
#' `key` is GBIF's own occurrence identifier. It is carried so overlapping query
#' polygons can be deduplicated by record identity: two distinct observations
#' often share a rounded coordinate and a dataset, so coordinates cannot tell a
#' repeat apart from a neighbour (LESSONS L-024).
#'
#' @param res An `rgbif::occ_data` result (or its `$data`).
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

#' Fetch GBIF occurrences for one species, paginated and capped
#'
#' Degrades gracefully: any network error (offline, timeout, rate limit) or a
#' missing `rgbif` yields an empty table plus a warning — never an abort, so one
#' bad species cannot sink the geo step (mirrors the cascade's per-provider
#' resilience, LESSONS L-003).
#'
#' @param name Scientific name.
#' @param wkt Query polygon (from `gbif_occ_wkt`).
#' @param max_records Hard cap on rows returned for this species.
#' @param page_size Records per request.
#' @param query Network seam; defaults to [gbif_occ_query_page()].
#' @param sleep Sleep seam for the retry backoff; defaults to `Sys.sleep`.
#' @return Data frame in the [gbif_occ_parse()] schema, tagged with a
#'   `"gbif_error"` attribute: `TRUE` when the lookup failed (rate-limited,
#'   offline, missing rgbif), `FALSE` when it succeeded (possibly with 0 rows).
#'   This lets callers tell "we could not check" from "genuinely absent".
#' @noRd
gbif_occ_fetch_species <- function(name, wkt, max_records = 300L,
                                   page_size = 300L,
                                   query = gbif_occ_query_page,
                                   sleep = Sys.sleep) {
    empty <- gbif_occ_parse(NULL)
    if (is.na(wkt) || !nzchar(wkt)) {
        return(.gbif_occ_flag_error(empty, FALSE))
    }
    if (identical(query, gbif_occ_query_page) &&
        !requireNamespace("rgbif", quietly = TRUE)) {
        warning("Package 'rgbif' is not installed; skipping GBIF occurrences.")
        return(.gbif_occ_flag_error(empty, TRUE))
    }
    tryCatch({
        pages <- list()
        got <- 0L
        start <- 0L
        repeat {
            want <- min(page_size, max_records - got)
            if (want <= 0L) break
            parsed <- gbif_occ_parse(
                .gbif_occ_query_retry(query, name, wkt, start, want, sleep = sleep)
            )
            if (nrow(parsed) == 0L) break
            pages[[length(pages) + 1L]] <- parsed
            got <- got + nrow(parsed)
            if (nrow(parsed) < want) break
            start <- start + nrow(parsed)
        }
        out <- if (length(pages) == 0L) empty else do.call(rbind, pages)
        .gbif_occ_flag_error(out, FALSE)
    }, error = function(e) {
        warning(sprintf("GBIF occurrence lookup failed for '%s': %s", name, conditionMessage(e)))
        .gbif_occ_flag_error(empty, TRUE)
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
#' network. Session-memoized like `provider_gbif`; the cache resets when the
#' buffer (WKT) changes.
#'
#' @param names Character vector of scientific names.
#' @param buffer The `buffer` slot from `geo_buffer()` (`sfc`, EPSG:4326).
#' @param max_records Per-species record cap.
#' @param page_size Records per request.
#' @param fetch Per-species fetch seam; defaults to [gbif_occ_fetch_species()].
#' @param throttle Seconds to pause between live species queries (GBIF etiquette
#'   — proactively spacing requests avoids the 429 storm). Cached species do not
#'   count against it.
#' @param sleep Sleep seam for the throttle; defaults to `Sys.sleep`.
#' @return Data frame in the [gbif_occ_parse()] schema (all species combined),
#'   tagged with a `"failed"` attribute: the character vector of names whose
#'   lookup failed (rate-limited/offline). Failed names are NOT cached, so a
#'   later re-run retries them.
#' @noRd
gbif_occ_in_buffer <- function(names, buffer, max_records = 300L,
                               page_size = 300L,
                               fetch = gbif_occ_fetch_species,
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
    key <- paste(wkts, collapse = "\n")
    if (!identical(.gbif_occ_cache$wkt, key)) {
        .gbif_occ_cache$wkt <- key
        .gbif_occ_cache$by_name <- new.env(parent = emptyenv())
    }
    store <- .gbif_occ_cache$by_name

    failed <- character(0)
    queried <- FALSE
    # The throttle spaces REQUESTS, not species. A fragmented area sends one
    # request per block, and firing a species' blocks back-to-back trips GBIF's
    # 429 just as fast as firing one request per species did.
    fetch_block <- function(nm, wkt) {
        if (queried && throttle > 0) {
            sleep(throttle)
        }
        queried <<- TRUE
        fetch(nm, wkt, max_records, page_size)
    }

    parts <- lapply(names_chr, function(nm) {
        if (!is.null(store[[nm]])) {
            return(store[[nm]])
        }
        pages <- lapply(wkts, function(w) fetch_block(nm, w))
        # One failed block is enough to miss a real occurrence, so the species
        # stays "could not check" and uncached instead of being called absent.
        if (any(vapply(pages, function(p) isTRUE(attr(p, "gbif_error")), logical(1)))) {
            failed <<- c(failed, nm)
            return(empty)
        }
        pts <- do.call(rbind, pages)
        if (nrow(pts) > 0L) {
            pts$species <- nm
        }
        pts <- .gbif_occ_refine(pts, buffer)
        pts <- .gbif_occ_dedupe(pts)
        store[[nm]] <- pts
        pts
    })
    parts <- parts[vapply(parts, function(p) nrow(p) > 0L, logical(1))]
    out <- if (length(parts) == 0L) empty else do.call(rbind, parts)
    attr(out, "failed") <- unique(failed)
    out
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
