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
# GBIF geometry limit: occ_search/occ_data reject WKT that is too large or too
# complex (self-intersections, too many vertices, wrong winding). We do NOT send
# the raw buffer ring. gbif_occ_wkt() coarsens it to a query polygon that is
# guaranteed to fit and to *contain* the buffer (simplify -> convex hull ->
# bbox), then gbif_occ_in_buffer() refines the returned points against the exact
# buffer with sf::st_intersects. So the query polygon only trims transfer; the
# "within 10 km" decision is always the exact sf test (ADR-009).

.gbif_occ_cache <- new.env(parent = emptyenv())

# Vertex ceiling for the query polygon. GBIF tolerates far more, but a farm-scale
# buffer under this cap keeps the WKT small and well inside the API limit.
.GBIF_WKT_MAX_VERTICES <- 300L

#' Count the vertices of an sf/sfc geometry
#' @noRd
.geo_n_vertices <- function(geom) {
    nrow(sf::st_coordinates(geom))
}

#' Force a single (multi)polygon's rings to the GBIF winding (CCW exterior)
#'
#' GBIF returns the *complement* of a clockwise polygon, which would silently
#' drop every real point after the exact-buffer refine. sf does not guarantee a
#' winding, so we reorient here. Holes are left as-is (the query polygon is a
#' coarse superset; hole precision does not matter — the refine is exact).
#'
#' @param geom An `sfc` with one POLYGON or MULTIPOLYGON.
#' @return The same geometry with CCW exterior ring(s).
#' @noRd
.geo_force_ccw <- function(geom) {
    ring_ccw <- function(ring) {
        # Shoelace: negative signed area == clockwise in x/y (lon/lat) space.
        x <- ring[, 1L]
        y <- ring[, 2L]
        i <- seq_len(nrow(ring) - 1L)
        signed2 <- sum(x[i] * y[i + 1L] - x[i + 1L] * y[i])
        if (signed2 < 0) ring[rev(seq_len(nrow(ring))), , drop = FALSE] else ring
    }
    reorient <- function(poly) {
        poly[[1L]] <- ring_ccw(poly[[1L]])
        poly
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
#' Coarsens the buffer until it fits `max_vertices`, preferring shape fidelity:
#' the buffer itself if it is already small, otherwise a topology-preserving
#' `sf::st_simplify` at growing tolerances, otherwise the convex hull, and — as a
#' last resort — the bounding box. Every candidate is a superset of the buffer,
#' so the exact-buffer refine downstream never loses a real occurrence.
#'
#' @param buffer An `sfc`/`sf` polygon (the `buffer` slot from `geo_buffer()`).
#' @param max_vertices Vertex ceiling for the emitted polygon.
#' @return A WKT string (EPSG:4326, CCW), or `NA_character_` if `buffer` is empty.
#' @noRd
gbif_occ_wkt <- function(buffer, max_vertices = .GBIF_WKT_MAX_VERTICES) {
    geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(buffer)))
    if (length(geom) == 0L || all(sf::st_is_empty(geom))) {
        return(NA_character_)
    }
    geom <- sf::st_transform(geom, 4326)

    candidate <- geom
    if (.geo_n_vertices(candidate) > max_vertices) {
        for (tol in c(0.001, 0.005, 0.01, 0.05, 0.1)) {
            simplified <- sf::st_make_valid(
                sf::st_simplify(geom, dTolerance = tol, preserveTopology = TRUE)
            )
            if (!all(sf::st_is_empty(simplified)) &&
                .geo_n_vertices(simplified) <= max_vertices) {
                candidate <- simplified
                break
            }
            candidate <- simplified
        }
    }
    if (all(sf::st_is_empty(candidate)) || .geo_n_vertices(candidate) > max_vertices) {
        candidate <- sf::st_convex_hull(geom)
    }
    if (all(sf::st_is_empty(candidate)) || .geo_n_vertices(candidate) > max_vertices) {
        candidate <- sf::st_as_sfc(sf::st_bbox(geom))
    }
    sf::st_as_text(.geo_force_ccw(candidate)[[1L]])
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
#' @param res An `rgbif::occ_data` result (or its `$data`).
#' @return Data frame `species`/`decimalLongitude`/`decimalLatitude`/`datasetKey`
#'   (zero rows when nothing usable is present).
#' @noRd
gbif_occ_parse <- function(res) {
    empty <- data.frame(
        species = character(0), decimalLongitude = numeric(0),
        decimalLatitude = numeric(0), datasetKey = character(0),
        stringsAsFactors = FALSE
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
#' this buffer, and refines every returned point against the exact buffer. The
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

    wkt <- gbif_occ_wkt(buffer)
    if (is.na(wkt) || !nzchar(wkt)) {
        return(empty)
    }
    if (!identical(.gbif_occ_cache$wkt, wkt)) {
        .gbif_occ_cache$wkt <- wkt
        .gbif_occ_cache$by_name <- new.env(parent = emptyenv())
    }
    store <- .gbif_occ_cache$by_name

    failed <- character(0)
    queried <- FALSE
    parts <- lapply(names_chr, function(nm) {
        if (!is.null(store[[nm]])) {
            return(store[[nm]])
        }
        if (queried && throttle > 0) {
            sleep(throttle)
        }
        queried <<- TRUE
        pts <- fetch(nm, wkt, max_records, page_size)
        if (isTRUE(attr(pts, "gbif_error"))) {
            # Could not check this species — leave it uncached so a re-run retries.
            failed <<- c(failed, nm)
            return(empty)
        }
        if (nrow(pts) > 0L) {
            pts$species <- nm
        }
        pts <- .gbif_occ_refine(pts, buffer)
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
