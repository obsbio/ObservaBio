# GBIF occurrences inside the buffer (SPEC §8 step 3, week 6). Every test is
# offline: the network primitive gbif_occ_query_page() is never reached — the
# pure layers (WKT simplification, parsing, in-buffer refine, degradation) are
# exercised through the `fetch`/`query` seams and stubbed rgbif responses.

skip_if_not_installed("sf")

# A 10 km buffer around a Brasília-ish point, as geo_buffer() returns it.
sample_buffer <- function(cx = -47.9, cy = -15.8, dist_m = 10000) {
    pt <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(cx, cy)), crs = 4326))
    geo_buffer(pt, dist_m = dist_m)$buffer
}

reset_occ_cache <- function() {
    .gbif_occ_cache$wkt <- NULL
    .gbif_occ_cache$by_name <- NULL
}

# ---- gbif_occ_wkt -----------------------------------------------------------

# The query polygon must CONTAIN the buffer: correctness downstream is the exact
# refine, so a coarser candidate may over-fetch but must never cut the buffer.
# The buffer is shrunk by 5 m first, because st_as_text() writes 7 significant
# digits (about 1 m) and the round trip would otherwise fail on rounding alone.
expect_contains_buffer <- function(wkt, buffer) {
    query <- sf::st_as_sfc(wkt, crs = 4326)
    buf <- sf::st_union(sf::st_geometry(buffer))
    metric <- metric_crs_for(buf)
    shrunk <- sf::st_transform(
        sf::st_buffer(sf::st_transform(buf, metric), dist = -5), 4326
    )
    expect_true(sf::st_covers(query, shrunk, sparse = FALSE)[1L, 1L])
}

test_that("gbif_occ_wkt returns a WKT polygon that fits the character budget", {
    buf <- sample_buffer()
    wkt <- gbif_occ_wkt(buf)
    expect_type(wkt, "character")
    expect_match(wkt, "POLYGON")
    expect_lte(nchar(wkt), .GBIF_WKT_MAX_CHARS)
    expect_contains_buffer(wkt, buf)
})

test_that("gbif_occ_wkt simplifies a many-vertex buffer under the budget", {
    pt <- sf::st_sfc(sf::st_point(c(-47.9, -15.8)), crs = 4326)
    # A high-resolution disk: ~4 * 400 = 1600 vertices, far over the budget.
    dense <- sf::st_buffer(sf::st_transform(pt, 31983), dist = 10000, nQuadSegs = 400)
    dense <- sf::st_transform(dense, 4326)
    expect_gt(nchar(sf::st_as_text(dense[[1L]])), .GBIF_WKT_MAX_CHARS)

    wkt <- gbif_occ_wkt(dense)
    expect_lte(nchar(wkt), .GBIF_WKT_MAX_CHARS)
    expect_contains_buffer(wkt, dense)
})

test_that("gbif_occ_wkt fits a multi-part buffer under the budget", {
    # One study area with two detached parcels — a farm plus its reserve, or two
    # KML folders. Its buffer is a 2-part MULTIPOLYGON, which used to emit a
    # 5224-character WKT: a 6864-character URL, past GBIF's 4 KB request line
    # (LESSONS L-022). The old cap counted vertices and let it through.
    parcels <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
        sf::st_point(c(-47.0, -15.0)), sf::st_point(c(-46.0, -15.0)), crs = 4326
    ))
    buf <- geo_buffer(parcels)$buffer
    expect_length(unclass(sf::st_geometry(buf)[[1L]]), 2L)
    expect_gt(nchar(sf::st_as_text(sf::st_geometry(buf)[[1L]])), .GBIF_WKT_MAX_CHARS)

    wkt <- gbif_occ_wkt(buf)
    expect_lte(nchar(wkt), .GBIF_WKT_MAX_CHARS)
    expect_contains_buffer(wkt, buf)
})

test_that("gbif_occ_wkt falls back to the bounding box for a tight budget", {
    # A budget no simplify tolerance can meet forces the end of the ladder.
    buf <- sample_buffer()
    wkt <- gbif_occ_wkt(buf, max_chars = 200L)
    expect_lte(nchar(wkt), 200L)
    expect_contains_buffer(wkt, buf)
})

test_that("gbif_occ_wkt returns NA for an empty geometry", {
    empty <- sf::st_sfc(sf::st_polygon(), crs = 4326)
    expect_true(is.na(gbif_occ_wkt(empty)))
})

# ---- gbif_occ_parse ---------------------------------------------------------

test_that("gbif_occ_parse extracts coordinate points from a stubbed occ_data", {
    res <- list(
        meta = list(count = 2L),
        data = data.frame(
            scientificName = c("Handroanthus impetiginosus", "Handroanthus impetiginosus"),
            decimalLongitude = c(-47.91, -47.88),
            decimalLatitude = c(-15.81, -15.79),
            datasetKey = c("ds-1", "ds-2"),
            stringsAsFactors = FALSE
        )
    )
    pts <- gbif_occ_parse(res)
    expect_equal(nrow(pts), 2L)
    expect_named(pts, c("species", "decimalLongitude", "decimalLatitude", "datasetKey"))
    expect_equal(pts$datasetKey, c("ds-1", "ds-2"))
})

test_that("gbif_occ_parse drops rows missing coordinates and handles empties", {
    res <- list(data = data.frame(
        scientificName = c("A", "B"),
        decimalLongitude = c(-47.9, NA),
        decimalLatitude = c(-15.8, -15.7),
        datasetKey = c("ds-1", "ds-2"),
        stringsAsFactors = FALSE
    ))
    pts <- gbif_occ_parse(res)
    expect_equal(nrow(pts), 1L)
    expect_equal(pts$species, "A")

    expect_equal(nrow(gbif_occ_parse(NULL)), 0L)
    expect_equal(nrow(gbif_occ_parse(list(data = "no data found"))), 0L)
})

# ---- gbif_occ_fetch_species: degradation ------------------------------------

test_that("gbif_occ_fetch_species degrades to empty + warning on a query error", {
    failing <- function(name, wkt, start, limit) stop("network down")
    expect_warning(
        pts <- gbif_occ_fetch_species("Any species", "POLYGON((0 0,1 0,1 1,0 1,0 0))",
                                      query = failing),
        "failed"
    )
    expect_equal(nrow(pts), 0L)
})

test_that("gbif_occ_fetch_species returns empty for an NA WKT without querying", {
    boom <- function(...) stop("should not be called")
    expect_equal(nrow(gbif_occ_fetch_species("X", NA_character_, query = boom)), 0L)
})

test_that("gbif_occ_fetch_species paginates until a short page", {
    calls <- 0L
    query <- function(name, wkt, start, limit) {
        calls <<- calls + 1L
        # First page full (2 rows), second page short (1 row) -> stop.
        rows <- if (start == 0L) 2L else 1L
        list(data = data.frame(
            scientificName = rep(name, rows),
            decimalLongitude = rep(-47.9, rows),
            decimalLatitude = rep(-15.8, rows),
            datasetKey = rep("ds", rows),
            stringsAsFactors = FALSE
        ))
    }
    pts <- gbif_occ_fetch_species("Sp", "POLYGON((0 0,1 0,1 1,0 1,0 0))",
                                  max_records = 100L, page_size = 2L, query = query)
    expect_equal(nrow(pts), 3L)
    expect_equal(calls, 2L)
})

test_that("gbif_occ_fetch_species retries a transient rate-limit error, then succeeds", {
    attempts <- 0L
    flaky <- function(name, wkt, start, limit) {
        attempts <<- attempts + 1L
        if (attempts < 3L) stop("Too many requests! ... please use occ_download()")
        list(data = data.frame(
            scientificName = name, decimalLongitude = -47.9, decimalLatitude = -15.8,
            datasetKey = "ds", stringsAsFactors = FALSE
        ))
    }
    pts <- gbif_occ_fetch_species("Sp", "POLYGON((0 0,1 0,1 1,0 1,0 0))",
                                  query = flaky, sleep = function(s) invisible())
    expect_equal(attempts, 3L)          # two transient failures + one success
    expect_equal(nrow(pts), 1L)
    expect_false(isTRUE(attr(pts, "gbif_error")))
})

test_that("gbif_occ_fetch_species does not retry a non-transient error and flags it", {
    calls <- 0L
    failing <- function(name, wkt, start, limit) {
        calls <<- calls + 1L
        stop("bad request: invalid geometry")
    }
    expect_warning(
        pts <- gbif_occ_fetch_species("Sp", "POLYGON((0 0,1 0,1 1,0 1,0 0))",
                                      query = failing, sleep = function(s) invisible()),
        "failed"
    )
    expect_equal(calls, 1L)             # no retry on a non-transient error
    expect_equal(nrow(pts), 0L)
    expect_true(isTRUE(attr(pts, "gbif_error")))
})

test_that("gbif_occ_fetch_species tags a genuine empty result as not-failed", {
    query <- function(name, wkt, start, limit) list(data = "no data found")
    pts <- gbif_occ_fetch_species("Sp", "POLYGON((0 0,1 0,1 1,0 1,0 0))", query = query)
    expect_equal(nrow(pts), 0L)
    expect_false(isTRUE(attr(pts, "gbif_error")))
})

# ---- gbif_occ_in_buffer -----------------------------------------------------

test_that("gbif_occ_in_buffer with no names never touches the fetch seam", {
    reset_occ_cache()
    boom <- function(...) stop("network must not be reached")
    out <- gbif_occ_in_buffer(character(0), sample_buffer(), fetch = boom)
    expect_equal(nrow(out), 0L)
    expect_named(out, c("species", "decimalLongitude", "decimalLatitude", "datasetKey"))
})

test_that("gbif_occ_in_buffer refines returned points against the exact buffer", {
    reset_occ_cache()
    buf <- sample_buffer(dist_m = 10000)
    # One point next to the centre (inside the 10 km disk), one ~1 deg away (out).
    stub_fetch <- function(name, wkt, max_records, page_size) {
        data.frame(
            species = name,
            decimalLongitude = c(-47.90, -46.50),
            decimalLatitude = c(-15.80, -15.80),
            datasetKey = c("ds-in", "ds-out"),
            stringsAsFactors = FALSE
        )
    }
    out <- gbif_occ_in_buffer("Handroanthus impetiginosus", buf, fetch = stub_fetch)
    expect_equal(nrow(out), 1L)
    expect_equal(out$datasetKey, "ds-in")
    expect_equal(out$species, "Handroanthus impetiginosus")
})

test_that("gbif_occ_in_buffer memoizes per species for a fixed buffer", {
    reset_occ_cache()
    buf <- sample_buffer()
    calls <- 0L
    stub_fetch <- function(name, wkt, max_records, page_size) {
        calls <<- calls + 1L
        data.frame(
            species = name, decimalLongitude = -47.9, decimalLatitude = -15.8,
            datasetKey = "ds", stringsAsFactors = FALSE
        )
    }
    gbif_occ_in_buffer(c("Sp one", "Sp one"), buf, fetch = stub_fetch)
    gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch)
    expect_equal(calls, 1L)
})

test_that("gbif_occ_in_buffer records failed species and does not cache them", {
    reset_occ_cache()
    buf <- sample_buffer()
    calls <- 0L
    failing_fetch <- function(name, wkt, max_records, page_size) {
        calls <<- calls + 1L
        out <- gbif_occ_parse(NULL)
        attr(out, "gbif_error") <- TRUE          # simulate a rate-limited lookup
        out
    }
    out1 <- gbif_occ_in_buffer("Sp x", buf, fetch = failing_fetch,
                               sleep = function(s) invisible())
    expect_equal(attr(out1, "failed"), "Sp x")
    expect_equal(nrow(out1), 0L)

    # A failed lookup is not cached, so a re-run retries it.
    gbif_occ_in_buffer("Sp x", buf, fetch = failing_fetch, sleep = function(s) invisible())
    expect_equal(calls, 2L)
})

test_that("gbif_occ_in_buffer throttles between live species via the sleep seam", {
    reset_occ_cache()
    buf <- sample_buffer()
    naps <- 0L
    stub_fetch <- function(name, wkt, max_records, page_size) {
        data.frame(
            species = name, decimalLongitude = -47.9, decimalLatitude = -15.8,
            datasetKey = "ds", stringsAsFactors = FALSE
        )
    }
    gbif_occ_in_buffer(c("Sp one", "Sp two"), buf, fetch = stub_fetch,
                       throttle = 0.5, sleep = function(s) naps <<- naps + 1L)
    expect_equal(naps, 1L)               # one pause, between the two live queries
})

# ---- gbif_occ_presence ------------------------------------------------------

test_that("gbif_occ_presence reports per-species presence as a named logical", {
    occ <- data.frame(
        species = c("Sp one", "Sp one", "Sp two"),
        decimalLongitude = -47.9, decimalLatitude = -15.8, datasetKey = "ds",
        stringsAsFactors = FALSE
    )
    pres <- gbif_occ_presence(occ, c("Sp one", "Sp two", "Sp three"))
    expect_equal(pres, c("Sp one" = TRUE, "Sp two" = TRUE, "Sp three" = FALSE))

    empty <- gbif_occ_presence(gbif_occ_parse(NULL), c("Sp one"))
    expect_equal(empty, c("Sp one" = FALSE))
})
