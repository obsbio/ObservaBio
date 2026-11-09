# GBIF occurrences inside the buffer (SPEC §8 step 3, week 6). Every test is
# offline: the pure layers (WKT simplification, parsing, in-buffer refine,
# degradation) are exercised through the `fetch` seam, and the live grouped
# path through a GBIF mocked with httr2.

skip_if_not_installed("sf")

# A 10 km buffer around a Brasília-ish point, as geo_buffer() returns it.
sample_buffer <- function(cx = -47.9, cy = -15.8, dist_m = 10000) {
    pt <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(cx, cy)), crs = 4326))
    geo_buffer(pt, dist_m = dist_m)$buffer
}

reset_occ_cache <- function() {
    .gbif_occ_cache$stores <- NULL
    .gbif_match_cache$reset()
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
    expect_named(pts, c("species", "decimalLongitude", "decimalLatitude", "datasetKey", "key"))
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

# ---- gbif_occ_plan ------------------------------------------------------------

test_that("gbif_occ_plan gives a big taxon its own search and packs the others", {
    plan <- gbif_occ_plan(c("1" = 250L, "2" = 30L, "3" = 20L, "4" = 500L),
                          limit = 300L, max_keys = 10L)
    expect_equal(plan, list("4", c("1", "2", "3")))
})

test_that("gbif_occ_plan holds each shared search under the page and the URL budget", {
    set.seed(3)
    counts <- stats::setNames(sample(1:300, 60, replace = TRUE), as.character(1:60))
    plan <- gbif_occ_plan(counts, limit = 300L, max_keys = 7L)
    expect_setequal(unlist(plan), names(counts))    # every taxon, once
    expect_equal(length(unlist(plan)), length(counts))
    expect_true(all(vapply(plan, function(g) sum(counts[g]) <= 300L, logical(1))))
    expect_true(all(lengths(plan) <= 7L))
})

# ---- gbif_occ_fetch_grouped: the live path, over a mocked GBIF ----------------

square_wkt <- "POLYGON((0 0,1 0,1 1,0 1,0 0))"
no_sleep <- function(s) invisible()

occ_record <- function(key, taxon, lon = -47.9, lat = -15.8, genus = "90") {
    list(key = key, scientificName = "Aus bus", decimalLongitude = lon,
         decimalLatitude = lat, datasetKey = "ds-1", taxonKey = taxon,
         acceptedTaxonKey = taxon, speciesKey = taxon, genusKey = genus)
}

# A fake GBIF. `taxa` maps a name to its species/match answer. `pool` holds all
# the occurrence records: the facet search counts them per taxon key, and a
# search on keys returns the records that carry one of them. The returned
# environment logs the query of each occurrence search.
fake_gbif <- function(taxa, pool, env = parent.frame()) {
    log <- new.env()
    log$searches <- list()
    keys_of <- function(r) unique(as.character(unlist(r[.GBIF_OCC_TAXON_FIELDS])))
    httr2::local_mocked_responses(function(req) {
        q <- httr2::url_parse(req$url)$query
        if (grepl("/species/match", req$url, fixed = TRUE)) {
            return(httr2::response_json(body = taxa[[q$name]] %||% list(matchType = "NONE")))
        }
        log$searches[[length(log$searches) + 1L]] <- q
        if (identical(q$facet, "taxonKey")) {
            tab <- table(unlist(lapply(pool, keys_of)))
            counts <- lapply(names(tab), function(k) list(name = k, count = as.integer(tab[[k]])))
            return(httr2::response_json(body = list(
                count = length(pool), results = list(),
                facets = list(list(field = "TAXON_KEY", counts = counts))
            )))
        }
        want <- unlist(q[names(q) == "taxonKey"])
        hit <- Filter(function(r) any(keys_of(r) %in% want), pool)
        limit <- as.integer(q$limit)
        httr2::response_json(body = list(offset = 0L, limit = limit,
                                         endOfRecords = length(hit) <= limit,
                                         count = length(hit), results = utils::head(hit, limit)))
    }, env = env)
    log
}

exact <- function(key) list(usageKey = key, matchType = "EXACT", status = "ACCEPTED")

test_that("gbif_occ_fetch_grouped sends one facet search and groups the taxa with records", {
    reset_occ_cache()
    log <- fake_gbif(
        taxa = list("Aus bus" = exact(1L), "Cus dus" = exact(2L), "Eus fus" = exact(3L)),
        pool = list(occ_record(11, "1"), occ_record(12, "1", lon = -47.8), occ_record(21, "2"))
    )
    out <- gbif_occ_fetch_grouped(c("Aus bus", "Cus dus", "Eus fus", "Nomen nudum"),
                                  rep(square_wkt, 4L))
    expect_equal(out[[1L]]$key, c("11", "12"))
    expect_equal(out[[2L]]$key, "21")
    # No records and no GBIF name are real answers ("absent"), not failures.
    expect_equal(nrow(out[[3L]]), 0L)
    expect_false(isTRUE(attr(out[[3L]], "gbif_error")))
    expect_equal(nrow(out[[4L]]), 0L)
    expect_false(isTRUE(attr(out[[4L]], "gbif_error")))
    # One facet search, then one search for the two taxa with records.
    expect_length(log$searches, 2L)
    expect_equal(log$searches[[1L]]$facet, "taxonKey")
    expect_setequal(unlist(log$searches[[2L]][names(log$searches[[2L]]) == "taxonKey"]),
                    c("1", "2"))
})

test_that("gbif_occ_fetch_grouped gives a synonym the records of its accepted taxon", {
    reset_occ_cache()
    fake_gbif(
        taxa = list("Amazilia fimbriata" = list(usageKey = 2476422L, acceptedUsageKey = 10941997L,
                                                matchType = "EXACT", status = "SYNONYM")),
        pool = list(occ_record(1, "10941997"))
    )
    out <- gbif_occ_fetch_grouped("Amazilia fimbriata", square_wkt)
    expect_equal(out[[1L]]$key, "1")
})

test_that("gbif_occ_fetch_grouped splits the records of a shared search by taxon", {
    # A genus and one of its species share a search: the genus gets both
    # records, the species only its own.
    reset_occ_cache()
    fake_gbif(
        taxa = list("Aus bus" = exact(1L), "Aus" = exact(90L)),
        pool = list(occ_record(11, "1"), occ_record(21, "2"))
    )
    out <- gbif_occ_fetch_grouped(c("Aus bus", "Aus"), rep(square_wkt, 2L))
    expect_equal(out[[1L]]$key, "11")
    expect_setequal(out[[2L]]$key, c("11", "21"))
})

test_that("gbif_occ_fetch_grouped fails the species of a failed facet search", {
    reset_occ_cache()
    httr2::local_mocked_responses(function(req) {
        if (grepl("/species/match", req$url, fixed = TRUE)) {
            return(httr2::response_json(body = exact(1L)))
        }
        httr2::response(status_code = 503L, headers = list("Retry-After" = "0"))
    })
    out <- gbif_occ_fetch_grouped("Aus bus", square_wkt)
    expect_true(isTRUE(attr(out[[1L]], "gbif_error")))
})

test_that("gbif_occ_fetch_grouped fails a shared search whose page misses records", {
    # The facet counts say one page holds both taxa, but the answer has more
    # records than the page: some records are lost.
    reset_occ_cache()
    httr2::local_mocked_responses(function(req) {
        q <- httr2::url_parse(req$url)$query
        if (grepl("/species/match", req$url, fixed = TRUE)) {
            return(httr2::response_json(body = exact(if (q$name == "Aus bus") 1L else 2L)))
        }
        if (identical(q$facet, "taxonKey")) {
            return(httr2::response_json(body = list(facets = list(list(counts = list(
                list(name = "1", count = 1L), list(name = "2", count = 1L)
            ))))))
        }
        httr2::response_json(body = list(endOfRecords = FALSE, count = 400L,
                                         results = list(occ_record(11, "1"))))
    })
    out <- gbif_occ_fetch_grouped(c("Aus bus", "Cus dus"), rep(square_wkt, 2L))
    expect_true(all(vapply(out, function(p) isTRUE(attr(p, "gbif_error")), logical(1))))
})

test_that("gbif_occ_fetch_grouped fails a species whose GBIF lookup failed", {
    reset_occ_cache()
    httr2::local_mocked_responses(function(req) {
        httr2::response(status_code = 503L, headers = list("Retry-After" = "0"))
    })
    out <- gbif_occ_fetch_grouped("Aus bus", square_wkt)
    expect_true(isTRUE(attr(out[[1L]], "gbif_error")))
})

# ---- gbif_occ_query_blocks --------------------------------------------------

# A buffer of N detached parcels, spread far enough that one polygon cannot
# cover them tightly.
scattered_buffer <- function(n, spread = 1) {
    set.seed(1)
    pts <- lapply(seq_len(n), function(i) {
        sf::st_point(c(-47 + stats::runif(1, 0, spread), -15 + stats::runif(1, 0, spread)))
    })
    geo_buffer(sf::st_sf(id = seq_len(n), geometry = sf::st_sfc(pts, crs = 4326)))$buffer
}

test_that("gbif_occ_query_blocks leaves a compact buffer as one block", {
    blocks <- gbif_occ_query_blocks(sample_buffer())
    expect_length(blocks, 1L)
})

test_that("gbif_occ_query_blocks splits a fragmented buffer and loses nothing", {
    buf <- scattered_buffer(12L, spread = 1.5)
    # One polygon over all twelve parcels over-fetches many times over.
    expect_gt(.gbif_occ_overfetch(sf::st_union(sf::st_geometry(buf))),
              .GBIF_QUERY_MAX_OVERFETCH)

    blocks <- gbif_occ_query_blocks(buf)
    expect_gt(length(blocks), 1L)
    # The blocks still cover the buffer. The tolerance is relative because
    # st_union on lon/lat leaves square-metre residue on a shared edge.
    union <- sf::st_union(do.call(c, blocks))
    exact <- sf::st_union(sf::st_geometry(buf))
    residue <- as.numeric(sf::st_area(sf::st_sym_difference(union, exact)))
    expect_lt(residue / as.numeric(sf::st_area(exact)), 1e-5)
})

test_that("gbif_occ_query_blocks holds every block under the over-fetch ceiling", {
    for (block in gbif_occ_query_blocks(scattered_buffer(12L, spread = 1.5))) {
        expect_lte(.gbif_occ_overfetch(block), .GBIF_QUERY_MAX_OVERFETCH)
    }
})

test_that("gbif_occ_query_blocks costs nothing for an empty buffer", {
    expect_length(gbif_occ_query_blocks(sf::st_sfc(sf::st_polygon(), crs = 4326)), 0L)
})

# ---- gbif_occ_in_buffer -----------------------------------------------------

test_that("gbif_occ_in_buffer with no names never touches the fetch seam", {
    reset_occ_cache()
    boom <- function(...) stop("network must not be reached")
    out <- gbif_occ_in_buffer(character(0), sample_buffer(), fetch = boom)
    expect_equal(nrow(out), 0L)
    expect_named(out, c("species", "decimalLongitude", "decimalLatitude", "datasetKey", "key"))
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

test_that("gbif_occ_in_buffer keeps one cache per area", {
    # One shared cache was reset by each new area, so a second run searched
    # every area but the last one again.
    reset_occ_cache()
    calls <- 0L
    stub_fetch <- function(name, wkt, max_records, page_size) {
        calls <<- calls + 1L
        gbif_occ_parse(NULL)
    }
    area_a <- sample_buffer(-47.9, -15.8)
    area_b <- sample_buffer(-45.0, -12.0)
    for (buf in list(area_a, area_b, area_a, area_b)) {
        gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch, sleep = no_sleep)
    }
    expect_equal(calls, 2L)             # the second pass asks GBIF nothing
})

test_that("gbif_occ_in_buffer sends the live grouped searches by default", {
    reset_occ_cache()
    # Per species, one point near the buffer centre and one about 1 degree away.
    fake_gbif(
        taxa = list("Sp one" = exact(1L), "Sp two" = exact(2L)),
        pool = list(occ_record(11, "1", -47.90, -15.80), occ_record(12, "1", -46.50, -15.80),
                    occ_record(21, "2", -47.90, -15.80), occ_record(22, "2", -46.50, -15.80))
    )
    out <- gbif_occ_in_buffer(c("Sp one", "Sp two"), sample_buffer(), sleep = no_sleep)
    expect_equal(sort(unique(out$species)), c("Sp one", "Sp two"))
    expect_true(all(out$decimalLongitude == -47.90))   # refined to the buffer
    expect_length(attr(out, "failed"), 0L)
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

# ---- gbif_occ_in_buffer over query blocks -----------------------------------

test_that("gbif_occ_in_buffer sends one request per block per species", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    n_blocks <- length(gbif_occ_query_blocks(buf))
    expect_gt(n_blocks, 1L)

    calls <- 0L
    stub_fetch <- function(name, wkt, max_records, page_size) {
        calls <<- calls + 1L
        gbif_occ_parse(NULL)
    }
    gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch, sleep = function(s) invisible())
    expect_equal(calls, n_blocks)
})

test_that("gbif_occ_in_buffer keeps a point that only one block returns", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    # The point sits in the first parcel, so only the block covering it answers.
    target <- sf::st_coordinates(sf::st_centroid(
        suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(buf)), "POLYGON"))[1L]
    ))
    stub_fetch <- function(name, wkt, max_records, page_size) {
        inside <- lengths(sf::st_intersects(
            sf::st_sfc(sf::st_point(target[1L, c("X", "Y")]), crs = 4326),
            sf::st_as_sfc(wkt, crs = 4326)
        )) > 0L
        if (!inside) {
            return(gbif_occ_parse(NULL))
        }
        data.frame(species = name, decimalLongitude = target[1L, "X"],
                   decimalLatitude = target[1L, "Y"], datasetKey = "ds-1",
                   stringsAsFactors = FALSE)
    }
    out <- gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch,
                              sleep = function(s) invisible())
    expect_equal(nrow(out), 1L)
    expect_equal(out$datasetKey, "ds-1")
})

test_that("gbif_occ_in_buffer drops a record two overlapping blocks both return", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    centre <- sf::st_coordinates(sf::st_centroid(
        suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(buf)), "POLYGON"))[1L]
    ))
    # Every block answers with the same record, as overlapping query polygons do.
    stub_fetch <- function(name, wkt, max_records, page_size) {
        data.frame(species = name, decimalLongitude = centre[1L, "X"],
                   decimalLatitude = centre[1L, "Y"], datasetKey = "ds-dup",
                   stringsAsFactors = FALSE)
    }
    out <- gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch,
                              sleep = function(s) invisible())
    expect_equal(nrow(out), 1L)
})

test_that("gbif_occ_in_buffer marks a species failed when any one block fails", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    seen <- 0L
    # The first block answers cleanly, the second cannot be checked. Reporting
    # "absent" here would hide whatever the failed block covered.
    stub_fetch <- function(name, wkt, max_records, page_size) {
        seen <<- seen + 1L
        out <- gbif_occ_parse(NULL)
        attr(out, "gbif_error") <- seen == 2L
        out
    }
    out <- gbif_occ_in_buffer("Sp x", buf, fetch = stub_fetch,
                              sleep = function(s) invisible())
    expect_equal(attr(out, "failed"), "Sp x")
    expect_equal(nrow(out), 0L)
})

test_that("gbif_occ_in_buffer keeps distinct records that share a coordinate", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    centre <- sf::st_coordinates(sf::st_centroid(
        suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(buf)), "POLYGON"))[1L]
    ))
    # Two separate observations at the same rounded coordinate, from the same
    # dataset — routine for iNaturalist records. Only the GBIF key tells them
    # apart, and every block returns both.
    stub_fetch <- function(name, wkt, max_records, page_size) {
        data.frame(
            species = name,
            decimalLongitude = rep(centre[1L, "X"], 2L),
            decimalLatitude = rep(centre[1L, "Y"], 2L),
            datasetKey = c("ds-1", "ds-1"),
            key = c("101", "102"),
            stringsAsFactors = FALSE
        )
    }
    out <- gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch,
                              sleep = function(s) invisible())
    expect_equal(nrow(out), 2L)
    expect_setequal(out$key, c("101", "102"))
})

test_that("gbif_occ_in_buffer collapses the same record seen by two blocks", {
    reset_occ_cache()
    buf <- scattered_buffer(12L, spread = 1.5)
    centre <- sf::st_coordinates(sf::st_centroid(
        suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(buf)), "POLYGON"))[1L]
    ))
    stub_fetch <- function(name, wkt, max_records, page_size) {
        data.frame(species = name, decimalLongitude = centre[1L, "X"],
                   decimalLatitude = centre[1L, "Y"], datasetKey = "ds-1",
                   key = "777", stringsAsFactors = FALSE)
    }
    out <- gbif_occ_in_buffer("Sp one", buf, fetch = stub_fetch,
                              sleep = function(s) invisible())
    expect_equal(nrow(out), 1L)
    expect_equal(out$key, "777")
})

# ---- .geo_force_ccw: winding of exterior and interior rings -----------------

test_that(".geo_force_ccw winds the exterior CCW and the holes CW", {
    # GBIF rejects a polygon whose interior ring runs anticlockwise, with
    # "Polygon with anticlockwise interior ring", and the whole lookup fails.
    # A tight query polygon over scattered parcels does grow holes.
    signed2 <- function(ring) {
        i <- seq_len(nrow(ring) - 1L)
        sum(ring[i, 1L] * ring[i + 1L, 2L] - ring[i + 1L, 1L] * ring[i, 2L])
    }
    outer <- cbind(c(0, 4, 4, 0, 0), c(0, 0, 4, 4, 0))          # CCW
    hole  <- cbind(c(1, 2, 2, 1, 1), c(1, 1, 2, 2, 1))          # also CCW: wrong
    poly <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 4326)
    expect_gt(signed2(hole), 0)

    rings <- unclass(.geo_force_ccw(poly)[[1L]])
    expect_gt(signed2(rings[[1L]]), 0)   # exterior anticlockwise
    expect_lt(signed2(rings[[2L]]), 0)   # hole clockwise
})

test_that(".geo_force_ccw fixes the holes of every part of a MULTIPOLYGON", {
    signed2 <- function(ring) {
        i <- seq_len(nrow(ring) - 1L)
        sum(ring[i, 1L] * ring[i + 1L, 2L] - ring[i + 1L, 1L] * ring[i, 2L])
    }
    part <- function(dx) list(
        cbind(c(0, 4, 4, 0, 0) + dx, c(0, 0, 4, 4, 0)),
        cbind(c(1, 2, 2, 1, 1) + dx, c(1, 1, 2, 2, 1))
    )
    multi <- sf::st_sfc(sf::st_multipolygon(list(part(0), part(10))), crs = 4326)
    for (p in unclass(.geo_force_ccw(multi)[[1L]])) {
        expect_gt(signed2(p[[1L]]), 0)
        expect_lt(signed2(p[[2L]]), 0)
    }
})

# ---- Audit 2026-11-08: A09 -------------------------------------------------

test_that("gbif_occ_fetch_grouped marks the first page of a big taxon as truncated", {
    reset_occ_cache()
    pool <- lapply(seq_len(301L), function(k) occ_record(k, "1", lon = -40))
    fake_gbif(taxa = list("Aus bus" = exact(1L), "Cus dus" = exact(2L)),
              pool = c(pool, list(occ_record(9001, "2"))))
    out <- gbif_occ_fetch_grouped(c("Aus bus", "Cus dus"), rep(square_wkt, 2L))
    expect_equal(nrow(out[[1L]]), 300L)
    expect_false(isTRUE(attr(out[[1L]], "gbif_error")))
    expect_true(isTRUE(attr(out[[1L]], "gbif_truncated")))
    expect_false(isTRUE(attr(out[[2L]], "gbif_truncated")))
})

test_that("gbif_occ_in_buffer reports a truncated species, also from the cache", {
    reset_occ_cache()
    buf <- sample_buffer()
    calls <- 0L
    far_page <- function(name, wkt, max_records, page_size) {
        calls <<- calls + 1L
        out <- data.frame(species = name, decimalLongitude = -46.5,
                          decimalLatitude = -15.8, datasetKey = "ds-far",
                          stringsAsFactors = FALSE)
        attr(out, "gbif_truncated") <- TRUE
        out
    }
    out <- gbif_occ_in_buffer("Aus bus", buf, fetch = far_page, sleep = no_sleep)
    expect_equal(nrow(out), 0L)
    expect_equal(attr(out, "truncated"), "Aus bus")
    again <- gbif_occ_in_buffer("Aus bus", buf, fetch = far_page, sleep = no_sleep)
    expect_equal(attr(again, "truncated"), "Aus bus")
    expect_equal(calls, 1L)
})
