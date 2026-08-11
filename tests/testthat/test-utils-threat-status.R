test_that("mma_lookup flags MMA-listed species by normalized key", {
    skip_if_not(file.exists(br_extdata_path("sensitive_species.rds")),
                "MMA base not embedded")
    data <- mma_load_data()
    # Pick a real listed species from the base to keep the test data-driven.
    sample <- data$scientificName[[1]]
    res <- mma_lookup(c(sample, "Definitely Notlisted"))
    expect_equal(nrow(res), 2L)
    expect_false(is.na(res$statusMMA[[1]]))
    expect_true(res$statusMMA[[1]] %in% c("CR (PEX)", "CR", "EN", "VU", "NT"))
    expect_false(is.na(res$statusSourceMMA[[1]]))
    expect_true(is.na(res$statusMMA[[2]]))
})

test_that("mma_lookup is accent- and case-insensitive", {
    skip_if_not(file.exists(br_extdata_path("sensitive_species.rds")),
                "MMA base not embedded")
    data <- mma_load_data()
    sample <- data$scientificName[[1]]
    res <- mma_lookup(toupper(sample))
    expect_false(is.na(res$statusMMA[[1]]))
})

test_that("mma_lookup picks the most restrictive category on multiple matches", {
    # Drive mma_load_data() from a stub base with two rows for one key.
    stub <- data.frame(
        scientificName = c("Aus bus", "Aus bus"),
        match_key = c("aus bus", "aus bus"),
        category = c("VU", "EN"),
        source = c("Portaria 148/2022", "Portaria 1.704/2026"),
        stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    res <- mma_lookup("Aus bus")
    expect_equal(res$statusMMA, "EN")               # EN more restrictive than VU
    expect_equal(res$statusSourceMMA, "Portaria 1.704/2026")
})

test_that("mma_lookup returns empty structure for empty input", {
    res <- mma_lookup(character(0))
    expect_equal(nrow(res), 0L)
    expect_identical(names(res), c("scientificName", "statusMMA", "statusSourceMMA"))
})

test_that("gbif_body_field reads scalars and degrades to NA", {
    expect_equal(gbif_body_field(list(code = "VU"), "code"), "VU")
    expect_true(is.na(gbif_body_field(list(code = ""), "code")))
    expect_true(is.na(gbif_body_field(list(), "code")))
    expect_true(is.na(gbif_body_field("notalist", "code")))
})

test_that("IUCN helpers degrade gracefully with no network dependency in unit tests", {
    # Empty input never touches the network.
    expect_equal(fetch_gbif_iucn_category(character(0)), character(0))
    expect_equal(gbif_match_usage_keys(character(0)), character(0))
    # All-NA keys short-circuit before any request.
    expect_true(all(is.na(fetch_gbif_iucn_category(c(NA, NA)))))
})

test_that("resolve_threat_status attaches status columns without requiring IUCN", {
    df <- data.frame(
        scientificName = c("Aus bus", "Cus dus"),
        taxonID = c(NA, NA),
        stringsAsFactors = FALSE
    )
    stub <- data.frame(
        scientificName = "Aus bus", match_key = "aus bus",
        category = "EN", source = "Portaria 148/2022", stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    out <- resolve_threat_status(df, include_iucn = FALSE)
    expect_equal(out$statusMMA, c("EN", NA))
    expect_true(all(is.na(out$iucnCategory)))
    expect_true("iucnCriteria" %in% names(out))
    expect_true(all(is.na(out$iucnCriteria)))
})

test_that("only a GBIF row's taxonID is used as a GBIF usageKey", {
    # A florabr/faunabr `taxonID` is that base's own id, NOT a GBIF usageKey:
    # feeding it to the GBIF species API would look up an unrelated taxon. Such
    # rows must fall back to matching by name.
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)

    seen <- NULL
    local_mocked_bindings(
        iucn_key_route_enabled = function(key = NULL) FALSE,
        iucn_category = function(scientific_names, taxon_ids = NA) {
            seen <<- taxon_ids
            rep("LC", length(scientific_names))
        }
    )
    df <- data.frame(
        scientificName = c("Araucaria angustifolia", "Panthera onca"),
        taxonID = c("33971", "2435099"),        # florabr id vs GBIF usageKey
        provider = c("florabr", "gbif"),
        stringsAsFactors = FALSE
    )
    resolve_threat_status(df)
    expect_equal(seen, c(NA_character_, "2435099"))
})

test_that("resolve_threat_status fills iucnCriteria from the key route", {
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    local_mocked_bindings(
        iucn_key_route_enabled = function(key = NULL) TRUE,
        fetch_iucn_rredlist = function(genus, specific, infra = NA, key = NULL) {
            data.frame(iucnCategory = "VU", iucnCriteria = "A2ac+3c",
                       stringsAsFactors = FALSE)
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", taxonID = "1", stringsAsFactors = FALSE
    )
    out <- resolve_threat_status(df)
    expect_equal(out$iucnCategory, "VU")
    expect_equal(out$iucnCriteria, "A2ac+3c")
})

test_that("a key-route miss never erases the category the GBIF route can give", {
    # The key route needs genus+specificEpithet and can miss per species. The
    # category must then fall back to the keyless GBIF route; only `criteria`
    # is exclusive to the key route.
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    local_mocked_bindings(
        iucn_key_route_enabled = function(key = NULL) TRUE,
        fetch_iucn_rredlist = function(genus, specific, infra = NA, key = NULL) {
            data.frame(iucnCategory = NA_character_, iucnCriteria = NA_character_,
                       stringsAsFactors = FALSE)
        },
        iucn_category = function(scientific_names, taxon_ids = NA) {
            rep("NT", length(scientific_names))
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", taxonID = "1", stringsAsFactors = FALSE
    )
    out <- resolve_threat_status(df)
    expect_equal(out$iucnCategory, "NT")     # rescued, not NA
    expect_true(is.na(out$iucnCriteria))     # criteria stays exclusive
})

test_that("extract_rredlist_assessment handles list, data.frame, and NULL", {
    expect_equal(
        extract_rredlist_assessment(list(category = "VU", criteria = "A2c")),
        list(category = "VU", criteria = "A2c")
    )
    df <- data.frame(category = "EN", criteria = "B1", stringsAsFactors = FALSE)
    expect_equal(extract_rredlist_assessment(df)$category, "EN")
    expect_equal(extract_rredlist_assessment(df)$criteria, "B1")
    nested <- list(red_list_category = list(code = "NT"))
    expect_equal(extract_rredlist_assessment(nested)$category, "NT")
    empty <- extract_rredlist_assessment(NULL)
    expect_true(is.na(empty$category) && is.na(empty$criteria))
})

test_that("fetch_iucn_rredlist degrades to NA when the key route is disabled", {
    old <- Sys.getenv("IUCN_KEY")
    Sys.unsetenv("IUCN_KEY")
    on.exit(if (nzchar(old)) Sys.setenv(IUCN_KEY = old), add = TRUE)
    # Disabled route: no network, all NA regardless of rredlist availability.
    res <- fetch_iucn_rredlist(c("Panthera", "Handroanthus"),
                               c("onca", "impetiginosus"))
    expect_equal(nrow(res), 2L)
    expect_true(all(is.na(res$iucnCategory)))
    expect_true(all(is.na(res$iucnCriteria)))
    expect_equal(fetch_iucn_rredlist(character(0), character(0))$iucnCategory,
                 character(0))
})

# ---------------------------------------------------------------------------
# BYO-key: the user's own IUCN key, per session (ADR-005)
# ---------------------------------------------------------------------------

# Run `expr` with IUCN_KEY set to `value` (NULL = unset), restoring it after.
local_iucn_env <- function(value, expr) {
    old <- Sys.getenv("IUCN_KEY", unset = NA_character_)
    on.exit({
        if (is.na(old)) Sys.unsetenv("IUCN_KEY") else Sys.setenv(IUCN_KEY = old)
    }, add = TRUE)
    if (is.null(value)) Sys.unsetenv("IUCN_KEY") else Sys.setenv(IUCN_KEY = value)
    force(expr)
}

test_that("as_iucn_key normalizes anything that is not a usable key to ''", {
    expect_equal(as_iucn_key(NULL), "")
    expect_equal(as_iucn_key(NA), "")
    expect_equal(as_iucn_key(character(0)), "")
    expect_equal(as_iucn_key("   "), "")
    expect_equal(as_iucn_key("  ABC  "), "ABC")
})

test_that("iucn_key_route_enabled gates on the key it is handed", {
    # Never enabled without a key, whatever the server secret says.
    local_iucn_env("SERVER-SECRET", {
        expect_false(iucn_key_route_enabled(""))
        expect_false(iucn_key_route_enabled(NULL))
        expect_false(iucn_key_route_enabled("   "))
    })
    # With a key, the only remaining gate is the package.
    expect_equal(iucn_key_route_enabled("FAKE"), has_rredlist())
})

test_that("the session key wins over the server secret; empty falls back to it", {
    local_iucn_env("SERVER-SECRET", {
        expect_equal(iucn_key_for_run("USER-KEY"), "USER-KEY")
        expect_equal(iucn_key_for_run("  USER-KEY  "), "USER-KEY")
        expect_equal(iucn_key_for_run(NULL), "SERVER-SECRET")
        expect_equal(iucn_key_for_run(""), "SERVER-SECRET")
        expect_equal(iucn_key_for_run("   "), "SERVER-SECRET")
    })
    local_iucn_env(NULL, {
        expect_equal(iucn_key_for_run(NULL), "")           # no key anywhere
        expect_equal(iucn_key_for_run("USER-KEY"), "USER-KEY")
    })
})

test_that("resolve_threat_status hands the session key to the key route", {
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    seen <- NULL
    local_mocked_bindings(
        iucn_key_route_enabled = function(key = NULL) nzchar(as_iucn_key(key)),
        fetch_iucn_rredlist = function(genus, specific, infra = NA, key = NULL) {
            seen <<- key
            data.frame(iucnCategory = "VU", iucnCriteria = "A2ac+3c",
                       stringsAsFactors = FALSE)
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", stringsAsFactors = FALSE
    )
    out <- resolve_threat_status(df, iucn_key = "USER-KEY")
    expect_equal(seen, "USER-KEY")
    expect_equal(out$iucnCriteria, "A2ac+3c")
})

test_that("no key at all preserves the keyless GBIF route (no regression)", {
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    local_mocked_bindings(
        fetch_iucn_rredlist = function(genus, specific, infra = NA, key = NULL) {
            stop("the key route must not run without a key")
        },
        iucn_category = function(scientific_names, taxon_ids = NA) {
            rep("LC", length(scientific_names))
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", stringsAsFactors = FALSE
    )
    out <- local_iucn_env(NULL, resolve_threat_status(df, iucn_key = NULL))
    expect_equal(out$iucnCategory, "LC")        # category still arrives…
    expect_true(is.na(out$iucnCriteria))        # …only criteria is missing
})

test_that("a session key degrades to the keyless route when rredlist is absent", {
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    local_mocked_bindings(
        has_rredlist = function() FALSE,
        iucn_category = function(scientific_names, taxon_ids = NA) {
            rep("EN", length(scientific_names))
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", stringsAsFactors = FALSE
    )
    out <- expect_no_error(resolve_threat_status(df, iucn_key = "FAKE"))
    expect_equal(out$iucnCategory, "EN")
    expect_true(is.na(out$iucnCriteria))
})

test_that("the rredlist cache is namespaced by key: one key never serves another", {
    skip_if_not_installed("rredlist")
    .iucn_rredlist_cache$reset()
    on.exit(.iucn_rredlist_cache$reset(), add = TRUE)

    seen_keys <- character(0)
    local_mocked_bindings(
        rl_species_latest = function(genus, species, infra = NULL, key = NULL,
                                     parse = TRUE, ...) {
            seen_keys <<- c(seen_keys, key)
            hit <- identical(key, "GOOD")
            list(category = if (hit) "VU" else NA_character_,
                 criteria = if (hit) "A2c" else NA_character_)
        },
        .package = "rredlist"
    )

    # An invalid key fails every lookup and caches the NA…
    bad <- fetch_iucn_rredlist("Panthera", "onca", key = "BAD")
    expect_true(is.na(bad$iucnCriteria))

    # …which must not poison the next run under a working key.
    good <- fetch_iucn_rredlist("Panthera", "onca", key = "GOOD")
    expect_equal(good$iucnCategory, "VU")
    expect_equal(good$iucnCriteria, "A2c")
    expect_equal(seen_keys, c("BAD", "GOOD"))   # the 2nd call really re-fetched

    # The key itself never enters the process-global cache.
    memo <- .iucn_rredlist_cache$get()
    expect_length(memo, 2L)
    expect_false(any(grepl("GOOD|BAD", names(memo))))
})

test_that("the session key never reaches the console or the output columns", {
    stub <- data.frame(
        scientificName = character(0), match_key = character(0),
        category = character(0), source = character(0), stringsAsFactors = FALSE
    )
    .mma_cache$set(stub)
    on.exit(.mma_cache$reset(), add = TRUE)
    local_mocked_bindings(
        iucn_key_route_enabled = function(key = NULL) TRUE,
        fetch_iucn_rredlist = function(genus, specific, infra = NA, key = NULL) {
            data.frame(iucnCategory = "VU", iucnCriteria = "A2c",
                       stringsAsFactors = FALSE)
        }
    )
    df <- data.frame(
        scientificName = "Panthera onca", genus = "Panthera",
        specificEpithet = "onca", stringsAsFactors = FALSE
    )
    printed <- capture.output(
        out <- resolve_threat_status(df, iucn_key = "SECRET-KEY")
    )
    expect_false(any(grepl("SECRET-KEY", printed, fixed = TRUE)))
    flat <- unlist(lapply(out, as.character))
    expect_false(any(grepl("SECRET-KEY", flat, fixed = TRUE)))
})
