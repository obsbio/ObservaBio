# Title: Taxonomic Cascade Engine
# Ported from Saira (R/utils_taxadb.R), pure pieces only. Drives the BR -> GBIF
# provider cascade over the provider contract (SPEC §16). No Shiny coupling.

#' Validate a length-1 logical flag
#' @noRd
validate_bool_flag <- function(value, arg) {
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
        stop(sprintf("%s must be a single non-NA logical.", arg))
    }
    invisible(TRUE)
}

#' Strip leading/trailing punctuation from a name token
#' @noRd
strip_name_token <- function(token) {
    gsub("^[^A-Za-z0-9x.-]+|[^A-Za-z0-9x.-]+$", "", token)
}

#' Apply canonical capitalization to scientific-name tokens
#' @noRd
format_scientific_tokens <- function(tokens, rank_tokens) {
    if (length(tokens) == 0L) {
        return(tokens)
    }
    formatted <- tokens
    for (idx in seq_along(tokens)) {
        token <- tokens[[idx]]
        lower <- tolower(token)
        if (lower %in% rank_tokens || identical(lower, "x")) {
            formatted[[idx]] <- lower
            next
        }
        if (idx == 1L) {
            formatted[[idx]] <- paste0(toupper(substr(lower, 1, 1)), substr(lower, 2, nchar(lower)))
        } else {
            formatted[[idx]] <- lower
        }
    }
    formatted
}

#' Normalize a scientific name to canonical form
#'
#' Removes author strings like `(Linnaeus, 1771)`, handles qualifiers
#' (`cf./aff./sp./spp.`; `Genus sp.` -> NA), and applies canonical capitalization.
#' The single shared normalizer for every provider (LESSONS L-006).
#'
#' @param name Character scalar (or coercible) name.
#' @param remove_authors Drop author strings/connectors/years. Default TRUE.
#' @param ignore_qualifiers Strip cf./aff./sp. markers. Default TRUE.
#' @return Canonical name, or NA_character_ for blank/qualifier-only input.
#' @noRd
normalize_scientific_name <- function(name, remove_authors = TRUE, ignore_qualifiers = TRUE) {
    validate_bool_flag(remove_authors, "remove_authors")
    validate_bool_flag(ignore_qualifiers, "ignore_qualifiers")

    if (is_blank_value(name)) {
        return(NA_character_)
    }

    value <- as.character(name)
    value <- gsub("\\|", " ", value)
    value <- gsub("\\s+", " ", trimws(value))
    if (!nzchar(value)) {
        return(NA_character_)
    }

    if (remove_authors) {
        value <- gsub("\\([^\\)]*\\)", " ", value)
    }
    value <- gsub("\\s+", " ", trimws(value))
    if (!nzchar(value)) {
        return(NA_character_)
    }

    tokens <- strsplit(value, " ", fixed = TRUE)[[1]]
    tokens <- trimws(tokens)
    tokens <- vapply(tokens, strip_name_token, FUN.VALUE = character(1))
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0L) {
        return(NA_character_)
    }

    rank_tokens <- c("subsp", "subsp.", "var", "var.", "f", "f.", "forma")
    qualifier_tokens <- name_qualifier_tokens()

    if (remove_authors) {
        # Capitalized tokens are dropped as authors and lowercase ones kept as
        # epithets, so an author's lowercase particle would survive into the name
        # ("Leucaena leucocephala (Lam.) de Wit" -> "Leucaena leucocephala de").
        # No Latin epithet is one of these, so they are treated as author residue.
        author_connectors <- c(
            "ex", "in", "et", "al.", "and", "&",
            "de", "del", "della", "den", "der", "des", "di", "du",
            "da", "das", "dos", "van", "von", "ter", "zur"
        )
        keep <- logical(length(tokens))
        for (idx in seq_along(tokens)) {
            token <- tokens[[idx]]
            lower <- tolower(token)
            if (idx == 1L) {
                keep[[idx]] <- TRUE
            } else if (lower %in% rank_tokens) {
                keep[[idx]] <- TRUE
            } else if (lower %in% author_connectors) {
                keep[[idx]] <- FALSE
            } else if (grepl("^[0-9]{4}$", token)) {
                keep[[idx]] <- FALSE
            } else if (grepl("^[A-Z]", token)) {
                keep[[idx]] <- FALSE
            } else {
                keep[[idx]] <- TRUE
            }
        }
        tokens <- tokens[keep]

        # A rank marker names the epithet that follows it, so one left dangling at
        # the end marks nothing: it is the "filius" abbreviation of an author that
        # was just dropped ("Ipomoea neurocephala Hallier f.").
        while (length(tokens) > 0L && tolower(tokens[[length(tokens)]]) %in% rank_tokens) {
            tokens <- tokens[-length(tokens)]
        }
    }
    if (length(tokens) == 0L) {
        return(NA_character_)
    }

    if (ignore_qualifiers) {
        had_sp_marker <- any(tolower(tokens) %in% c("sp", "sp.", "spp", "spp."))
        tokens <- tokens[!(tolower(tokens) %in% qualifier_tokens)]
        if (length(tokens) == 1L && had_sp_marker) {
            return(NA_character_)
        }
    }

    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0L) {
        return(NA_character_)
    }

    tokens <- format_scientific_tokens(tokens, rank_tokens)
    normalized <- gsub("\\s+", " ", trimws(paste(tokens, collapse = " ")))
    if (!nzchar(normalized)) NA_character_ else normalized
}

#' Placeholder tokens marking an unnamed species (`Genus sp.`)
#' @noRd
name_placeholder_tokens <- function() {
    c("sp", "sp.", "spp", "spp.")
}

#' Uncertainty tokens marking a doubtful identification (`Genus cf. species`)
#' @noRd
name_uncertainty_tokens <- function() {
    c("cf", "cf.", "aff", "aff.", "nr", "nr.")
}

#' Taxonomic-uncertainty/placeholder qualifier tokens (cf./aff./sp./spp.)
#'
#' The single source of truth for the markers [normalize_scientific_name()]
#' strips and [has_name_qualifier()] detects.
#' @noRd
name_qualifier_tokens <- function() {
    c(name_uncertainty_tokens(), name_placeholder_tokens())
}

#' Whether a raw name carries a taxonomic-uncertainty qualifier
#'
#' Vectorised. Splits on whitespace and pipe separators (like
#' [normalize_scientific_name()]) and reports whether any token is a
#' [name_qualifier_tokens()] marker. Used by the audit to surface uncertain
#' identifications the cascade resolved anyway (the reference script's
#' `safe_rank`; see `audit_unresolved()`).
#'
#' @param name Character vector of raw (un-normalized) names.
#' @return Logical vector, `TRUE` where a qualifier is present.
#' @noRd
has_name_qualifier <- function(name) {
    markers <- name_qualifier_tokens()
    vapply(name, function(x) {
        if (is_blank_value(x)) {
            return(FALSE)
        }
        value <- gsub("\\|", " ", as.character(x))
        tokens <- strsplit(trimws(value), "\\s+")[[1]]
        any(tolower(tokens) %in% markers)
    }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

#' Canonical capitalisation for a genus token ("panthera" -> "Panthera")
#' @noRd
format_genus_token <- function(token) {
    if (is_blank_value(token)) {
        return(NA_character_)
    }
    cleaned <- gsub("^[^A-Za-z]+|[^A-Za-z-]+$", "", as.character(token))
    if (!nzchar(cleaned)) {
        return(NA_character_)
    }
    paste0(toupper(substr(cleaned, 1, 1)), tolower(substr(cleaned, 2, nchar(cleaned))))
}

#' Canonical lower-case epithet token; NA when the token is not a plain word
#' @noRd
format_epithet_token <- function(token) {
    if (is_blank_value(token)) {
        return(NA_character_)
    }
    cleaned <- tolower(gsub("^[^A-Za-z]+|[^A-Za-z-]+$", "", as.character(token)))
    if (!grepl("^[a-z][a-z-]*$", cleaned)) {
        return(NA_character_)
    }
    cleaned
}

#' Derive `genus` / `specificEpithet` / `taxonRank` from a scientific name
#'
#' The BR bases answer with a match verdict (`check_names()` returns the accepted
#' name + family, no parsed parts), so these columns must come from the name
#' itself. Ported from Saira's `extract_scientific_name_components()`.
#' `Genus sp.` -> genus rank, no epithet; `Genus cf. species` -> the epithet after
#' the qualifier, species rank. Parses the unique names and expands back by
#' `match()` (LESSONS: unique -> resolve -> match back).
#'
#' @param scientific_names Character vector of scientific names.
#' @return Data frame with `genus`, `specificEpithet`, `taxonRank`.
#' @noRd
scientific_name_components <- function(scientific_names) {
    sn <- as.character(scientific_names)
    if (length(sn) > 1L) {
        u <- unique(sn)
        if (length(u) < length(sn)) {
            out <- scientific_name_components(u)[match(sn, u), , drop = FALSE]
            rownames(out) <- NULL
            return(out)
        }
    }

    placeholders <- name_placeholder_tokens()
    uncertain <- name_uncertainty_tokens()
    none <- list(genus = NA_character_, specificEpithet = NA_character_,
                 taxonRank = NA_character_)

    parsed <- lapply(sn, function(value) {
        if (is_blank_value(value)) {
            return(none)
        }
        cleaned <- gsub("\\s+", " ", trimws(gsub("\\|", " ", as.character(value))))
        tokens <- strsplit(cleaned, " ", fixed = TRUE)[[1]]
        tokens <- tokens[nzchar(tokens)]
        if (length(tokens) == 0L) {
            return(none)
        }

        genus <- format_genus_token(tokens[[1]])
        if (tolower(tokens[[1]]) %in% placeholders || is.na(genus)) {
            return(none)
        }
        at_genus <- list(genus = genus, specificEpithet = NA_character_,
                         taxonRank = "genus")
        if (length(tokens) == 1L) {
            return(at_genus)
        }

        second <- tolower(tokens[[2]])
        if (second %in% placeholders) {
            return(at_genus)
        }
        epithet <- if (second %in% uncertain) {
            # `Genus cf. species` -> the epithet sits after the qualifier.
            if (length(tokens) < 3L) return(at_genus)
            format_epithet_token(tokens[[3]])
        } else {
            format_epithet_token(tokens[[2]])
        }
        if (is.na(epithet)) {
            return(at_genus)
        }
        list(genus = genus, specificEpithet = epithet, taxonRank = "species")
    })

    data.frame(
        genus = vapply(parsed, function(x) x$genus, FUN.VALUE = character(1)),
        specificEpithet = vapply(parsed, function(x) x$specificEpithet,
                                 FUN.VALUE = character(1)),
        taxonRank = vapply(parsed, function(x) x$taxonRank, FUN.VALUE = character(1)),
        stringsAsFactors = FALSE
    )
}

#' Map a raw taxonomicStatus string to a coarse resolution class
#' @noRd
normalize_taxonomic_status <- function(status) {
    if (is_blank_value(status)) {
        return("unresolved")
    }
    status_lower <- tolower(as.character(status))
    if (identical(status_lower, "accepted")) {
        return("accepted")
    }
    if (grepl("synonym|misspell|misappl|invalid|unaccepted|provision", status_lower)) {
        return("synonym")
    }
    "unresolved"
}

#' Coarse status key used to rank/deduplicate cascade rows
#' @noRd
cascade_status_key <- function(status_value, taxonomic_status = NA_character_) {
    status_chr <- tolower(as.character(status_value %||% ""))
    if (length(status_chr) == 0L || is.na(status_chr) || !nzchar(status_chr)) {
        status_chr <- normalize_taxonomic_status(taxonomic_status %||% NA_character_)
    }
    if (is.na(status_chr) || !nzchar(status_chr)) {
        return("not_found")
    }
    if (identical(status_chr, "unresolved")) {
        return("ambiguous")
    }
    if (status_chr %in% c("accepted", "synonym", "ambiguous", "not_found")) {
        return(status_chr)
    }
    "not_found"
}

#' Numeric rank for a status key (accepted best -> not_found worst)
#' @noRd
cascade_status_rank <- function(status_value, taxonomic_status = NA_character_) {
    switch(cascade_status_key(status_value, taxonomic_status),
        accepted = 4L, synonym = 3L, ambiguous = 2L, not_found = 1L, 0L)
}

#' Whether an incoming row should replace an existing row for the same name
#' @noRd
should_replace_cascade_row <- function(existing_status, incoming_status,
                                       existing_taxonomic_status = NA_character_,
                                       incoming_taxonomic_status = NA_character_) {
    cascade_status_rank(incoming_status, incoming_taxonomic_status) >=
        cascade_status_rank(existing_status, existing_taxonomic_status)
}

#' Collapse multiple provider rows per name to the single best row
#'
#' Keeps the highest-ranked row per `query_name` (ties: last row wins, i.e. the
#' later/higher-priority-fallback provider). Ported from Saira.
#'
#' @param cascade_results Data frame of stacked provider results.
#' @return One row per `query_name`.
#' @noRd
collapse_cascade_results <- function(cascade_results) {
    if (is.null(cascade_results) || !is.data.frame(cascade_results) ||
        nrow(cascade_results) == 0L) {
        return(empty_canonical_result())
    }
    if (!"query_name" %in% names(cascade_results)) {
        return(cascade_results)
    }

    status_vec <- if ("validation_status" %in% names(cascade_results)) {
        as.character(cascade_results$validation_status)
    } else {
        rep(NA_character_, nrow(cascade_results))
    }
    tax_vec <- if ("taxonomicStatus" %in% names(cascade_results)) {
        as.character(cascade_results$taxonomicStatus)
    } else {
        rep(NA_character_, nrow(cascade_results))
    }

    split_rows <- split(seq_len(nrow(cascade_results)), cascade_results$query_name)
    keep_rows <- vapply(split_rows, function(idxs) {
        ranks <- vapply(idxs, function(i) {
            cascade_status_rank(status_vec[[i]], tax_vec[[i]])
        }, FUN.VALUE = integer(1))
        best_rows <- idxs[ranks == max(ranks)]
        best_rows[[length(best_rows)]]
    }, FUN.VALUE = integer(1))

    out <- cascade_results[keep_rows, , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Canonical not-found placeholder for unresolved names
#' @noRd
build_cascade_placeholder <- function(query_names, status = "not_found") {
    if (length(query_names) == 0L) {
        return(empty_canonical_result())
    }
    df <- data.frame(query_name = as.character(query_names), stringsAsFactors = FALSE)
    df$validation_status <- status
    normalize_provider_result(df, provider = NA_character_)
}

#' Run the taxonomic cascade over registered providers
#'
#' Deduplicates and normalizes names up front (LESSONS L-001), then queries each
#' provider in priority order. Only an `accepted` result short-circuits a name;
#' `synonym`/`ambiguous`/`not_found` names carry forward to the next provider
#' (L-002). A provider that errors is logged and skipped (L-003). Results are
#' collapsed to one best row per name.
#'
#' Providers run in two passes, cheap before expensive (ADR-023). Pass 1 asks a
#' base that publishes `exact_match` only for the names it holds verbatim, and
#' asks a base without that index for everything. Pass 2 is the fuzzy fallback,
#' reached only by names pass 1 left unresolved. Both passes keep priority order,
#' and rows are stacked by priority before the collapse regardless of which pass
#' produced them.
#'
#' @param query_names Character vector of raw scientific names.
#' @param providers List of `ObservaBio_provider` objects. Defaults to the registry.
#' @return Canonical-schema data frame, one row per unique normalized name, with
#'   a `provider_failures` attribute (data frame of provider/error).
#' @noRd
run_cascade <- function(query_names, providers = get_providers()) {
    empty_failures <- function() {
        data.frame(provider = character(0), error = character(0), stringsAsFactors = FALSE)
    }
    failures <- list()

    normalized <- vapply(query_names, normalize_scientific_name, FUN.VALUE = character(1))
    all_names <- unique(normalized[!is.na(normalized) & nzchar(normalized)])

    if (length(all_names) == 0L || length(providers) == 0L) {
        out <- empty_canonical_result()
        attr(out, "provider_failures") <- empty_failures()
        return(out)
    }

    providers <- providers[order(vapply(providers, function(p) p$priority, integer(1)))]

    # Which names each base holds verbatim. This is what splits the cheap pass
    # from the expensive one: check_names()/check_fauna_names() answer a name in
    # this index from a hash (~0.04 s) but burn ~0.38 s per name outside it inside
    # agrep(). On a real 1520-row list, 246 names sat outside both indexes and
    # cost 187 s of agrep between them — for zero `accepted` and zero `synonym`.
    #
    # Only a base that supplies `exact_match` can have work deferred: without that
    # index we cannot know whether it would have matched, so it keeps being asked
    # for everything in pass 1. That keeps the contract's promise intact — a new
    # base is still one line, and it never silently loses priority.
    exact <- lapply(providers, function(p) {
        if (!is.function(p$exact_match)) {
            return(character(0))
        }
        tryCatch(as.character(p$exact_match(all_names)), error = function(e) character(0))
    })

    remaining <- all_names
    collected <- list()
    collected_at <- integer(0)
    queried <- rep(list(character(0)), length(providers))
    availability <- rep(NA, length(providers))

    # Resolved on first use, not up front: a provider the cascade never reaches
    # must not show up in `provider_failures`.
    is_available <- function(i) {
        if (!is.na(availability[[i]])) {
            return(availability[[i]])
        }
        ok <- tryCatch(isTRUE(providers[[i]]$is_available()), error = function(e) FALSE)
        availability[[i]] <<- ok
        if (!ok) {
            failures[[length(failures) + 1L]] <<- data.frame(
                provider = providers[[i]]$id, error = "provider unavailable",
                stringsAsFactors = FALSE
            )
        }
        ok
    }

    # One provider call. A name is never sent to the same provider twice, so the
    # two passes below cannot produce duplicate rows for it.
    ask <- function(i, to_query) {
        provider <- providers[[i]]
        to_query <- setdiff(to_query, queried[[i]])
        if (length(to_query) == 0L) {
            return(invisible(NULL))
        }
        queried[[i]] <<- c(queried[[i]], to_query)
        matches <- tryCatch(provider$query(to_query), error = function(e) e)
        if (inherits(matches, "error")) {
            failures[[length(failures) + 1L]] <<- data.frame(
                provider = provider$id, error = conditionMessage(matches),
                stringsAsFactors = FALSE
            )
            return(invisible(NULL))
        }
        if (is.null(matches) || !is.data.frame(matches) || nrow(matches) == 0L) {
            return(invisible(NULL))
        }
        matches <- normalize_provider_result(matches, provider$id)
        collected[[length(collected) + 1L]] <<- matches
        collected_at <<- c(collected_at, i)
        accepted_names <- unique(matches$query_name[matches$validation_status == "accepted"])
        remaining <<- setdiff(remaining, accepted_names)
        invisible(NULL)
    }

    # Pass 1, cheap: a base that publishes an exact index is asked only for the
    # names it holds verbatim (a hash hit). A base without that index is still
    # asked everything, so opting out never costs it its priority.
    for (i in seq_along(providers)) {
        if (length(remaining) == 0L) {
            break
        }
        if (!is_available(i)) {
            next
        }
        ask(i, if (is.function(providers[[i]]$exact_match)) {
            intersect(remaining, exact[[i]])
        } else {
            remaining
        })
    }

    # Pass 2, expensive: the fuzzy fallback, reached only by names nothing cheaper
    # resolved. A base outside its exact index can only answer `ambiguous` (its
    # `Correct` verdict *is* the index), and `ambiguous` never outranks the
    # `accepted`/`synonym` pass 1 already has — so deferring cannot change a
    # result, only skip work. See ADR-023.
    for (i in seq_along(providers)) {
        if (length(remaining) == 0L) {
            break
        }
        if (!is.function(providers[[i]]$exact_match) || !is_available(i)) {
            next
        }
        held_by_later <- as.character(unlist(exact[seq_along(providers) > i],
                                             use.names = FALSE))
        ask(i, setdiff(remaining, setdiff(held_by_later, exact[[i]])))
    }

    # Stack in provider-priority order whatever order the passes ran in:
    # collapse_cascade_results() breaks ties with the *last* row, so a pass 2 row
    # must not outrank a later provider's pass 1 row.
    combined <- if (length(collected) > 0L) {
        do.call(rbind, collected[order(collected_at)])
    } else {
        empty_canonical_result()
    }

    resolved_any <- unique(combined$query_name)
    missing <- setdiff(all_names, resolved_any)
    if (length(missing) > 0L) {
        combined <- rbind(combined, build_cascade_placeholder(missing))
    }

    out <- collapse_cascade_results(combined)
    attr(out, "provider_failures") <- if (length(failures) > 0L) {
        do.call(rbind, failures)
    } else {
        empty_failures()
    }
    out
}
