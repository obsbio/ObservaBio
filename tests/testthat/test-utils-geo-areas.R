# Study-area linking (locality <-> shapefile). Pure and offline: no geometry is
# read here, only the record -> area assignment and the per-area species slice
# that run_geo_verification() consumes.

mk_area <- function(name, localities = character(0)) {
    list(name = name, dir = tempfile(), localities = localities)
}

sample_records <- function() {
    data.frame(
        scientificName = c("Aaa bbb", "Ccc ddd", "Aaa bbb", "Eee fff"),
        locality = c("RPPN Rio do Brasil", "Fazenda Trijunção",
                     "Fazenda Trijunção", "Sítio Sem Área"),
        stringsAsFactors = FALSE
    )
}

# ---- area_key ---------------------------------------------------------------

test_that("area_key folds case, accents and punctuation", {
    expect_equal(area_key("RPPN Rio do Brasil"), area_key("rppn_rio_do_brasil"))
    expect_equal(area_key("Fazenda Trijunção"), area_key("FAZENDA TRIJUNCAO"))
})

# ---- area_slug / merge_areas / drop_area ------------------------------------

test_that("area_slug is input-id safe and agrees with area_key", {
    expect_equal(area_slug("RPPN Rio do Brasil"), "rppn_rio_do_brasil")
    expect_equal(area_slug("Fazenda Trijunção"), "fazenda_trijuncao")
    expect_match(area_slug("Área 1 (nova)!"), "^[a-z0-9_]+$")
    # Same area by name == same slug, which is what dedup and input ids rely on.
    expect_equal(area_slug("fazenda bananal"), area_slug("FAZENDA  BANANAL"))
    # A name with nothing to slug still yields a usable id.
    expect_equal(area_slug("---"), "area")
})

test_that("merge_areas accumulates across separate uploads", {
    # The regression: fileInput replaces its selection, so a second upload used
    # to drop the first area silently.
    held <- merge_areas(list(), list(mk_area("fazenda bananal")))
    got <- merge_areas(held, list(mk_area("RPPN Rio do Brasil")))
    expect_equal(vapply(got, function(a) a$name, character(1)),
                 c("fazenda bananal", "RPPN Rio do Brasil"))
})

test_that("merge_areas replaces an area re-sent under the same name, in place", {
    held <- list(mk_area("A"), mk_area("B"))
    fixed <- mk_area("a")                 # same slug, corrected archive
    fixed$dir <- "/tmp/corrected"
    got <- merge_areas(held, list(fixed))
    expect_length(got, 2L)
    expect_equal(got[[1]]$dir, "/tmp/corrected")   # kept its row
    expect_equal(got[[2]]$name, "B")
})

test_that("merge_areas takes several areas from one upload and tolerates none", {
    got <- merge_areas(list(), list(mk_area("A"), mk_area("B")))
    expect_length(got, 2L)
    expect_equal(merge_areas(got, list()), got)
})

test_that("drop_area removes by slug and leaves the rest alone", {
    held <- list(mk_area("fazenda bananal"), mk_area("RPPN Rio do Brasil"))
    got <- drop_area(held, "fazenda_bananal")
    expect_length(got, 1L)
    expect_equal(got[[1]]$name, "RPPN Rio do Brasil")
    expect_equal(drop_area(held, "nao_existe"), held)
    expect_equal(drop_area(held, NULL), held)
    expect_length(drop_area(list(), "x"), 0L)
})

# ---- assign_record_areas ----------------------------------------------------

test_that("assign_record_areas links records by normalized locality", {
    areas <- list(
        mk_area("RPPN", "rppn_rio_do_brasil"),          # file-style spelling
        mk_area("Trijuncao", "Fazenda Trijunção")
    )
    got <- assign_record_areas(sample_records(), areas)
    expect_equal(got, c("RPPN", "Trijuncao", "Trijuncao", NA))
})

test_that("a single area with nothing to link by claims every record", {
    # No locality column at all.
    records <- data.frame(scientificName = c("Aaa bbb", "Ccc ddd"),
                          stringsAsFactors = FALSE)
    expect_equal(assign_record_areas(records, list(mk_area("Única"))),
                 c("Única", "Única"))

    # Locality column present, but the user linked nothing.
    expect_equal(assign_record_areas(sample_records(), list(mk_area("Única"))),
                 rep("Única", 4L))
})

test_that("two unlinked areas verify nothing (the fallback is single-area only)", {
    areas <- list(mk_area("A"), mk_area("B"))
    expect_true(all(is.na(assign_record_areas(sample_records(), areas))))
})

test_that("a locality claimed by two areas is dropped, not assigned arbitrarily", {
    areas <- list(
        mk_area("A", c("RPPN Rio do Brasil", "Fazenda Trijunção")),
        mk_area("B", "Fazenda Trijunção")               # conflicting claim
    )
    got <- assign_record_areas(sample_records(), areas)
    expect_equal(got[[1L]], "A")                        # unambiguous, still linked
    expect_true(all(is.na(got[c(2L, 3L)])))             # ambiguous -> unlinked
})

test_that("assign_record_areas tolerates no areas and no records", {
    expect_true(all(is.na(assign_record_areas(sample_records(), list()))))
    expect_length(assign_record_areas(sample_records()[0, ], list(mk_area("A"))), 0L)
})

# ---- unlinked_localities ----------------------------------------------------

test_that("unlinked_localities names the localities left out of the check", {
    areas <- list(mk_area("RPPN", "RPPN Rio do Brasil"))
    records <- sample_records()
    got <- assign_record_areas(records, areas)
    expect_equal(unlinked_localities(records, got),
                 c("Fazenda Trijunção", "Sítio Sem Área"))
})

test_that("unlinked_localities is empty when every record is linked", {
    areas <- list(mk_area("A", c("RPPN Rio do Brasil", "Fazenda Trijunção",
                                 "Sítio Sem Área")))
    records <- sample_records()
    expect_length(unlinked_localities(records, assign_record_areas(records, areas)), 0L)
})

# ---- geo_name_map_by_area ---------------------------------------------------

test_that("geo_name_map_by_area slices species per area and resolves synonyms", {
    records <- sample_records()
    areas <- list(mk_area("RPPN", "RPPN Rio do Brasil"),
                  mk_area("Trijuncao", "Fazenda Trijunção"))
    record_areas <- assign_record_areas(records, areas)
    q <- function(x) vapply(x, normalize_scientific_name, character(1), USE.NAMES = FALSE)
    cascade <- data.frame(
        query_name = q(c("Aaa bbb", "Ccc ddd")),
        scientificName = c("Aaa bbb", "Ccc aceito"),   # 2nd is a resolved synonym
        stringsAsFactors = FALSE
    )

    m <- geo_name_map_by_area(records, cascade, record_areas)
    # "Aaa bbb" appears in both areas; "Eee fff" is unlinked and contributes nothing.
    expect_equal(nrow(m), 3L)
    expect_setequal(m$area, c("RPPN", "Trijuncao"))
    expect_equal(sort(m$query_name[m$area == "Trijuncao"]), q(c("Aaa bbb", "Ccc ddd")))
    expect_equal(m$scientificName[m$query_name == q("Ccc ddd")], "Ccc aceito")
    expect_false(q("Eee fff") %in% m$query_name)
})

test_that("geo_name_map_by_area dedupes a species repeated inside one area", {
    records <- data.frame(
        scientificName = c("Aaa bbb", "Aaa bbb"),
        locality = c("Área 1", "Área 1"), stringsAsFactors = FALSE
    )
    areas <- list(mk_area("Um", "Área 1"))
    m <- geo_name_map_by_area(records, NULL, assign_record_areas(records, areas))
    expect_equal(nrow(m), 1L)
    expect_equal(m$scientificName, "Aaa bbb")   # no cascade -> its own name
})

test_that("geo_name_map_by_area returns an empty frame when nothing is linked", {
    records <- sample_records()
    m <- geo_name_map_by_area(records, NULL, rep(NA_character_, nrow(records)))
    expect_equal(nrow(m), 0L)
    expect_named(m, c("area", "query_name", "scientificName"))
})

# ---- area_flag_key ----------------------------------------------------------

test_that("area_flag_key composes the join key and refuses an unlinked record", {
    expect_equal(area_flag_key("RPPN", "aaa bbb"), "rppn|aaa bbb")
    expect_true(is.na(area_flag_key(NA_character_, "aaa bbb")))
    expect_true(is.na(area_flag_key("RPPN", NA_character_)))
    # Zero-length in, zero-length out: paste0() would have invented a key.
    expect_length(area_flag_key(character(0), character(0)), 0L)
})
