# Build the invasive-species lookup used to cross-check uploaded species lists.
#
# This is a STATUS CROSS-CHECK, like the MMA list (SPEC §5) -- not a taxonomic
# provider. It never resolves names; it only answers "is this taxon on a national
# invasive-species list, and which one?".
#
# Sources (data-raw/, public national lists, regenerated yearly):
#   invasive_horus_2023.csv   Lista de especies exoticas invasoras do Brasil,
#                             Instituto Horus, 2023 (490 rows).
#   invasive_griis_brasil.csv Global Register of Introduced and Invasive Species
#                             (GRIIS) -- Brazil (1215 rows).
#   invasive_ucs_federais.csv Lista de Especies Exoticas Invasoras em Unidades de
#                             Conservacao Federais (2348 rows -- ONE ROW PER
#                             OCCURRENCE PER UC, so many rows per species).
#
# Output: inst/extdata/invasive_species.rds (+ .meta.json sidecar)
#   One row per match_key, with scientificName (clean, author-free), match_key and
#   invasiveSource (every source list that carries the taxon, joined by "; ").
#   The match_key comes from invasive_match_key() -- the SAME recipe the runtime
#   lookup applies -- so the base and validated names always compare identically
#   (LESSONS L-006). Everything else in the CSVs (guid, authorship, vernacular
#   name, per-UC locality, introduction motive, ...) is noise for this job and is
#   dropped: only what invasive_lookup() needs survives into the RDS.
#
# The three CSVs are raw and dirty. Quirks handled below:
#   - Horus/GRIIS carry a LABEL ROW (row 2) whose cells repeat the column names
#     ("Nome Cientifico", "kingdom", ...) instead of holding data. It is dropped
#     by content, not by position.
#   - `scientificName` is EMPTY on the rows whose names the source's own backbone
#     failed to resolve -- 96 rows across the three files, and they are real
#     listings (Xenopus laevis, Equus caballus, Anolis sagrei, hybrids...). The
#     raw name therefore falls back to `Supplied Name`; dropping blank
#     `scientificName` rows would silently lose those species.
#   - `Genus sp.` listings normalize to NA (normalize_scientific_name) and drop
#     out on purpose: a genus-level listing cannot be matched to a species without
#     flagging every congener.
#
# To regenerate (from the project root):
#   Rscript -e "source('data-raw/generate_invasive_species.R')"

pkgload::load_all(".", quiet = TRUE)

out_path <- file.path("inst", "extdata", "invasive_species.rds")
meta_path <- base_meta_path(out_path)

sources <- list(
  list(label = "Instituto Hórus 2023", file = "invasive_horus_2023.csv"),
  list(label = "GRIIS Brasil", file = "invasive_griis_brasil.csv"),
  list(label = "UCs Federais", file = "invasive_ucs_federais.csv")
)

# The label row repeats the column names. Compared on the normalized value so the
# check needs no accented literal ("Nome Cientifico" -> "nome cientifico").
label_row_keys <- c("nome cientifico", "scientificname", "supplied name")

read_source <- function(src) {
  df <- utils::read.csv(
    file.path("data-raw", src$file),
    check.names = FALSE, stringsAsFactors = FALSE,
    colClasses = "character", encoding = "UTF-8"
  )
  column <- function(nm) {
    if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
  }
  # `scientificName` is the source's resolved name and wins; `Supplied Name` is
  # the fallback for the rows it left blank (see the header note).
  raw_name <- fill_blank_values(column("scientificName"), column("Supplied Name"))
  keep <- !normalize_for_matching(raw_name) %in% label_row_keys
  data.frame(
    raw_name = raw_name[keep],
    source = src$label,
    stringsAsFactors = FALSE
  )
}

rows <- do.call(rbind, lapply(sources, read_source))

# Author-free canonical name for display; match_key for joining. Both go through
# the package's shared normalizer, so a name with an author, a qualifier or odd
# spacing lands on the same key from either side.
rows$scientificName <- vapply(
  rows$raw_name,
  function(nm) {
    normalize_scientific_name(nm, remove_authors = TRUE, ignore_qualifiers = TRUE)
  },
  FUN.VALUE = character(1), USE.NAMES = FALSE
)
rows$match_key <- invasive_match_key(rows$raw_name)

usable <- !is.na(rows$match_key) & nzchar(rows$match_key) & !is.na(rows$scientificName)
dropped <- rows[!usable, , drop = FALSE]
rows <- rows[usable, , drop = FALSE]

# Union the three lists: one row per taxon, keeping every source that lists it.
# unique() preserves first-appearance order, and the rows are stacked in `sources`
# order, so invasiveSource always reads "Instituto Horus 2023; GRIIS Brasil; ...".
by_key <- split(rows, rows$match_key)
invasive_species <- do.call(rbind, lapply(by_key, function(group) {
  data.frame(
    scientificName = group$scientificName[[1]],
    match_key = group$match_key[[1]],
    invasiveSource = paste(unique(group$source), collapse = "; "),
    stringsAsFactors = FALSE
  )
}))
invasive_species <- invasive_species[order(invasive_species$scientificName), , drop = FALSE]
rownames(invasive_species) <- NULL

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
saveRDS(invasive_species, file = out_path, version = 2)

n_by_source <- lapply(sources, function(src) {
  length(unique(rows$match_key[rows$source == src$label]))
})
names(n_by_source) <- vapply(sources, function(src) src$label, character(1))

jsonlite::write_json(
  list(
    base = "invasive_species",
    version = format(Sys.Date(), "%Y-%m"),
    date = format(Sys.Date(), "%Y-%m-%d"),
    sources = lapply(sources, function(src) {
      list(label = src$label, file = src$file,
           n_species = n_by_source[[src$label]])
    }),
    n_species = nrow(invasive_species),
    n_by_source = n_by_source
  ),
  meta_path, auto_unbox = TRUE, pretty = TRUE
)

message(sprintf(
  "Saved %d invasive species (from %d listed rows, %d unusable) to %s",
  nrow(invasive_species), nrow(rows), nrow(dropped), out_path
))
message(paste(
  sprintf("  %s: %d", names(n_by_source), unlist(n_by_source)),
  collapse = "\n"
))
