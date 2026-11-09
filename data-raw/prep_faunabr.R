# data-raw/prep_faunabr.R
# Regenerate the embedded Fauna do Brasil base. Run LOCALLY (never at runtime).
# Downloads the official Catálogo Taxonômico da Fauna do Brasil dataset, keeps
# the "short" column set, and writes inst/extdata/faunabr_observabio.rds + a sidecar.
#
# Usage:  Rscript data-raw/prep_faunabr.R
#         Rscript data-raw/prep_faunabr.R <archive.zip> <version>
#         The second form builds the base from a DwC-A archive that is already
#         on disk, for when the JBRJ IPT is offline. ChecklistBank keeps a copy
#         of the original archive:
#         https://api.checklistbank.org/dataset/281817/archive
# Notes:  faunabr::get_faunabr() only downloads with verbose = TRUE (LESSONS L-004).
#         faunabr is pinned to commit 66b3466 (v1.1.0). The older e3d4331 pin
#         breaks on the current data (locationID vs locality) — see LESSONS L-005.

suppressWarnings(suppressMessages(library(faunabr)))

# The pinned faunabr selects columns with df_no_species[, colnames(data)] on a
# data.table. That returns a character vector, and the next rbind fails.
# Catalog 1.54 is the first version that reaches that line: a valid subspecies
# with no species row (Belotus maculatus platonensis). faunabr fixed the line
# in commit 1d0c16d. This patch applies the same fix, and only while the line
# is there, so it stops when the pin moves past that commit.
local({
    fd <- faunabr:::fauna_discrepancies
    src <- deparse(fd, width.cutoff = 500L)
    bad <- "df_no_species[, colnames(data)]"
    if (any(grepl(bad, src, fixed = TRUE))) {
        good <- "df_no_species[, colnames(data), with = FALSE]"
        fixed <- eval(parse(text = sub(bad, good, src, fixed = TRUE)))
        environment(fixed) <- environment(fd)
        utils::assignInNamespace("fauna_discrepancies", fixed, ns = "faunabr")
    }
})

out_rds  <- file.path("inst", "extdata", "faunabr_observabio.rds")
out_meta <- sub("\\.rds$", ".meta.json", out_rds)
tmp_dir  <- file.path("data-raw", "tmp", "faunabr")
dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
source_note <- "Catálogo Taxonômico da Fauna do Brasil (faunabr::get_faunabr)"
if (length(args) >= 2L) {
    # With verbose = FALSE, get_faunabr() does not download (L-004). It unzips
    # and merges the archive that is already in output_dir.
    message("Building Fauna do Brasil ", args[[2L]], " from ", args[[1L]], "...")
    file.copy(args[[1L]], file.path(tmp_dir, paste0(args[[2L]], ".zip")), overwrite = TRUE)
    faunabr::get_faunabr(
        output_dir   = tmp_dir,
        data_version = args[[2L]],
        overwrite    = TRUE,
        verbose      = FALSE
    )
    source_note <- paste0(source_note, ", local archive ", basename(args[[1L]]))
} else {
    message("Downloading Fauna do Brasil (this can take a few minutes)...")
    faunabr::get_faunabr(
        output_dir   = tmp_dir,
        data_version = "latest",
        overwrite    = TRUE,
        verbose      = TRUE
    )
}

versions <- list.dirs(tmp_dir, full.names = FALSE, recursive = FALSE)
versions <- versions[nzchar(versions)]
load_version <- if (length(versions) >= 1L) versions[[length(versions)]] else "latest"

message("Loading (type = 'short')...")
data <- faunabr::load_faunabr(
    data_dir     = tmp_dir,
    data_version = load_version,
    type         = "short",
    verbose      = FALSE
)

saveRDS(data, out_rds, compress = "xz")

meta <- list(
    provider = "faunabr",
    version  = as.character(load_version),
    date     = as.character(Sys.Date()),
    source   = source_note,
    nrow     = nrow(data),
    ncol     = ncol(data),
    columns  = names(data)
)
jsonlite::write_json(meta, out_meta, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("Wrote %s (%d rows, %d cols) and %s",
                out_rds, nrow(data), ncol(data), out_meta))
