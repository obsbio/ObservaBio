# data-raw/prep_faunabr.R
# Regenerate the embedded Fauna do Brasil base. Run LOCALLY (never at runtime).
# Downloads the official Catálogo Taxonômico da Fauna do Brasil dataset, keeps
# the "short" column set, and writes inst/extdata/faunabr_observabio.rds + a sidecar.
#
# Usage:  Rscript data-raw/prep_faunabr.R
# Notes:  faunabr::get_faunabr() only downloads with verbose = TRUE (LESSONS L-004).
#         faunabr is pinned to commit 66b3466 (v1.1.0). The older e3d4331 pin
#         breaks on the current data (locationID vs locality) — see LESSONS L-005.

suppressWarnings(suppressMessages(library(faunabr)))

out_rds  <- file.path("inst", "extdata", "faunabr_observabio.rds")
out_meta <- sub("\\.rds$", ".meta.json", out_rds)
tmp_dir  <- file.path("data-raw", "tmp", "faunabr")
dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

message("Downloading Fauna do Brasil (this can take a few minutes)...")
faunabr::get_faunabr(
    output_dir   = tmp_dir,
    data_version = "latest",
    overwrite    = TRUE,
    verbose      = TRUE
)

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
    source   = "Catálogo Taxonômico da Fauna do Brasil (faunabr::get_faunabr)",
    nrow     = nrow(data),
    ncol     = ncol(data),
    columns  = names(data)
)
jsonlite::write_json(meta, out_meta, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("Wrote %s (%d rows, %d cols) and %s",
                out_rds, nrow(data), ncol(data), out_meta))
