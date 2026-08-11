# data-raw/prep_florabr.R
# Regenerate the embedded Flora do Brasil base. Run LOCALLY (never at runtime).
# Downloads the official Flora e Funga do Brasil dataset, keeps the "short"
# column set, and writes inst/extdata/florabr_validabio.rds + a version sidecar.
#
# Usage:  Rscript data-raw/prep_florabr.R
# Note:   florabr::get_florabr() only downloads with verbose = TRUE (LESSONS L-004).

suppressWarnings(suppressMessages(library(florabr)))

out_rds  <- file.path("inst", "extdata", "florabr_validabio.rds")
out_meta <- sub("\\.rds$", ".meta.json", out_rds)
tmp_dir  <- file.path("data-raw", "tmp", "florabr")
dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

message("Downloading Flora do Brasil (this can take a few minutes)...")
florabr::get_florabr(
    output_dir   = tmp_dir,
    data_version = "latest",
    overwrite    = TRUE,
    verbose      = TRUE
)

# Detect the version directory that get_florabr created.
versions <- list.dirs(tmp_dir, full.names = FALSE, recursive = FALSE)
versions <- versions[nzchar(versions)]
load_version <- if (length(versions) >= 1L) versions[[length(versions)]] else "latest"

message("Loading (type = 'short')...")
data <- florabr::load_florabr(
    data_dir     = tmp_dir,
    data_version = load_version,
    type         = "short",
    verbose      = FALSE
)

saveRDS(data, out_rds, compress = "xz")

meta <- list(
    provider = "florabr",
    version  = as.character(load_version),
    date     = as.character(Sys.Date()),
    source   = "Flora e Funga do Brasil (florabr::get_florabr)",
    nrow     = nrow(data),
    ncol     = ncol(data),
    columns  = names(data)
)
jsonlite::write_json(meta, out_meta, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("Wrote %s (%d rows, %d cols) and %s",
                out_rds, nrow(data), ncol(data), out_meta))
