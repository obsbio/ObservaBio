# Title: Upload Helpers
# Structural validation of the uploaded shapefile `.zip`. The geometry read +
# 10 km buffer live in the geo weeks (SPEC §8); the upload step only unpacks the
# archive and confirms the four mandatory shapefile components are present.
# One `.zip` is one study area: `unzip_shapefiles()` unpacks the whole (possibly
# multi-file) upload and names each area after its archive.

#' Unpack and structurally validate a shapefile `.zip`
#'
#' Extracts the archive and checks that the `.shp/.shx/.dbf/.prj` components all
#' exist (SPEC §8, step 1). Does not read geometry — that is the geo step.
#'
#' @param zip_path Path to the uploaded `.zip`.
#' @param exdir Directory to extract into (a fresh temp dir by default).
#' @return List with `dir` (extraction dir), `shp` (path to the `.shp`) and
#'   `files` (all extracted paths).
#' @noRd
unzip_shapefile <- function(zip_path, exdir = tempfile("ObservaBio_shp")) {
    if (!file.exists(zip_path)) {
        stop(sprintf("Shapefile .zip not found: %s", zip_path))
    }
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

    files <- tryCatch(
        utils::unzip(zip_path, exdir = exdir),
        error = function(e) stop(sprintf("Failed to unzip shapefile: %s", conditionMessage(e)))
    )
    if (length(files) == 0L) {
        stop("Shapefile .zip is empty or not a valid archive.")
    }

    exts <- tolower(tools::file_ext(files))
    required <- c("shp", "shx", "dbf", "prj")
    missing <- setdiff(required, exts)
    if (length(missing) > 0L) {
        stop(sprintf(
            "Shapefile .zip missing required component(s): %s",
            paste0(".", missing, collapse = ", ")
        ))
    }

    list(
        dir = exdir,
        shp = files[exts == "shp"][[1]],
        files = files
    )
}

#' Unpack a set of shapefile `.zip`s — one archive per study area
#'
#' Each `.zip` is unpacked by [unzip_shapefile()] into its own temp dir and named
#' after the archive (minus the extension), which is the label the user links to
#' `locality` in Step 1. A failing archive does not sink the others: it comes
#' back in the `errors` slot so the module can name it in a notification.
#'
#' @param paths Character vector of uploaded `.zip` paths (`datapath`).
#' @param names Character vector of the original file names, same length as
#'   `paths`. Defaults to the basenames of `paths` (which are temp names, so the
#'   module should always pass the real ones).
#' @return List with `areas` (list of `name`/`dir`/`shp`, in upload order) and
#'   `errors` (named character: original file name -> message).
#' @noRd
unzip_shapefiles <- function(paths, names = basename(paths)) {
    paths <- as.character(paths)
    names <- as.character(names)
    if (length(paths) == 0L) {
        return(list(areas = list(), errors = character(0)))
    }
    if (length(names) != length(paths)) {
        stop("unzip_shapefiles(): `names` must be the same length as `paths`.")
    }

    areas <- list()
    errors <- character(0)
    for (i in seq_along(paths)) {
        label <- sub("\\.zip$", "", names[[i]], ignore.case = TRUE)
        parsed <- tryCatch(unzip_shapefile(paths[[i]]), error = function(e) e)
        if (inherits(parsed, "error")) {
            errors[[names[[i]]]] <- conditionMessage(parsed)
            next
        }
        areas[[length(areas) + 1L]] <- list(
            name = label, dir = parsed$dir, shp = parsed$shp
        )
    }
    list(areas = areas, errors = errors)
}
