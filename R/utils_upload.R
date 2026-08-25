# Title: Upload Helpers
# Structural validation of the uploaded study-area files. The geometry read +
# 10 km buffer live in the geo weeks (SPEC §8). The upload step only resolves
# each file to the path that `sf::st_read` must open, and confirms the format is
# complete. One file is one study area: `unpack_area_files()` handles the whole
# (possibly multi-file) upload and names each area after its file.
#
# Three formats, one field: a shapefile `.zip`, a `.kmz`, or a bare `.kml`. The
# `.kmz` is a zip around a `.kml`, and this code unpacks it instead of handing it
# to GDAL: the built-in KML driver cannot open a `.kmz`, and the LIBKML driver
# that can is not guaranteed on the deploy platform (LESSONS L-016).

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

#' Unpack a `.kmz` and find the `.kml` inside it
#'
#' A `.kmz` is a zip holding one `.kml` plus its assets (icons, overlays). The
#' Google Earth convention names the document `doc.kml`, so that name wins when
#' the archive carries several `.kml` files.
#'
#' @param kmz_path Path to the uploaded `.kmz`.
#' @param exdir Directory to extract into (a fresh temp dir by default).
#' @return List with `dir` (extraction dir), `kml` (path to the `.kml`) and
#'   `files` (all extracted paths).
#' @noRd
unpack_kmz <- function(kmz_path, exdir = tempfile("ObservaBio_kmz")) {
    if (!file.exists(kmz_path)) {
        stop(sprintf("KMZ file not found: %s", kmz_path))
    }
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

    files <- tryCatch(
        utils::unzip(kmz_path, exdir = exdir),
        error = function(e) stop(sprintf("Failed to unzip KMZ: %s", conditionMessage(e)))
    )
    if (length(files) == 0L) {
        stop("KMZ file is empty or not a valid archive.")
    }

    kmls <- files[tolower(tools::file_ext(files)) == "kml"]
    if (length(kmls) == 0L) {
        stop("KMZ file has no .kml inside it.")
    }
    doc <- kmls[tolower(basename(kmls)) == "doc.kml"]
    kml <- if (length(doc) > 0L) doc[[1L]] else kmls[[1L]]

    list(dir = exdir, kml = kml, files = files)
}

#' Resolve a set of uploaded files into study areas — one file per area
#'
#' Each file is resolved to the path `sf::st_read` must open and named after the
#' file (minus the extension), which is the label the user links to `locality` in
#' Step 1. A failing file does not sink the others: it comes back in the `errors`
#' slot so the module can name it in a notification.
#'
#' @param paths Character vector of uploaded file paths (`datapath`).
#' @param names Character vector of the original file names, same length as
#'   `paths`. Defaults to the basenames of `paths` (which are temp names, so the
#'   module should always pass the real ones).
#' @return List with `areas` (list of `name`/`dir`/`source`, in upload order) and
#'   `errors` (named character: original file name -> message).
#' @noRd
unpack_area_files <- function(paths, names = basename(paths)) {
    paths <- as.character(paths)
    names <- as.character(names)
    if (length(paths) == 0L) {
        return(list(areas = list(), errors = character(0)))
    }
    if (length(names) != length(paths)) {
        stop("unpack_area_files(): `names` must be the same length as `paths`.")
    }

    areas <- list()
    errors <- character(0)
    for (i in seq_along(paths)) {
        label <- sub("\\.(zip|kmz|kml)$", "", names[[i]], ignore.case = TRUE)
        # The upload name carries the format, not the temp `datapath` Shiny hands
        # us — that one has no extension at all.
        ext <- tolower(tools::file_ext(names[[i]]))
        parsed <- tryCatch(
            switch(
                ext,
                zip = {
                    one <- unzip_shapefile(paths[[i]])
                    list(dir = one$dir, source = one$shp)
                },
                kmz = {
                    one <- unpack_kmz(paths[[i]])
                    list(dir = one$dir, source = one$kml)
                },
                kml = list(dir = dirname(paths[[i]]), source = paths[[i]]),
                stop(sprintf(
                    "Unsupported area format %s. Send a shapefile .zip, a .kmz, or a .kml.",
                    if (nzchar(ext)) sprintf("'.%s'", ext) else "(no extension)"
                ))
            ),
            error = function(e) e
        )
        if (inherits(parsed, "error")) {
            errors[[names[[i]]]] <- conditionMessage(parsed)
            next
        }
        areas[[length(areas) + 1L]] <- list(
            name = label, dir = parsed$dir, source = parsed$source
        )
    }
    list(areas = areas, errors = errors)
}
