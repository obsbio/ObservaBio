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

# An archive can expand to much more than its upload size. These limits apply
# to the whole archive and to the extracted members, and they apply while the
# data is written, so a false size in the archive directory cannot pass them
# (audit A06).
.AREA_MAX_ENTRIES <- 2000L
.AREA_MAX_UNPACKED_BYTES <- 200 * 1024^2

#' List the members of an area archive, with the entry limit
#'
#' @param path Archive path.
#' @param what Archive kind for messages ("Shapefile .zip", "KMZ file").
#' @return Character vector of member names (directories and macOS metadata
#'   removed).
#' @noRd
area_archive_members <- function(path, what) {
    inv <- tryCatch(
        utils::unzip(path, list = TRUE),
        error = function(e) stop(sprintf("Failed to unzip %s: %s", what, conditionMessage(e)))
    )
    if (!is.data.frame(inv) || nrow(inv) == 0L) {
        stop(sprintf("%s is empty or not a valid archive.", what))
    }
    if (nrow(inv) > .AREA_MAX_ENTRIES) {
        stop(sprintf("%s has %d entries. The limit is %d.", what, nrow(inv), .AREA_MAX_ENTRIES))
    }
    nm <- as.character(inv$Name)
    nm[!grepl("/$", nm) & !grepl("(^|/)__MACOSX/", nm) & !grepl("^\\._", basename(nm))]
}

#' Extract some members of an area archive, inside the size limit
#'
#' Reads each member as a stream and stops at the limit. Only the given
#' members are written, flat, in `exdir`. On an error, `exdir` is removed.
#'
#' @param path Archive path.
#' @param members Member names to extract.
#' @param exdir Destination directory.
#' @param what Archive kind for messages.
#' @return Character vector of the extracted paths.
#' @noRd
area_extract_members <- function(path, members, exdir, what) {
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    budget <- .AREA_MAX_UNPACKED_BYTES
    out <- file.path(exdir, basename(members))
    ok <- FALSE
    on.exit(if (!ok) unlink(exdir, recursive = TRUE), add = TRUE)
    for (i in seq_along(members)) {
        con <- unz(path, members[[i]], open = "rb")
        dest <- file(out[[i]], "wb")
        tryCatch({
            repeat {
                chunk <- readBin(con, "raw", n = 1048576L)
                if (length(chunk) == 0L) {
                    break
                }
                budget <- budget - length(chunk)
                if (budget < 0) {
                    stop(sprintf("%s expands to more than the limit of %d MiB.",
                                 what, .AREA_MAX_UNPACKED_BYTES %/% 1024^2))
                }
                writeBin(chunk, dest)
            }
        }, finally = {
            close(con)
            close(dest)
        })
    }
    ok <- TRUE
    out
}

#' Unpack and structurally validate a shapefile `.zip`
#'
#' Checks that the `.shp/.shx/.dbf/.prj` components of one shapefile exist with
#' the same basename (SPEC §8, step 1), and extracts only those (plus `.cpg`).
#' The other members stay in the archive. Does not read geometry — that is the
#' geo step.
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
    what <- "Shapefile .zip"
    members <- area_archive_members(zip_path, what)
    exts <- tolower(tools::file_ext(members))
    stems <- tolower(tools::file_path_sans_ext(members))
    required <- c("shp", "shx", "dbf", "prj")

    shps <- which(exts == "shp")
    if (length(shps) == 0L) {
        stop("Shapefile .zip missing required component(s): .shp")
    }
    # The first .shp with all its siblings wins. Without a complete one, the
    # message names what the first .shp lacks.
    pick <- NULL
    missing <- NULL
    for (k in shps) {
        set <- which(stems == stems[[k]])
        lack <- setdiff(required, exts[set])
        if (length(lack) == 0L) {
            pick <- set[exts[set] %in% c(required, "cpg")]
            break
        }
        if (is.null(missing)) missing <- lack
    }
    if (is.null(pick)) {
        stop(sprintf(
            "Shapefile .zip missing required component(s): %s",
            paste0(".", missing, collapse = ", ")
        ))
    }

    files <- area_extract_members(zip_path, members[pick], exdir, what)
    list(
        dir = exdir,
        shp = files[tolower(tools::file_ext(files)) == "shp"][[1L]],
        files = files
    )
}

#' Unpack a `.kmz` and find the `.kml` inside it
#'
#' A `.kmz` is a zip holding one `.kml` plus its assets (icons, overlays). The
#' Google Earth convention names the document `doc.kml`, so that name wins when
#' the archive carries several `.kml` files. Only that `.kml` is extracted: the
#' area needs no asset.
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
    what <- "KMZ file"
    members <- area_archive_members(kmz_path, what)
    kmls <- members[tolower(tools::file_ext(members)) == "kml"]
    if (length(kmls) == 0L) {
        stop("KMZ file has no .kml inside it.")
    }
    doc <- kmls[tolower(basename(kmls)) == "doc.kml"]
    kml <- if (length(doc) > 0L) doc[[1L]] else kmls[[1L]]

    files <- area_extract_members(kmz_path, kml, exdir, what)
    list(dir = exdir, kml = files[[1L]], files = files)
}

#' Temporary directories that an area change releases
#'
#' The app owns the extraction directory of a `.zip`/`.kmz` area (`owned`). A
#' bare `.kml` lives in the Shiny upload directory, and the app must not
#' remove it.
#'
#' @param old,new Area lists before and after the change.
#' @return Character vector of directories to remove.
#' @noRd
area_released_dirs <- function(old, new) {
    owned_dirs <- function(areas) {
        d <- vapply(areas, function(a) {
            if (isTRUE(a$owned) && length(a$dir) == 1L) as.character(a$dir) else NA_character_
        }, character(1))
        unique(d[!is.na(d)])
    }
    setdiff(owned_dirs(old), owned_dirs(new))
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
#' @return List with `areas` (list of `name`/`dir`/`source`/`owned`, in upload
#'   order) and `errors` (named character: original file name -> message).
#'   `owned` is TRUE when the app made `dir`, see [area_released_dirs()].
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
                    list(dir = one$dir, source = one$shp, owned = TRUE)
                },
                kmz = {
                    one <- unpack_kmz(paths[[i]])
                    list(dir = one$dir, source = one$kml, owned = TRUE)
                },
                kml = list(dir = dirname(paths[[i]]), source = paths[[i]], owned = FALSE),
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
            name = label, dir = parsed$dir, source = parsed$source,
            owned = parsed$owned
        )
    }
    list(areas = areas, errors = errors)
}
