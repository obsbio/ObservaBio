# unzip_shapefile unpacks a shapefile .zip and enforces that the four mandatory
# components are present. unpack_kmz unpacks a .kmz and finds the .kml inside.
# unpack_area_files routes the whole upload by extension. Geometry reading is
# deferred to the geo weeks.

make_shapefile_zip <- function(exts) {
    skip_if_not_installed("zip")
    src <- tempfile("shpsrc")
    dir.create(src)
    comps <- file.path(src, paste0("area.", exts))
    for (f in comps) writeLines("x", f)
    zip_path <- tempfile(fileext = ".zip")
    zip::zipr(zip_path, comps)
    zip_path
}

test_that("unzip_shapefile accepts a complete shapefile archive", {
    zip_path <- make_shapefile_zip(c("shp", "shx", "dbf", "prj"))
    on.exit(unlink(zip_path), add = TRUE)

    res <- unzip_shapefile(zip_path)
    expect_true(file.exists(res$shp))
    expect_equal(tolower(tools::file_ext(res$shp)), "shp")
    expect_length(res$files, 4L)
})

test_that("unzip_shapefile rejects an archive missing the .prj", {
    zip_path <- make_shapefile_zip(c("shp", "shx", "dbf"))
    on.exit(unlink(zip_path), add = TRUE)

    expect_error(unzip_shapefile(zip_path), "missing required component")
})

test_that("unzip_shapefile errors on a missing file", {
    expect_error(unzip_shapefile(tempfile(fileext = ".zip")), "not found")
})

# ---- unpack_area_files (one file = one study area) --------------------------

test_that("unpack_area_files names each area after its file", {
    zips <- c(make_shapefile_zip(c("shp", "shx", "dbf", "prj")),
              make_shapefile_zip(c("shp", "shx", "dbf", "prj")))
    on.exit(unlink(zips), add = TRUE)

    res <- unpack_area_files(zips, c("RPPN Rio do Brasil.zip", "Fazenda Trijuncao.ZIP"))
    expect_length(res$areas, 2L)
    expect_length(res$errors, 0L)
    # The extension is stripped case-insensitively; the rest is left verbatim.
    expect_equal(vapply(res$areas, function(a) a$name, character(1)),
                 c("RPPN Rio do Brasil", "Fazenda Trijuncao"))
    expect_true(all(vapply(res$areas, function(a) file.exists(a$source), logical(1))))
    # Each area unpacks into its own directory.
    expect_equal(length(unique(vapply(res$areas, function(a) a$dir, character(1)))), 2L)
})

test_that("unpack_area_files reports a bad file by name without sinking the others", {
    zips <- c(make_shapefile_zip(c("shp", "shx", "dbf", "prj")),
              make_shapefile_zip(c("shp", "shx", "dbf")))
    on.exit(unlink(zips), add = TRUE)

    res <- unpack_area_files(zips, c("boa.zip", "sem_prj.zip"))
    expect_length(res$areas, 1L)
    expect_equal(res$areas[[1L]]$name, "boa")
    expect_named(res$errors, "sem_prj.zip")
    expect_match(res$errors[["sem_prj.zip"]], "missing required component")
})

test_that("unpack_area_files on no upload returns an empty result", {
    res <- unpack_area_files(character(0))
    expect_length(res$areas, 0L)
    expect_length(res$errors, 0L)
})

# ---- unpack_kmz (a .kmz is a zip around a .kml) -----------------------------

# A minimal KML document. The upload step never reads the geometry, so the
# content only has to be a file named `.kml`.
make_kmz <- function(entries = c("doc.kml")) {
    skip_if_not_installed("zip")
    src <- tempfile("kmzsrc")
    dir.create(src)
    comps <- file.path(src, entries)
    for (f in comps) writeLines("<kml/>", f)
    kmz_path <- tempfile(fileext = ".kmz")
    zip::zipr(kmz_path, comps)
    kmz_path
}

test_that("unpack_kmz finds the .kml inside the archive", {
    kmz_path <- make_kmz()
    on.exit(unlink(kmz_path), add = TRUE)

    res <- unpack_kmz(kmz_path)
    expect_true(file.exists(res$kml))
    expect_equal(tolower(tools::file_ext(res$kml)), "kml")
})

test_that("unpack_kmz prefers doc.kml over the other .kml files", {
    # Google Earth names the document doc.kml; the rest are linked overlays.
    kmz_path <- make_kmz(c("overlay.kml", "doc.kml"))
    on.exit(unlink(kmz_path), add = TRUE)

    expect_equal(basename(unpack_kmz(kmz_path)$kml), "doc.kml")
})

test_that("unpack_kmz rejects an archive with no .kml", {
    kmz_path <- make_kmz("icon.png")
    on.exit(unlink(kmz_path), add = TRUE)

    expect_error(unpack_kmz(kmz_path), "no .kml inside")
})

test_that("unpack_kmz errors on a missing file", {
    expect_error(unpack_kmz(tempfile(fileext = ".kmz")), "not found")
})

# ---- unpack_area_files: the three formats in one upload ---------------------

test_that("unpack_area_files accepts .zip, .kmz and .kml in one upload", {
    zip_path <- make_shapefile_zip(c("shp", "shx", "dbf", "prj"))
    kmz_path <- make_kmz()
    kml_path <- tempfile(fileext = ".kml")
    writeLines("<kml/>", kml_path)
    on.exit(unlink(c(zip_path, kmz_path, kml_path)), add = TRUE)

    res <- unpack_area_files(
        c(zip_path, kmz_path, kml_path),
        c("Fazenda.zip", "Reserva.KMZ", "Talhao.kml")
    )
    expect_length(res$errors, 0L)
    expect_equal(vapply(res$areas, function(a) a$name, character(1)),
                 c("Fazenda", "Reserva", "Talhao"))
    expect_equal(vapply(res$areas, function(a) tolower(tools::file_ext(a$source)),
                        character(1)),
                 c("shp", "kml", "kml"))
    expect_true(all(vapply(res$areas, function(a) file.exists(a$source), logical(1))))
})

test_that("unpack_area_files reports an unsupported extension by name", {
    path <- tempfile(fileext = ".geojson")
    writeLines("{}", path)
    on.exit(unlink(path), add = TRUE)

    res <- unpack_area_files(path, "area.geojson")
    expect_length(res$areas, 0L)
    expect_named(res$errors, "area.geojson")
    expect_match(res$errors[["area.geojson"]], "Unsupported area format")
})
