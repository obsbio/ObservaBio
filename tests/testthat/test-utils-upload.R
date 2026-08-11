# unzip_shapefile unpacks a shapefile .zip and enforces that the four mandatory
# components are present. Geometry reading is deferred to the geo weeks.

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

# ---- unzip_shapefiles (one .zip = one study area) ---------------------------

test_that("unzip_shapefiles names each area after its archive", {
    zips <- c(make_shapefile_zip(c("shp", "shx", "dbf", "prj")),
              make_shapefile_zip(c("shp", "shx", "dbf", "prj")))
    on.exit(unlink(zips), add = TRUE)

    res <- unzip_shapefiles(zips, c("RPPN Rio do Brasil.zip", "Fazenda Trijuncao.ZIP"))
    expect_length(res$areas, 2L)
    expect_length(res$errors, 0L)
    # The extension is stripped case-insensitively; the rest is left verbatim.
    expect_equal(vapply(res$areas, function(a) a$name, character(1)),
                 c("RPPN Rio do Brasil", "Fazenda Trijuncao"))
    expect_true(all(vapply(res$areas, function(a) file.exists(a$shp), logical(1))))
    # Each area unpacks into its own directory.
    expect_equal(length(unique(vapply(res$areas, function(a) a$dir, character(1)))), 2L)
})

test_that("unzip_shapefiles reports a bad archive by name without sinking the others", {
    zips <- c(make_shapefile_zip(c("shp", "shx", "dbf", "prj")),
              make_shapefile_zip(c("shp", "shx", "dbf")))
    on.exit(unlink(zips), add = TRUE)

    res <- unzip_shapefiles(zips, c("boa.zip", "sem_prj.zip"))
    expect_length(res$areas, 1L)
    expect_equal(res$areas[[1L]]$name, "boa")
    expect_named(res$errors, "sem_prj.zip")
    expect_match(res$errors[["sem_prj.zip"]], "missing required component")
})

test_that("unzip_shapefiles on no upload returns an empty result", {
    res <- unzip_shapefiles(character(0))
    expect_length(res$areas, 0L)
    expect_length(res$errors, 0L)
})
