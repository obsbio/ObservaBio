# Title: Deploy Entrypoint
# The file the hosting platform runs (shinyapps.io / Posit Connect Cloud). The
# bundle carries the package *source*, not an installed package: `ObservaBio` is not
# on CRAN and the repo is private, so the server cannot install it. `load_all()`
# loads R/ straight from the bundle instead — hence `pkgload` is an Imports
# dependency, not Suggests (see docs/DECISIONS.md).
#
# `attach_testthat = FALSE` matters: the default attaches testthat, which is a
# Suggests package and therefore absent from renv.lock and the manifest — the
# app would fail to boot in production. `helpers = FALSE` skips test helpers for
# the same reason. `export_all = FALSE` keeps the namespace honest (only what
# NAMESPACE exports), matching an installed package.
#
# R/_disable_autoload.R stops Shiny from raw-sourcing R/ before this runs, so
# load_all() below is the ONLY thing that loads the package (with .onLoad, hence
# the provider registry). Read that file's note before touching this setup.
#
# Regenerate renv.lock + manifest.json with `Rscript data-raw/build_deploy.R`
# before publishing. Runbook: docs/deploy.md.

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

run_app()
