# Sentinel — do not remove. Deploy correctness depends on it.
#
# When a hosting platform (shinyapps.io / Posit Connect Cloud) runs the root
# app.R, Shiny's shinyAppDir() first calls loadSupport(), which RAW-SOURCES every
# file in R/ into a shared environment BEFORE app.R runs. That collides with the
# package: app.R already builds the real namespace via pkgload::load_all(), so R/
# would load twice — and the raw copy is broken, because it is not a namespace,
# so .onLoad() never fires and register_default_providers() (ObservaBio-package.R)
# never runs. run_app() would then resolve to the raw copy with an empty provider
# registry, and processing would fail while the UI still renders.
#
# The presence of a file matching ^_disable_autoload\.r$ makes loadSupport()
# return early without sourcing (see ?shiny::loadSupport). load_all() in app.R is
# then the only thing that loads the package, with the namespace and .onLoad
# intact. This file is intentionally empty apart from this note.
