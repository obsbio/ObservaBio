# Title: Application UI (shell)
# 4-step guided workspace: Enviar → Processar → Resultado → Exportar. The app
# fills the viewport (ADR-019): a fixed dark rail on the left carries the brand,
# the step trail, the "Como usar" trigger and the partner logos; the right side
# takes all the remaining width, is the only scrolling surface, and mounts the
# step modules through a hidden tabset. app_server owns the step state and drives
# navigation. UI text is PT-BR (SPEC §2.1); business logic stays in the pure
# utils_*/provider_* functions.

#' Version a local www asset by file mtime, so the browser refetches it after
#' every CSS/JS rebuild (and every deploy) instead of serving a stale copy under
#' the fixed filename. Mirrors run_app()'s www resolution.
#' @noRd
www_asset <- function(rel) {
    www <- system.file("app/www", package = "ObservaBio")
    if (www == "") www <- "inst/app/www"
    f <- file.path(www, rel)
    href <- paste0("www/", rel)
    if (file.exists(f)) paste0(href, "?v=", as.integer(file.mtime(f))) else href
}

#' Head tags — theme fonts, the built CSS bundle and the drop-zone script
#' (served at www/ by run_app()).
#' @noRd
app_head <- function() {
    shiny::tags$head(
        shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        shiny::tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
                         crossorigin = NA),
        shiny::tags$link(
            rel = "stylesheet",
            href = paste0(
                "https://fonts.googleapis.com/css2?",
                "family=Noto+Serif:ital,wght@0,400;0,600;1,400",
                "&family=Inter:wght@400;500;600",
                "&family=IBM+Plex+Mono:wght@400;500&display=swap"
            )
        ),
        shiny::tags$link(rel = "stylesheet", href = www_asset("custom.css")),
        shiny::tags$script(src = www_asset("js/dropzone.js")),
        shiny::tags$script(src = www_asset("js/processing-overlay.js")),
        shiny::tags$meta(name = "viewport",
                         content = "width=device-width, initial-scale=1")
    )
}

#' The left rail: brand, the step trail, the help trigger and the client logos.
#'
#' It is the app's only persistent chrome, so everything global lives here — the
#' help walkthrough is reachable from every step of the wizard.
#' @noRd
app_rail <- function() {
    logo <- function(src, alt) {
        shiny::tags$img(class = "app-rail__logo", src = src, alt = alt)
    }
    shiny::tags$aside(
        class = "app-rail",
        shiny::tags$div(
            class = "app-rail__brand",
            shiny::tags$img(
                class = "app-rail__brandmark",
                src = "www/img/observatorio-claro.png",
                alt = "Observatório Biodiversidade"
            )
        ),
        shiny::tags$div(class = "app-rail__eyebrow", "Fluxo de validação"),
        mod_stepper_ui("stepper"),
        shiny::tags$div(
            class = "app-rail__foot",
            mod_help_ui("help"),
            shiny::tags$div(
                class = "app-rail__logos",
                logo("www/img/humanize.png", "iHumanize")
            )
        )
    )
}

#' Step title block — eyebrow ("Passo 03 — Resultado") + serif heading, with an
#' optional note pinned to the right of the heading.
#'
#' Shared by the four step modules so the screens open the same way.
#' @noRd
step_header <- function(n, step, title, note = NULL) {
    shiny::tags$div(
        class = "step-head",
        shiny::tags$div(
            shiny::tags$div(
                class = "step-eyebrow",
                sprintf("Passo %02d — %s", as.integer(n), step)
            ),
            shiny::tags$h2(class = "step-heading", title)
        ),
        note
    )
}

#' Build the ObservaBio Shiny UI
#'
#' @return A `bslib::page` UI object.
#' @noRd
app_ui <- function() {
    bslib::page(
        title = "ObservaBio — Padronização Darwin Core",
        theme = bslib::bs_theme(
            version = 5,
            bg = "#F7F3E9", fg = "#222826",
            primary = "#1E4620", secondary = "#C86446",
            success = "#277148", info = "#2B7DA3",
            warning = "#E5A93C", danger = "#B23A2E"
        ),
        app_head(),
        shiny::tags$div(
            class = "app-shell",
            app_rail(),
            shiny::tags$main(
                class = "app-main ob-topo",
                shiny::tabsetPanel(
                    id = "wizard",
                    type = "hidden",
                    shiny::tabPanelBody("step1", mod_upload_ui("upload")),
                    shiny::tabPanelBody("step2", mod_process_ui("process")),
                    shiny::tabPanelBody("step3", mod_results_ui("results")),
                    shiny::tabPanelBody("step4", mod_export_ui("export"))
                )
            )
        )
    )
}
