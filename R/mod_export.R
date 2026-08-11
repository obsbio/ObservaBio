# Title: Export Module (Step 4 — Exportar)
# Reactive bridge: serves the two ZHOUSE Excel deliverables via download cards and
# shows the processing timeline. Wraps the pure writers in utils_export.R
# (write_standardized_xlsx / write_audit_xlsx). Audit geo columns are added with
# the geo runtime (later increment). UI text is PT-BR (SPEC §2.1).

#' Export module UI (Step 4)
#'
#' @param id Module id.
#' @return A `shiny::tagList`.
#' @noRd
mod_export_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$div(
            class = "step-panel",
            step_header(4, "Exportar", "Exportar"),
            shiny::tags$p(
                class = "step-lede",
                "Baixe a planilha padronizada em Darwin Core e o relatório de ",
                "auditoria com as decisões da validação."
            ),
            shiny::tags$div(
                class = "export-grid",
                shiny::tags$div(
                    class = "export-card",
                    shiny::tags$span(class = "upload-card__badge upload-card__badge--tax",
                                     shiny::icon("table")),
                    shiny::tags$span(class = "export-card__title", "Dados Darwin Core"),
                    shiny::tags$span(class = "export-card__desc",
                                     "Planilha padronizada, uma linha por registro, pronta para envio."),
                    shiny::downloadButton(ns("dl_std"), "Baixar (.xlsx)",
                                          class = "btn-primary")
                ),
                shiny::tags$div(
                    class = "export-card",
                    shiny::tags$span(class = "upload-card__badge upload-card__badge--geo",
                                     shiny::icon("clipboard-list")),
                    shiny::tags$span(class = "export-card__title", "Relatório de auditoria"),
                    shiny::tags$span(class = "export-card__desc",
                                     "Decisão por nome (validador, match, taxonomia) e não resolvidos."),
                    shiny::downloadButton(ns("dl_audit"), "Baixar (.xlsx)",
                                          class = "btn-outline-secondary")
                )
            ),
            shiny::uiOutput(ns("timeline"))
        )
    )
}

#' Export module server (Step 4)
#'
#' @param id Module id.
#' @param result_r Reactive from `mod_process_server()` (list or NULL).
#' @return Invisibly NULL.
#' @noRd
mod_export_server <- function(id, result_r) {
    shiny::moduleServer(id, function(input, output, session) {

        output$timeline <- shiny::renderUI({
            res <- result_r()
            shiny::req(res)
            timings <- res$timings
            if (is.null(timings) || length(timings) == 0L) {
                return(NULL)
            }
            rows <- lapply(names(timings), function(phase) {
                shiny::tags$li(
                    shiny::tags$span(
                        class = "phase",
                        shiny::tags$span(class = "check", shiny::icon("check")),
                        phase
                    ),
                    shiny::tags$span(class = "dur", sprintf("%.1f s", timings[[phase]]))
                )
            })
            shiny::tags$div(
                class = "zh-card",
                style = "margin-top: var(--space-8); max-width: 520px;",
                shiny::tags$div(
                    class = "zh-card__body",
                    shiny::tags$span(class = "eyebrow", "Processamento"),
                    shiny::tags$ul(class = "timeline", rows)
                )
            )
        })

        output$dl_std <- shiny::downloadHandler(
            filename = function() "zhouse_darwincore_padronizada.xlsx",
            content = function(file) {
                res <- result_r()
                shiny::req(res)
                write_standardized_xlsx(
                    res$dwc, file,
                    model_cols = names(res$dwc), pt_labels = res$pt_labels
                )
            }
        )

        output$dl_audit <- shiny::downloadHandler(
            filename = function() "zhouse_darwincore_auditoria.xlsx",
            content = function(file) {
                res <- result_r()
                shiny::req(res)
                write_audit_xlsx(res$audit, file, unresolved = res$unresolved)
            }
        )

        invisible(NULL)
    })
}
