# Title: Process Module (Step 2 — Processar)
# Reactive bridge: on "Iniciar processamento" it runs the pure pipeline
# (run_cascade → resolve_threat_status → run_geo_verification →
# build_dwc_output / build_audit_table) behind a phased progress bar, capturing
# per-phase timings, and exposes the result reactive the app consumes on Steps 3
# (Resultado) and 4 (Exportar). The geographic phase fills distributionFlag and
# feeds the map; it degrades to NULL (no shapefile / network error) without
# aborting the run. UI text is PT-BR (SPEC §2.1).

#' Process module UI (Step 2)
#'
#' @param id Module id.
#' @return A `shiny::tagList`.
#' @noRd
mod_process_ui <- function(id) {
    ns <- shiny::NS(id)
    phase <- function(n, text) {
        shiny::tags$li(shiny::tags$span(class = "ico", n), text)
    }
    shiny::tagList(
        shiny::tags$div(
            class = "step-panel",
            step_header(2, "Processar", "Processar"),
            shiny::tags$p(
                class = "step-lede",
                "Padronização taxonômica (Flora e Fauna do Brasil + GBIF ao vivo), ",
                "cruzamento de status de conservação (MMA + IUCN) e montagem do ",
                "Darwin Core. Isso pode levar alguns segundos por espécie nova."
            ),
            shiny::tags$div(
                class = "zh-card process-card",
                shiny::tags$div(
                    class = "zh-card__body",
                    shiny::tags$span(class = "process-card__icon", shiny::icon("play")),
                    shiny::uiOutput(ns("ready")),
                    shiny::tags$ul(
                        class = "phases",
                        phase("①", "Validação taxonômica em cascata BR → GBIF"),
                        phase("②", "Status de conservação MMA + IUCN"),
                        phase("③", "Verificação geográfica (buffer 10 km)"),
                        phase("④", "Montagem Darwin Core")
                    ),
                    shiny::actionButton(
                        ns("processar"), "Iniciar processamento",
                        class = "btn-primary btn-lg", icon = shiny::icon("play")
                    ),
                    shiny::uiOutput(ns("status"))
                )
            )
        )
    )
}

#' Process module server (Step 2)
#'
#' @param id Module id.
#' @param upload_r Reactive from `mod_upload_server()$data` (list or NULL).
#' @param iucn_key_r Reactive from `mod_upload_server()$iucn_key`: the optional
#'   session-only IUCN key (ADR-005). Empty/NULL keeps the keyless GBIF route.
#' @return A reactive returning the result list (NULL until processed):
#'   `dwc`, `audit`, `unresolved`, `cascade`, `geo`, `record_areas` (the study
#'   area per record), `unlinked` (localities with no area), `summary`,
#'   `timings`, `model_cols`, `pt_labels`.
#' @noRd
mod_process_server <- function(id, upload_r, iucn_key_r = shiny::reactive(NULL)) {
    shiny::moduleServer(id, function(input, output, session) {
        result_rv <- shiny::reactiveVal(NULL)

        output$ready <- shiny::renderUI({
            upload <- upload_r()
            if (is.null(upload) || is.null(upload$records)) {
                return(shiny::tags$div(
                    shiny::tags$div(class = "process-card__title", "Nenhuma planilha carregada"),
                    shiny::tags$div(class = "process-card__meta",
                                    "Volte ao passo 01 e envie a planilha de espécies.")
                ))
            }
            # The queue is the DISTINCT names the cascade will work on: rows that
            # arrive already validated are preserved verbatim and skipped, and a
            # name repeated across rows is validated once.
            n_new <- length(unique(dwc_unvalidated_names(upload$records)))
            num <- function(x) {
                shiny::tags$span(class = "num",
                                 format(x, big.mark = ".", decimal.mark = ","))
            }
            shiny::tags$div(
                shiny::tags$div(class = "process-card__title", "Pronto para processar"),
                shiny::tags$div(
                    class = "process-card__meta",
                    num(nrow(upload$records)), " registros · ",
                    num(n_new), " espécies novas na fila"
                )
            )
        })

        shiny::observeEvent(input$processar, {
            upload <- upload_r()
            if (is.null(upload) || is.null(upload$records) || nrow(upload$records) == 0L) {
                shiny::showNotification(
                    "Envie uma planilha válida antes de processar.",
                    type = "warning", duration = 5
                )
                return(invisible(NULL))
            }

            records <- upload$records
            # Only names on not-yet-validated rows go to the cascade + IUCN.
            # Already-validated rows are preserved verbatim by build_dwc_output()
            # and carry their status in the input model, so skipping them avoids
            # the per-species IUCN HTTP cost on large lists.
            new_names <- dwc_unvalidated_names(records)
            timings <- c()
            t0 <- Sys.time()
            tick <- function(label) {
                now <- Sys.time()
                timings[[label]] <<- as.numeric(difftime(now, t0, units = "secs"))
                t0 <<- now
            }

            # An error escaping this observer would end the Shiny session — the
            # grey "Disconnected from the server" screen, with no way back except
            # a reload. Degrade to a notification instead, like the geographic
            # phase already does. (This cannot catch an out-of-memory kill: that
            # is the process dying, not an R condition. See LESSONS L-017.)
            result <- tryCatch(shiny::withProgress(message = "Processando", value = 0, {
                shiny::incProgress(0.15, detail = "Validação taxonômica…")
                cascade <- run_cascade(new_names)
                # The taxonomic phase is the biggest transient allocation of the
                # run; hand it back before the network phases start.
                gc(verbose = FALSE)
                tick("Padronização taxonômica")

                shiny::incProgress(0.35, detail = "Status de conservação (MMA + IUCN)…")
                # The key (when the user pasted one in Step 1) is read straight
                # from the reactive and handed to the pure function — never
                # stored, logged, or carried into the result.
                cascade <- resolve_threat_status(cascade, iucn_key = iucn_key_r())
                # The invasive cross-check is offline and runs on EVERY species
                # (new + already validated), so it rides along with the status
                # phase instead of paying for a phase of its own.
                invasive_sources <- resolve_invasive_flags(records, cascade)
                tick("Status de conservação")

                shiny::incProgress(0.3, detail = "Verificação geográfica…")
                # Each uploaded area is verified against only the species its own
                # records claim (the `locality` link). Runs on EVERY species with
                # an accepted name (new + already validated), not only the
                # cascade's — the "is this recorded near the operation area?"
                # check is relevant for the whole list. Degrades to NULL (no
                # distributionFlag, no map) on a missing shapefile or a
                # network/geometry error rather than aborting.
                record_areas <- assign_record_areas(records, upload$areas %||% list())
                geo <- tryCatch(
                    run_geo_verification(
                        upload$areas,
                        geo_name_map_by_area(records, cascade, record_areas)
                    ),
                    error = function(e) {
                        shiny::showNotification(
                            paste("Verificação geográfica indisponível:", conditionMessage(e)),
                            type = "warning", duration = 6
                        )
                        NULL
                    }
                )
                tick("Verificação geográfica")

                shiny::incProgress(0.15, detail = "Padronização Darwin Core…")
                dwc <- build_dwc_output(
                    records, cascade, model_cols = upload$model_cols,
                    distribution_flags = if (!is.null(geo)) geo$distribution_flags else NULL,
                    invasive_sources = invasive_sources,
                    record_areas = record_areas
                )
                audit <- build_audit_table(cascade, original_names = new_names, geo = geo,
                                           invasive_sources = invasive_sources)
                unresolved <- audit_unresolved(audit)
                tick("Darwin Core")

                shiny::incProgress(0.05, detail = "Concluído")
                list(
                    dwc = dwc, audit = audit, unresolved = unresolved,
                    cascade = cascade, geo = geo, record_areas = record_areas,
                    unlinked = unlinked_localities(records, record_areas),
                    summary = process_summary(dwc, cascade, geo), timings = timings,
                    model_cols = upload$model_cols, pt_labels = upload$pt_labels
                )
            }), error = function(e) {
                shiny::showNotification(
                    paste("O processamento falhou:", conditionMessage(e)),
                    type = "error", duration = NULL
                )
                NULL
            })

            if (is.null(result)) {
                return(invisible(NULL))
            }

            result_rv(result)
            shiny::showNotification(
                sprintf("Processamento concluído: %d registros.", nrow(result$dwc)),
                type = "message", duration = 4
            )
            # Records the user did not link to any area are NOT geo-verified, so
            # say which localities were left out rather than letting the empty
            # distributionFlag look like a bug.
            if (length(result$unlinked) > 0L) {
                shiny::showNotification(
                    sprintf(
                        "%d localidade(s) sem área vinculada (%s) — esses registros ficaram sem verificação geográfica.",
                        length(result$unlinked), paste(result$unlinked, collapse = ", ")
                    ),
                    type = "warning", duration = 10
                )
            }
        })

        output$status <- shiny::renderUI({
            res <- result_rv()
            shiny::req(res)
            shiny::tags$div(
                class = "process-status",
                shiny::tags$span(
                    class = "flag-confirmed badge-pill",
                    shiny::icon("check"),
                    sprintf("%d registros processados — abrindo resultados…",
                            nrow(res$dwc))
                )
            )
        })

        result_rv
    })
}
