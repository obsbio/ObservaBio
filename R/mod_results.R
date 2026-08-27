# Title: Results Module (Step 3 — Resultado)
# Reactive bridge for the read-only results workspace: summary tiles, the map
# (mod_map) + geo summary (mod_geo_verify) beside the curated results table, and
# a contextual species side panel driven by row selection. Read-only by design —
# the human reviews the exported files (SPEC §3). UI text is PT-BR (SPEC §2.1).
#
# Filtering (ADR-018) is one pure call — filter_results_view() in
# utils_results_ui.R — behind ONE filtered reactive that the table, the detail
# panel and the map markers all read from, so a filter can never leave them
# showing different species. The filter controls are checkbox groups (stable
# input ids, no dynamically created observers) painted as pills by 08-filters.css.

#' Results module UI (Step 3)
#'
#' @param id Module id.
#' @return A `shiny::tagList`.
#' @noRd
mod_results_ui <- function(id) {
    ns <- shiny::NS(id)
    # The standing caveat rides in the title block; the tooltip says what the
    # flag is actually based on.
    caveat <- shiny::tags$span(
        class = "badge-pill badge-info",
        title = paste0(
            "Alerta com base em ocorrências do GBIF a ≤ 10 km da área e na ",
            "distribuição conhecida (estado/bioma). Apenas sinaliza registros ",
            "para revisão."
        ),
        shiny::icon("circle-info"), "distributionFlag é alerta, não veredito"
    )
    shiny::tagList(
        shiny::tags$div(
            class = "step-panel",
            step_header(3, "Resultado", "Revisão dos registros", note = caveat),
            # One column: the stats, the map (hero, with the operation-area
            # context floating on it), the filters, the table and the detail
            # card. The filter bar sits between the map and the table because it
            # drives both.
            shiny::tags$div(
                class = "results-stack",
                shiny::uiOutput(ns("summary")),
                mod_map_ui(ns("map"), context = mod_geo_verify_ui(ns("geo"))),
                shiny::tags$div(
                    class = "filter-bar",
                    shiny::tags$span(class = "filter-bar__label", "Filtrar"),
                    shiny::uiOutput(ns("filters")),
                    shiny::tags$div(
                        class = "filter-bar__foot",
                        shiny::tags$span(
                            class = "filter-bar__count",
                            shiny::textOutput(ns("filter_count"), inline = TRUE)
                        ),
                        shiny::actionButton(
                            ns("flt_clear"), "Limpar filtros",
                            class = "filter-bar__clear",
                            icon = shiny::icon("filter-circle-xmark")
                        )
                    )
                ),
                shiny::tags$div(
                    class = "results-table",
                    DT::dataTableOutput(ns("table"))
                ),
                shiny::uiOutput(ns("detail"))
            ),
            shiny::tags$div(
                class = "upload-actions",
                shiny::actionButton(
                    ns("to_export"), "Ir para exportação",
                    class = "btn-primary btn-lg", icon = shiny::icon("arrow-right")
                )
            )
        )
    )
}

#' HTML for a distributionFlag badge (or an em dash when empty).
#' @noRd
flag_badge_html <- function(flag) {
    vapply(as.character(flag), function(f) {
        if (is.na(f) || !nzchar(f)) {
            return("<span class='flag-na'>—</span>")
        }
        sprintf("<span class='badge-pill %s'><span class='dot'></span>%s</span>",
                distribution_flag_class(f), htmltools::htmlEscape(f))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' HTML for the invasive-species badge (empty string when the taxon is not listed)
#'
#' The source list(s) go in the tooltip: the badge answers "is it invasive?", the
#' tooltip answers "says who?". Like every other signal here, an alert, not a
#' verdict.
#' @noRd
invasive_badge_html <- function(invasive, source = NA) {
    inv <- as.logical(invasive)
    src <- as.character(source)
    if (length(src) != length(inv)) {
        src <- rep(NA_character_, length(inv))
    }
    vapply(seq_along(inv), function(i) {
        if (!isTRUE(inv[[i]])) {
            return("")
        }
        title <- if (is.na(src[[i]])) {
            ""
        } else {
            sprintf(" title='%s'", htmltools::htmlEscape(src[[i]], attribute = TRUE))
        }
        sprintf(
            "<span class='badge-pill badge-invasive'%s><span class='dot'></span>exótica invasora</span>",
            title
        )
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' HTML for the taxonomic-status cell (matchType + validator, distinct signal).
#' @noRd
status_cell_html <- function(match_type, validator) {
    mt <- as.character(match_type)
    vd <- as.character(validator)
    vapply(seq_along(mt), function(i) {
        if (is.na(mt[[i]])) {
            return("<span class='muted'>já validado</span>")
        }
        src <- if (is.na(vd[[i]])) "" else paste0(" · ", htmltools::htmlEscape(vd[[i]]))
        sprintf("<span class='status'>%s%s</span>", htmltools::htmlEscape(mt[[i]]), src)
    }, FUN.VALUE = character(1))
}

#' Results module server (Step 3)
#'
#' @param id Module id.
#' @param result_r Reactive from `mod_process_server()` (list or NULL).
#' @param visible_r Reactive `TRUE` when Step 3 is showing (drives the map paint).
#' @return `list(go)`: `go` fires on "Ir para exportação".
#' @noRd
mod_results_server <- function(id, result_r, visible_r = shiny::reactive(TRUE)) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        geo_r <- shiny::reactive(result_r()$geo)

        # Per-record view feeding both the table and the side panel.
        view_r <- shiny::reactive({
            res <- result_r()
            shiny::req(res)
            build_results_view(res$dwc, res$cascade, res$geo,
                               record_areas = res$record_areas)
        })

        # One filter input per dimension; the pure helper does the filtering.
        filters_r <- shiny::reactive({
            list(flags = input$flt_flags, kingdoms = input$flt_kingdoms,
                 conservation = input$flt_conservation,
                 match_types = input$flt_match_types,
                 areas = input$flt_areas)
        })

        # The table and the detail panel read from this filtered view (all
        # dimensions). The map uses map_species_r below, which deliberately drops
        # the distribution flags — see the mod_map_server call for why.
        filtered_view_r <- shiny::reactive({
            f <- filters_r()
            filter_results_view(
                view_r(), flags = f$flags, kingdoms = f$kingdoms,
                conservation = f$conservation, match_types = f$match_types,
                areas = f$areas
            )
        })

        # The map only ever has markers for "confirmada" species (a marker is a
        # GBIF point inside the buffer, which is exactly what makes a species
        # "confirmada"), so filtering it by distributionFlag is degenerate — the
        # distribution pills drive the table only. The map still narrows on the
        # orthogonal dimensions (kingdom / conservation / match type).
        map_species_r <- shiny::reactive({
            f <- filters_r()
            if (!any(lengths(f[c("kingdoms", "conservation", "match_types")]) > 0L)) {
                return(NULL)          # no orthogonal filter active = paint every occurrence
            }
            filtered <- filter_results_view(
                view_r(), flags = NULL, kingdoms = f$kingdoms,
                conservation = f$conservation, match_types = f$match_types
            )
            unique(filtered$scientificName)
        })
        # The area filter goes to the map on its own reactive: unlike the
        # species dimensions it also drives which polygons are drawn and how the
        # viewport is framed.
        mod_map_server(
            "map", geo_r = geo_r, visible_r = visible_r,
            species_r = map_species_r,
            areas_r = shiny::reactive(filters_r()$areas)
        )
        mod_geo_verify_server("geo", geo_r = geo_r)

        # The pills depend on the DATA only, never on the filter state: toggling
        # a pill must not re-render (and reset) the controls under the cursor.
        # Counts come from the unfiltered view, so they hold still while filtering.
        output$filters <- shiny::renderUI({
            view <- view_r()
            shiny::req(nrow(view) > 0L)
            dims <- results_filter_options(view)
            groups <- lapply(names(dims), function(nm) {
                opts <- dims[[nm]]$options
                if (nrow(opts) == 0L) {
                    return(NULL)
                }
                choice_names <- lapply(seq_len(nrow(opts)), function(i) {
                    shiny::HTML(sprintf(
                        paste0("<span class='filter-pill %s' title='%s'>",
                               "<span class='dot'></span>%s",
                               "<span class='filter-pill__count'>%d</span></span>"),
                        opts$class[[i]],
                        htmltools::htmlEscape(opts$title[[i]], attribute = TRUE),
                        htmltools::htmlEscape(opts$label[[i]]),
                        opts$count[[i]]
                    ))
                })
                shiny::checkboxGroupInput(
                    ns(paste0("flt_", nm)), dims[[nm]]$label,
                    choiceNames = choice_names, choiceValues = opts$key,
                    inline = TRUE
                )
            })
            shiny::tags$div(class = "filter-groups", groups)
        })

        output$filter_count <- shiny::renderText({
            total <- nrow(view_r())
            shown <- nrow(filtered_view_r())
            fmt_int <- function(x) format(x, big.mark = ".", decimal.mark = ",")
            if (shown == total) {
                sprintf("%s registros", fmt_int(total))
            } else {
                sprintf("%s de %s registros", fmt_int(shown), fmt_int(total))
            }
        })

        shiny::observeEvent(input$flt_clear, {
            for (nm in names(results_filter_dimensions())) {
                shiny::updateCheckboxGroupInput(
                    session, paste0("flt_", nm), selected = character(0)
                )
            }
        }, ignoreInit = TRUE)

        output$summary <- shiny::renderUI({
            res <- result_r()
            shiny::req(res)
            s <- res$summary
            if (is.null(s)) {
                s <- process_summary(res$dwc, res$cascade, res$geo)
            }
            # PT-BR grouping: "." thousands, "," decimals (decimal.mark set
            # explicitly so format() does not warn that both marks are ".").
            fmt_int <- function(x) format(x, big.mark = ".", decimal.mark = ",")
            alerts <- if (is.na(s$alerts)) "—" else fmt_int(s$alerts)
            tile <- function(value, label, variant = "") {
                shiny::tags$div(
                    class = paste("stat-tile", variant),
                    shiny::tags$span(class = "stat-tile__value", value),
                    shiny::tags$span(class = "stat-tile__label", label)
                )
            }
            shiny::tags$div(
                class = "stat-strip",
                tile(fmt_int(s$records), "Registros", "stat-tile--primary"),
                tile(fmt_int(s$species), "Espécies novas"),
                tile(paste0(s$resolved_pct, "%"), "Resolvidas", "stat-tile--info"),
                tile(alerts, "Alertas geográficos", "stat-tile--secondary"),
                tile(fmt_int(s$invasive), "Invasoras", "stat-tile--warning")
            )
        })

        output$table <- DT::renderDataTable({
            view <- filtered_view_r()
            # The invasive badge rides next to the name rather than in a column of
            # its own: it is empty for most rows, and the alert belongs on the
            # species it is about.
            badge <- invasive_badge_html(view$invasive, view$invasiveSource)
            disp <- data.frame(
                Espécie = paste0(
                    sprintf("<span class='sci-name'>%s</span>",
                            htmltools::htmlEscape(view$scientificName)),
                    ifelse(nzchar(badge), paste0(" ", badge), "")
                ),
                Família = ifelse(is.na(view$family) | !nzchar(view$family), "—", view$family),
                `Status taxonômico` = status_cell_html(view$matchType, view$validator),
                Distribuição = flag_badge_html(view$distributionFlag),
                check.names = FALSE, stringsAsFactors = FALSE
            )
            DT::datatable(
                disp, escape = FALSE, rownames = FALSE, selection = "single",
                options = list(
                    scrollX = TRUE, pageLength = 12,
                    columnDefs = list(list(className = "dt-left", targets = "_all")),
                    # PT-BR chrome (SPEC §2.1) — and the empty state a filter can
                    # now produce has to speak PT-BR too.
                    language = list(
                        emptyTable = "Nenhum registro corresponde aos filtros.",
                        zeroRecords = "Nenhum registro corresponde aos filtros.",
                        info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                        infoEmpty = "Nenhum registro",
                        infoFiltered = "(filtrados de _MAX_)",
                        search = "Buscar:",
                        lengthMenu = "Mostrar _MENU_ registros",
                        paginate = list(previous = "Anterior", `next` = "Próxima")
                    )
                )
            )
        })

        output$detail <- shiny::renderUI({
            view <- filtered_view_r()
            sel <- input$table_rows_selected
            if (is.null(sel) || length(sel) == 0L) {
                return(shiny::tags$div(
                    class = "detail-panel",
                    shiny::tags$p(class = "muted",
                        "Selecione uma linha para ver a taxonomia e a distribuição da espécie.")
                ))
            }
            row <- view[sel, , drop = FALSE]
            kv <- function(key, val) {
                if (is.na(val) || !nzchar(as.character(val))) val <- "—"
                shiny::tags$div(
                    shiny::tags$div(class = "detail-panel__key", key),
                    shiny::tags$div(class = "detail-panel__val", val)
                )
            }
            # Conservation shown as a threat-level badge (IUCN/MMA colours).
            kv_threat <- function(key, status) {
                val <- if (is.na(status) || !nzchar(as.character(status))) {
                    shiny::tags$span(class = "muted", "—")
                } else {
                    shiny::tags$span(
                        class = paste("threat-badge", threat_status_class(status)),
                        shiny::tags$span(class = "dot"), as.character(status)
                    )
                }
                shiny::tags$div(
                    shiny::tags$div(class = "detail-panel__key", key),
                    shiny::tags$div(class = "detail-panel__val", val)
                )
            }
            # Invasive listing: the badge, plus the source list(s) spelled out —
            # in a detail panel there is room to say who says so.
            kv_invasive <- function(key, invasive, source) {
                val <- if (!isTRUE(as.logical(invasive))) {
                    shiny::tags$span(class = "muted", "—")
                } else {
                    shiny::tags$div(
                        shiny::HTML(invasive_badge_html(invasive, source)),
                        if (is_non_empty(source)) {
                            shiny::tags$div(
                                class = "muted",
                                style = "margin-top: var(--space-2); font-size: var(--text-xs);",
                                as.character(source)
                            )
                        }
                    )
                }
                shiny::tags$div(
                    shiny::tags$div(class = "detail-panel__key", key),
                    shiny::tags$div(class = "detail-panel__val", val)
                )
            }
            # Make the taxon (animal / plant / …) obvious next to the name.
            tx <- kingdom_badge_parts(row$kingdom)
            taxon_badge <- if (is.null(tx)) NULL else shiny::tags$span(
                class = paste("taxon-badge", tx$cls),
                shiny::icon(tx$icon), tx$label
            )
            # A species GBIF refused (rate limit) was never asked about, so it
            # must not read like a species with no nearby record — that is the
            # exact confusion LESSONS L-023 warns about.
            gbif_txt <- if (isTRUE(row$gbif_failed)) {
                "não consultado (limite GBIF)"
            } else if (is.na(row$gbif_count)) {
                "—"
            } else {
                sprintf("%d registros", row$gbif_count)
            }
            shiny::tags$div(
                class = "detail-panel",
                shiny::tags$div(
                    style = "display:flex; align-items:center; justify-content:space-between; gap:var(--space-4); flex-wrap:wrap;",
                    shiny::tags$div(
                        style = "display:flex; align-items:center; gap:var(--space-3); flex-wrap:wrap;",
                        shiny::tags$span(class = "detail-panel__name", row$scientificName),
                        taxon_badge
                    ),
                    shiny::HTML(flag_badge_html(row$distributionFlag))
                ),
                shiny::tags$div(
                    class = "detail-panel__section",
                    shiny::tags$span(class = "eyebrow", "Taxonomia"),
                    shiny::tags$div(
                        class = "detail-grid", style = "margin-top: var(--space-3);",
                        kv("Kingdom", row$kingdom), kv("Family", row$family),
                        kv("Genus", row$genus), kv("taxonID", row$taxonID)
                    )
                ),
                shiny::tags$div(
                    class = "detail-panel__section",
                    shiny::tags$span(class = "eyebrow", "Status & distribuição"),
                    shiny::tags$div(
                        class = "detail-grid", style = "margin-top: var(--space-3);",
                        kv("Validação", if (is.na(row$matchType)) "já validado" else
                            paste0(row$matchType, if (!is.na(row$validator)) paste0(" · ", row$validator) else "")),
                        kv_threat("Conservação", row$status),
                        kv("GBIF ≤ 10 km", gbif_txt),
                        kv_invasive("Exótica invasora", row$invasive, row$invasiveSource)
                    )
                )
            )
        })

        list(go = shiny::reactive(input$to_export))
    })
}
