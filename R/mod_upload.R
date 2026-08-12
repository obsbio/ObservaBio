# Title: Upload Module (Step 1 — Enviar)
# Reactive bridge only: reads the species table via read_ObservaBio_table() and
# validates the shapefile archives via unzip_shapefiles() (pure helpers). Returns
# `list(data, iucn_key, go)` — the parsed upload reactive, the optional
# session-only IUCN key (ADR-005), plus a "Continuar" advance signal the
# app_server observes to move to Step 2. UI text is PT-BR (SPEC §2.1).
#
# One `.zip` is one study area. When the sheet has a `locality` column, the user
# links each area to the localities it covers in the mapping panel below the
# uploads; only linked records are geo-verified (see utils_geo_areas.R).

#' Upload module UI (Step 1)
#'
#' @param id Module id.
#' @return A `shiny::tagList`.
#' @noRd
mod_upload_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$div(
            class = "step-panel",
            step_header(1, "Enviar", "Enviar arquivos"),
            shiny::tags$p(
                class = "step-lede",
                "Carregue a planilha de espécies e, opcionalmente, os shapefiles ",
                "das áreas de operação — um ", shiny::tags$code(".zip"),
                " por área. O separador e a coluna ",
                shiny::tags$code("scientificName"),
                " são detectados automaticamente."
            ),
            shiny::tags$div(
                class = "upload-form",
                shiny::tags$div(
                    class = "upload-duo",
                    upload_card(
                        ns("planilha"), badge = "tax", icon_name = "table",
                        title = "Dados taxonômicos",
                        formats = ".xlsx · .csv · .tsv · .txt",
                        hint = "planilha de inventário",
                        accept = c(".xlsx", ".csv", ".tsv", ".txt"),
                        status_id = ns("planilha_status")
                    ),
                    upload_card(
                        ns("shapefile"), badge = "geo", icon_name = "map",
                        title = "Áreas de estudo",
                        formats = "shapefiles .zip · opcional",
                        hint = "um .zip por área · pode enviar uma de cada vez",
                        accept = ".zip", status_id = ns("shapefile_status"),
                        multiple = TRUE
                    )
                ),
                shiny::uiOutput(ns("area_mapping")),
                advanced_options(ns),
                shiny::tags$div(
                    class = "upload-actions",
                    shiny::actionButton(
                        ns("continuar"), "Continuar",
                        class = "btn-primary btn-lg",
                        icon = shiny::icon("arrow-right")
                    )
                )
            )
        )
    )
}

#' "Opções avançadas" disclosure holding the optional IUCN key (ADR-005).
#'
#' Collapsed by default: the app works without a key, so the field must not
#' compete with the two uploads. The key is masked, lives only in this Shiny
#' session, and is never written to disk.
#' @noRd
advanced_options <- function(ns) {
    shiny::tags$details(
        class = "advanced-options",
        shiny::tags$summary(
            shiny::icon("sliders"), "Opções avançadas"
        ),
        shiny::tags$div(
            class = "advanced-options__body",
            shiny::passwordInput(
                ns("iucn_key"), label = "Chave IUCN (opcional)",
                placeholder = "Cole aqui a sua chave da API da IUCN"
            ),
            shiny::tags$p(
                class = "muted",
                "Opcional. Com a chave, preenchemos também o campo ",
                shiny::tags$em("criteria"), " da IUCN. Sem ela, a categoria ",
                "IUCN ainda é obtida pelo GBIF. A chave não é salva — some ",
                "quando você fecha a aba."
            ),
            shiny::tags$p(
                class = "muted",
                "Solicite uma chave gratuita em ",
                shiny::tags$a(
                    href = "https://api.iucnredlist.org", target = "_blank",
                    rel = "noopener noreferrer", "api.iucnredlist.org"
                ), "."
            )
        )
    )
}

#' One upload card: the dashed drop zone is the shell, Shiny's own `fileInput`
#' sits inside it and still owns the upload (04-components.css stretches its
#' file-picker label over the zone; www/js/dropzone.js forwards a dropped file
#' to the same input).
#' @noRd
upload_card <- function(input_id, badge, icon_name, title, formats, hint,
                        accept, status_id, multiple = FALSE) {
    shiny::tags$div(
        class = paste0("zh-card upload-card--", badge),
        shiny::tags$div(
            class = "zh-card__body upload-card",
            shiny::tags$div(
                class = "upload-card__head",
                shiny::tags$span(
                    class = paste0("upload-card__badge upload-card__badge--", badge),
                    shiny::icon(icon_name)
                ),
                shiny::tags$div(
                    shiny::tags$h3(class = "upload-card__title", title),
                    shiny::tags$span(class = "upload-card__formats", formats)
                )
            ),
            shiny::tags$div(
                class = paste0("dropzone dropzone--", badge),
                `data-dropzone` = NA,
                shiny::tags$div(class = "dropzone__icon", shiny::icon("upload")),
                shiny::tags$div(class = "dropzone__title", "Arraste ou clique para enviar"),
                shiny::tags$div(class = "dropzone__hint", hint),
                shiny::fileInput(
                    input_id, label = NULL, accept = accept, multiple = multiple,
                    buttonLabel = "Procurar…", placeholder = "Nenhum arquivo"
                )
            ),
            shiny::uiOutput(status_id)
        )
    )
}

#' Upload module server (Step 1)
#'
#' @param id Module id.
#' @return `list(data, iucn_key, go)`: `data` is a reactive with `records`,
#'   `model_cols`, `pt_labels`, `areas` (NULL until a valid `.zip`);
#'   `iucn_key` is a reactive with the optional session-only IUCN key; `go` is a
#'   reactive firing on the "Continuar" click.
#' @noRd
mod_upload_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        planilha_r <- shiny::reactive({
            shiny::req(input$planilha)
            tryCatch(
                read_ObservaBio_table(input$planilha$datapath),
                error = function(e) {
                    shiny::showNotification(
                        paste("Falha ao ler a planilha:", conditionMessage(e)),
                        type = "error", duration = 8
                    )
                    NULL
                }
            )
        })

        # One `.zip` per study area, named after the archive. Areas ACCUMULATE
        # across uploads: `fileInput` replaces its selection every time, so
        # sending the second area in a second action would otherwise drop the
        # first one silently (merge_areas() holds what we have). A bad archive
        # is reported by name and skipped; the valid ones still go through.
        areas_rv <- shiny::reactiveVal(list())

        shiny::observeEvent(input$shapefile, {
            files <- input$shapefile
            if (is.null(files) || nrow(files) == 0L) {
                return()
            }
            parsed <- unzip_shapefiles(files$datapath, files$name)
            if (length(parsed$errors) > 0L) {
                shiny::showNotification(
                    paste("Shapefile inválido —",
                          paste(sprintf("%s: %s", names(parsed$errors), parsed$errors),
                                collapse = " · ")),
                    type = "error", duration = 8
                )
            }
            areas_rv(merge_areas(areas_rv(), parsed$areas))
        })

        shiny::observeEvent(input$area_remove, {
            areas_rv(drop_area(areas_rv(), input$area_remove))
        }, ignoreInit = TRUE)

        shapefiles_r <- shiny::reactive({
            areas <- areas_rv()
            if (length(areas) == 0L) NULL else areas
        })

        # The `locality` universe the areas are linked against.
        localities_r <- shiny::reactive({
            parsed <- planilha_r()
            if (is.null(parsed) || !"locality" %in% names(parsed$records)) {
                return(character(0))
            }
            loc <- trimws(as.character(parsed$records$locality))
            sort(unique(loc[!is.na(loc) & nzchar(loc)]))
        })

        # The mapping panel depends on the DATA only (which archives, which
        # localities) — never on the selections themselves, so re-rendering can
        # never reset the field under the user's cursor. Same discipline as the
        # results filter pills (mod_results.R).
        output$area_mapping <- shiny::renderUI({
            areas <- shapefiles_r()
            if (is.null(areas)) {
                return(NULL)
            }
            locs <- localities_r()

            if (length(locs) == 0L) {
                note <- if (length(areas) == 1L) {
                    paste0("A planilha não tem a coluna locality preenchida — a área ",
                           "enviada vale para todos os registros.")
                } else {
                    paste0("A planilha não tem a coluna locality preenchida, então não ",
                           "há como ligar os registros a cada uma das ", length(areas),
                           " áreas. Envie apenas uma área, ou preencha locality na planilha.")
                }
                return(shiny::tags$div(
                    class = "area-map",
                    shiny::tags$span(class = "eyebrow", "Vínculo com locality"),
                    shiny::tags$p(class = "muted", note)
                ))
            }

            rows <- lapply(seq_along(areas), function(i) {
                nm <- areas[[i]]$name
                slug <- area_slug(nm)
                # Suggestion only: the file name usually already IS the locality.
                suggested <- locs[area_key(locs) %in% area_key(nm)]
                # Adding or removing an area re-renders every row, so carry the
                # choices already made across the rebuild (isolate(): reading a
                # row's own input here must not make the panel depend on it).
                current <- shiny::isolate(input[[paste0("area_loc_", slug)]])
                selected <- if (is.null(current)) suggested else intersect(current, locs)
                shiny::tags$div(
                    class = "area-map__row",
                    shiny::tags$span(
                        class = "area-map__name",
                        shiny::icon("draw-polygon"), nm
                    ),
                    shiny::selectizeInput(
                        ns(paste0("area_loc_", slug)), label = NULL,
                        choices = locs, selected = selected, multiple = TRUE,
                        width = "100%",
                        options = list(placeholder = "Localidades desta área")
                    ),
                    # One static observer serves every row: the click reports
                    # which slug to drop instead of each row owning an observer.
                    shiny::tags$button(
                        type = "button", class = "area-map__remove",
                        title = paste("Remover", nm),
                        `aria-label` = paste("Remover", nm),
                        onclick = sprintf(
                            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                            ns("area_remove"), slug
                        ),
                        shiny::icon("xmark")
                    )
                )
            })

            shiny::tags$div(
                class = "area-map",
                shiny::tags$span(class = "eyebrow", "Vínculo com locality"),
                shiny::tags$p(
                    class = "muted",
                    "Ligue cada área aos valores de ", shiny::tags$code("locality"),
                    " que ela cobre. Só os registros vinculados entram na ",
                    "verificação geográfica."
                ),
                rows
            )
        })

        # The upload payload's areas: the unpacked archives plus the links the
        # user made above.
        areas_r <- shiny::reactive({
            areas <- shapefiles_r()
            if (is.null(areas)) {
                return(NULL)
            }
            lapply(areas, function(a) {
                a$localities <- input[[paste0("area_loc_", area_slug(a$name))]] %||%
                    character(0)
                a
            })
        })

        # The upload's own read-back: a success chip once the file parses, the
        # standing caveat otherwise (the shapefile is optional).
        ok_chip <- function(text) {
            shiny::tags$span(
                class = "badge-pill flag-confirmed",
                shiny::tags$span(class = "dot"), text
            )
        }

        output$planilha_status <- shiny::renderUI({
            if (is.null(input$planilha)) {
                return(NULL)
            }
            parsed <- planilha_r()
            if (is.null(parsed)) {
                return(shiny::tags$p(class = "upload-card__note", "Planilha inválida."))
            }
            ok_chip(sprintf("%d registros · %d colunas",
                            nrow(parsed$records), length(parsed$model_cols)))
        })

        # Keyed on the accumulated areas, not on input$shapefile: after removing
        # the last area the input still holds its file, but there is no area.
        output$shapefile_status <- shiny::renderUI({
            areas <- shapefiles_r()
            if (is.null(areas)) {
                return(shiny::tags$p(
                    class = "upload-card__note",
                    "Sem shapefile a verificação geográfica fica limitada."
                ))
            }
            ok_chip(sprintf("%d %s", length(areas),
                            if (length(areas) == 1L) "área válida" else "áreas válidas"))
        })

        data_r <- shiny::reactive({
            parsed <- planilha_r()
            if (is.null(parsed)) {
                return(NULL)
            }
            list(
                records = parsed$records,
                model_cols = parsed$model_cols,
                pt_labels = parsed$pt_labels,
                areas = areas_r()
            )
        })

        # Deliberately outside data_r(): the key is sensitive and session-only,
        # so it travels on its own reactive instead of riding along with the
        # upload payload that the rest of the pipeline copies around.
        list(
            data = data_r,
            iucn_key = shiny::reactive(input$iucn_key),
            go = shiny::reactive(input$continuar)
        )
    })
}
