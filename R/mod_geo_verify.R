# Title: Geo Verification Summary (Step 3)
# Display bridge for the geographic verification result: which UF(s)/biome(s) the
# operation area falls in, and how the records split across the four
# distributionFlag categories (SPEC §8). Read-only; fed by the geo slot of the
# process result. Since the rework it is the card floating over the top-left of
# the map (ADR-019) — it is passed to mod_map_ui(context = ) — so it stays
# compact. UI text is PT-BR (SPEC §2.1). The map itself is mod_map.R.

#' Geo verification summary UI (Step 3) — the map's context card.
#'
#' @param id Module id.
#' @return A div hosting the server-rendered summary.
#' @noRd
mod_geo_verify_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tags$div(class = "map-context", shiny::uiOutput(ns("summary")))
}

#' Geo verification summary server (Step 3)
#'
#' @param id Module id.
#' @param geo_r Reactive returning the geo list (or `NULL`).
#' @return Invisibly NULL.
#' @noRd
mod_geo_verify_server <- function(id, geo_r) {
    shiny::moduleServer(id, function(input, output, session) {
        output$summary <- shiny::renderUI({
            geo <- geo_r()
            if (is.null(geo)) {
                return(shiny::tagList(
                    shiny::tags$span(class = "eyebrow", "Áreas de operação"),
                    shiny::tags$p(class = "geo-empty",
                        "Nenhum shapefile enviado — sem verificação de distribuição.")
                ))
            }

            # UF(s) and biome(s) share one wrapping chip row: in the map's
            # context card there is no room for a labelled block per dimension.
            chip <- function(v, labeller = NULL, class_fn = NULL) {
                cls <- trimws(paste("chip", if (!is.null(class_fn)) class_fn(v) else ""))
                shiny::tags$span(
                    class = cls,
                    if (!is.null(labeller)) labeller(v) else v
                )
            }
            # One block per uploaded area. With a single area the name/colour
            # header is dropped — there is nothing to tell apart, and the card
            # stays exactly as compact as it was before multi-area.
            areas <- geo$areas %||% list()
            multi <- length(areas) > 1L
            area_block <- function(i) {
                a <- areas[[i]]
                states <- if (length(a$area_states) == 0L) "—" else a$area_states
                shiny::tags$div(
                    class = "geo-area",
                    if (multi) {
                        shiny::tags$div(
                            class = "geo-area__head",
                            shiny::tags$span(
                                class = "geo-area__dot",
                                style = sprintf("background:%s;", area_colour(i))
                            ),
                            shiny::tags$span(class = "geo-area__name", a$name)
                        )
                    },
                    shiny::tags$div(
                        class = "geo-chips",
                        lapply(states, chip),
                        lapply(a$area_biomes, chip, labeller = biome_label,
                               class_fn = biome_class)
                    )
                )
            }
            area_chips <- shiny::tagList(lapply(seq_along(areas), area_block))

            lv <- distribution_flag_levels()
            counts <- geo_flag_counts(geo$per_species$distributionFlag)
            # A vertical list (dot + count + wrapping label) — the long PT-BR
            # labels overflowed a nowrap pill, especially in the narrow sidebar.
            count_row <- function(key) {
                mod <- sub("^flag-", "geo-count--", distribution_flag_class(lv[[key]]))
                shiny::tags$div(
                    class = paste("geo-count", mod),
                    shiny::tags$span(class = "geo-count__dot"),
                    shiny::tags$span(class = "geo-count__n",
                                     format(counts[[key]], big.mark = ".", decimal.mark = ",")),
                    shiny::tags$span(class = "geo-count__label", lv[[key]])
                )
            }

            # The "alert, not verdict" caveat is not repeated here: it sits in
            # the step's title block (mod_results.R), where there is room for it.
            shiny::tagList(
                shiny::tags$span(
                    class = "eyebrow",
                    if (multi) "Áreas de operação" else "Área de operação"
                ),
                area_chips,
                shiny::tags$div(
                    class = "geo-counts",
                    count_row("confirmed"), count_row("near_absent"),
                    count_row("outside"), count_row("no_data")
                ),
                # The legend says this too, but it is hidden on narrow screens
                # (06-map.css) and this card never is.
                shiny::tags$p(
                    class = "geo-note",
                    "Os pontos no mapa são registros de ocorrência do GBIF."
                )
            )
        })
        invisible(NULL)
    })
}
