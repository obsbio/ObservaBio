# Title: Map Module (Step 3 — leaflet)
# The operation polygon + 10 km buffer + GBIF occurrences — the hero of the
# results workspace (fills the main column). Follows the Saira map lessons
# (ui-design.md): the leafletOutput lives in a STATIC container (never a renderUI
# that depends on dynamic reactives), shapes/markers are added only via
# leafletProxy in an observe, and suspendWhenHidden = FALSE keeps the widget live
# while the Resultado tab is hidden. Minimal chrome (no layer menus). UI text is
# PT-BR (SPEC §2.1).

#' Map module UI (Step 3)
#'
#' @param id Module id.
#' @param context Optional UI floated over the map's top-left corner — the
#'   operation-area context (`mod_geo_verify_ui()`), which the rework puts on the
#'   map instead of in a sidebar beside it (ADR-019).
#' @return A `shiny::tags$div` map card.
#' @noRd
mod_map_ui <- function(id, context = NULL) {
    ns <- shiny::NS(id)
    shiny::tags$div(
        class = "map-card",
        # height:100% hands the sizing to the CSS (06-map.css), which scales the
        # hero with the viewport; an inline px height would override it.
        leaflet::leafletOutput(ns("map"), height = "100%"),
        # After the map in the DOM (it floats over it via CSS) so that on a
        # narrow screen, where it stops floating, it lands under the map.
        context,
        shiny::tags$div(
            class = "map-legend",
            title = paste0(
                "Os pontos são registros de ocorrência do GBIF para as espécies ",
                "da lista, dentro do buffer de 10 km da área."
            ),
            shiny::tags$div(class = "map-legend__item",
                shiny::tags$span(class = "map-legend__swatch map-legend__swatch--area"), "Áreas"),
            shiny::tags$div(class = "map-legend__item",
                shiny::tags$span(class = "map-legend__swatch map-legend__swatch--buffer"), "Buffer 10 km"),
            shiny::tags$div(class = "map-legend__item",
                shiny::tags$span(class = "map-legend__swatch map-legend__swatch--occ"), "Ocorrências GBIF")
        )
    )
}

#' Build the popup HTML for an occurrence marker (espécie + fonte + área).
#'
#' The source is a GBIF datasetKey, so the label names GBIF explicitly — the
#' bare UUID said nothing about where the point came from.
#' @noRd
occ_popup <- function(species, source, area = NULL) {
    sp <- htmltools::htmlEscape(as.character(species))
    src <- htmltools::htmlEscape(as.character(source))
    src[is.na(source) | !nzchar(as.character(source))] <- "—"
    out <- paste0("<span class='popup-sci'>", sp, "</span>",
                  "<br><span class='popup-src'>Fonte: GBIF · ", src, "</span>")
    if (!is.null(area)) {
        ar <- htmltools::htmlEscape(as.character(area))
        ar[is.na(area) | !nzchar(as.character(area))] <- "—"
        out <- paste0(out, "<br><span class='popup-src'>Área: ", ar, "</span>")
    }
    out
}

#' Map module server (Step 3)
#'
#' @param id Module id.
#' @param geo_r Reactive returning the geo list from `run_geo_verification()`
#'   (or `NULL`).
#' @param visible_r Reactive `TRUE` when the map's step is showing; the draw
#'   re-runs on show so `fitBounds` sees the real (non-zero) map size.
#' @param focus_r Optional reactive returning a species name to zoom to
#'   ("Ver no mapa" from the side panel).
#' @param species_r Optional reactive returning the species names to paint
#'   (the Resultado filter). `NULL` — the default, and what an unfiltered screen
#'   passes — paints every occurrence.
#' @param areas_r Optional reactive returning the study-area names to show (the
#'   Resultado "Área" filter). `NULL`/empty shows every area.
#' @return Invisibly NULL.
#' @noRd
mod_map_server <- function(id, geo_r, visible_r = shiny::reactive(TRUE),
                           focus_r = NULL, species_r = NULL, areas_r = NULL) {
    shiny::moduleServer(id, function(input, output, session) {

        # Base skeleton only — no dynamic data here (Saira lesson).
        output$map <- leaflet::renderLeaflet({
            leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
                leaflet::addProviderTiles(
                    leaflet::providers$CartoDB.Positron,
                    options = leaflet::providerTileOptions(noWrap = TRUE)
                ) |>
                leaflet::setView(lng = -52, lat = -15, zoom = 4)
        })
        shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

        # Which areas to draw: the "Área" filter, or all of them.
        visible_areas <- function(geo, selected) {
            areas <- geo$areas %||% list()
            if (length(areas) == 0L || length(selected) == 0L) {
                return(seq_along(areas))
            }
            keep <- which(vapply(areas, function(a) a$name %in% selected, logical(1)))
            if (length(keep) == 0L) seq_along(areas) else keep
        }

        # Shapes: the area + buffer polygons and the bounds. Depends on the geo
        # result AND visibility so the first paint (and fitBounds) happens once
        # the tab is shown and the container has a real size. It also depends on
        # the AREA filter — and only that one: narrowing to an area is a request
        # to look at that area, so reframing is the point. The species-level
        # filters still must not redraw the polygons or move the viewport out
        # from under the user.
        shiny::observe({
            visible <- isTRUE(visible_r())
            geo <- geo_r()
            selected <- if (is.null(areas_r)) NULL else areas_r()
            proxy <- leaflet::leafletProxy(session$ns("map"))
            proxy <- leaflet::clearShapes(proxy)
            if (!visible || is.null(geo)) {
                return(invisible(NULL))
            }
            areas <- geo$areas %||% list()
            idx <- visible_areas(geo, selected)
            if (length(idx) == 0L) {
                return(invisible(NULL))
            }

            # Every buffer first, then the areas on top of them.
            for (i in idx) {
                proxy <- leaflet::addPolygons(
                    proxy, data = sf::st_sf(geometry = areas[[i]]$buffer),
                    color = "#C9AE7C", weight = 1.5, fillColor = "#E5A93C",
                    fillOpacity = 0.15, group = "buffer"
                )
            }
            for (i in idx) {
                col <- area_colour(i)
                proxy <- leaflet::addPolygons(
                    proxy, data = sf::st_sf(geometry = areas[[i]]$area),
                    color = col, weight = 2, fillColor = col,
                    fillOpacity = 0.12, group = "area", label = areas[[i]]$name
                )
            }

            bb <- sf::st_bbox(do.call(c, lapply(areas[idx], function(a) a$buffer)))
            leaflet::fitBounds(proxy, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
        })

        # Markers: the GBIF occurrences, repainted (never re-rendered) whenever
        # the Resultado filter changes the species set — clearMarkers +
        # addCircleMarkers on the same proxy, so the table and the map always
        # show the same species.
        shiny::observe({
            visible <- isTRUE(visible_r())
            geo <- geo_r()
            species <- if (is.null(species_r)) NULL else species_r()
            selected <- if (is.null(areas_r)) NULL else areas_r()
            proxy <- leaflet::leafletProxy(session$ns("map"))
            proxy <- leaflet::clearMarkers(proxy)
            if (!visible || is.null(geo)) {
                return(invisible(NULL))
            }

            occ <- geo$occ
            if (!is.data.frame(occ) || nrow(occ) == 0L) {
                return(invisible(NULL))
            }
            if (!is.null(species)) {
                occ <- occ[as.character(occ$species) %in% species, , drop = FALSE]
            }
            if (length(selected) > 0L && "area" %in% names(occ)) {
                occ <- occ[as.character(occ$area) %in% selected, , drop = FALSE]
            }
            if (nrow(occ) == 0L) {
                return(invisible(NULL))
            }
            leaflet::addCircleMarkers(
                proxy, data = occ,
                lng = ~decimalLongitude, lat = ~decimalLatitude,
                radius = 5, stroke = TRUE, weight = 1,
                color = "#A84A30", fillColor = "#C86446", fillOpacity = 0.85,
                popup = occ_popup(occ$species, occ$datasetKey, occ$area),
                group = "occ"
            )
        })

        # "Ver no mapa" — zoom to a species' occurrences when asked.
        if (!is.null(focus_r)) {
            shiny::observeEvent(focus_r(), {
                sp <- focus_r()
                geo <- geo_r()
                if (is.null(sp) || is.null(geo) || !is.data.frame(geo$occ)) {
                    return(invisible(NULL))
                }
                pts <- geo$occ[as.character(geo$occ$species) == sp, , drop = FALSE]
                proxy <- leaflet::leafletProxy(session$ns("map"))
                if (nrow(pts) == 0L) {
                    failed <- geo$gbif_failed
                    msg <- if (!is.null(failed) && sp %in% failed) {
                        paste0("Não foi possível consultar o GBIF para esta espécie ",
                               "(limite de requisições). Processe novamente para tentar de novo.")
                    } else {
                        "Sem ocorrências GBIF a ≤ 10 km para esta espécie."
                    }
                    shiny::showNotification(msg, type = "message", duration = 5)
                    return(invisible(NULL))
                }
                leaflet::flyToBounds(
                    proxy, min(pts$decimalLongitude), min(pts$decimalLatitude),
                    max(pts$decimalLongitude), max(pts$decimalLatitude)
                )
            }, ignoreInit = TRUE)
        }

        invisible(NULL)
    })
}
