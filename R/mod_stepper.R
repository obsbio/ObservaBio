# Title: Wizard Stepper (the rail's step trail)
# Bridge: renders the 4-step trail (Enviar → Processar → Resultado → Exportar)
# inside the left rail from the app-level step reactive, and lets the user click
# any step they have already reached to jump back or forward. app_server still
# owns the step state (and the "max reached" gate); the stepper only reports the
# clicked index via the `goto` reactive. UI text is PT-BR (SPEC §2.1).

#' The four wizard steps, in order: PT-BR label + the subtitle shown under it in
#' the rail.
#' @noRd
wizard_steps <- function() {
    list(
        list(label = "Enviar",    sub = "planilha + área"),
        list(label = "Processar", sub = "cascata + geo"),
        list(label = "Resultado", sub = "mapa + tabela"),
        list(label = "Exportar",  sub = "2 planilhas")
    )
}

#' Stepper module UI
#'
#' @param id Module id.
#' @return A Shiny nav div hosting the server-rendered trail.
#' @noRd
mod_stepper_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tags$nav(class = "stepper", shiny::uiOutput(ns("bar")))
}

#' Stepper module server
#'
#' @param id Module id.
#' @param step_r Reactive returning the current step index (1-based).
#' @param reached_r Reactive returning the highest step index reached so far;
#'   steps up to it (except the current one) are rendered clickable.
#' @return A list with `goto`: a reactive of the last-clicked step index.
#' @noRd
mod_stepper_server <- function(id, step_r, reached_r = shiny::reactive(1L)) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output$bar <- shiny::renderUI({
            current <- as.integer(step_r())
            reached <- as.integer(reached_r())
            steps <- wizard_steps()

            nodes <- lapply(seq_along(steps), function(i) {
                state <- if (i < current) "is-done" else if (i == current) "is-active" else ""
                navigable <- i <= reached && i != current
                marker <- if (i < current) shiny::icon("check") else as.character(i)
                step <- shiny::tags$div(
                    class = trimws(paste("step", state, if (navigable) "is-clickable" else "")),
                    shiny::tags$span(class = "step__marker", marker),
                    shiny::tags$div(
                        class = "step__body",
                        shiny::tags$div(class = "step__label", steps[[i]]$label),
                        shiny::tags$div(class = "step__sub", steps[[i]]$sub)
                    )
                )
                if (!navigable) {
                    return(step)
                }
                # Report the clicked index; priority=event so re-clicking the
                # same step still fires (app_server gates against reached).
                htmltools::tagAppendAttributes(
                    step,
                    onclick = sprintf(
                        "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                        ns("goto"), i
                    )
                )
            })

            shiny::tagList(nodes)
        })

        list(goto = shiny::reactive(input$goto))
    })
}
