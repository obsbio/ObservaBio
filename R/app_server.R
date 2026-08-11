# Title: Application Server (orchestrator)
# Owns the wizard step state, wires the step modules, and passes reactives
# between them. No business logic here — it lives in the pure utils_*/provider_*
# functions the modules call. Navigation: Continuar (1→2), processing done (2→3),
# Ir para exportação (3→4); the stepper reflects that state and lets the user
# click any already-reached step to jump back or forward (gated by reached_rv).

#' ZHOUSE Shiny server
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Invisibly NULL.
#' @noRd
app_server <- function(input, output, session) {
    step_rv <- shiny::reactiveVal(1L)
    # Highest step reached so far — the stepper may navigate up to here (back or
    # forward), but never skip ahead to a step the flow has not unlocked yet.
    reached_rv <- shiny::reactiveVal(1L)
    go_to <- function(k) {
        k <- as.integer(k)
        step_rv(k)
        if (k > reached_rv()) reached_rv(k)
    }

    # A single place that reflects the step state into the hidden tabset.
    shiny::observeEvent(step_rv(), {
        shiny::updateTabsetPanel(session, "wizard",
                                 selected = paste0("step", step_rv()))
    })

    upload <- mod_upload_server("upload")
    result_r <- mod_process_server("process", upload$data, upload$iucn_key)
    results <- mod_results_server(
        "results", result_r,
        visible_r = shiny::reactive(step_rv() == 3L)
    )
    mod_export_server("export", result_r)
    stepper <- mod_stepper_server(
        "stepper", shiny::reactive(step_rv()), shiny::reactive(reached_rv())
    )
    # The "Como usar" walkthrough — global (its trigger is in the header), and
    # independent of the step state.
    mod_help_server("help")

    # Step 1 → 2: Continuar (guarded on a valid upload).
    shiny::observeEvent(upload$go(), {
        if (is.null(upload$data())) {
            shiny::showNotification(
                "Envie uma planilha válida para continuar.",
                type = "warning", duration = 5
            )
            return(invisible(NULL))
        }
        go_to(2L)
    }, ignoreInit = TRUE)

    # Step 2 → 3: processing produced a result.
    shiny::observeEvent(result_r(), {
        go_to(3L)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Step 3 → 4: Ir para exportação.
    shiny::observeEvent(results$go(), {
        go_to(4L)
    }, ignoreInit = TRUE)

    # Stepper click: jump to any already-reached step (back or forward).
    shiny::observeEvent(stepper$goto(), {
        g <- as.integer(stepper$goto())
        if (!is.na(g) && g >= 1L && g <= reached_rv()) {
            step_rv(g)
        }
    }, ignoreInit = TRUE)

    invisible(NULL)
}
