# Title: Help Module ("Como usar")
# The embedded tutorial. The trigger lives in the foot of the left rail, so it is
# reachable from any step; clicking it opens a short walkthrough of the flow
# (Enviar → Processar → Resultado → Exportar) in a modal. The content is a
# condensed docs/guia_de_uso.md — that guide stays the canonical long form, this
# is the in-app version. Read-only: opening and closing it does not touch the
# flow's state. UI text is PT-BR (SPEC §2.1).

#' Help module UI — the "Como usar" trigger for the rail.
#'
#' @param id Module id.
#' @return A `shiny::actionButton`.
#' @noRd
mod_help_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::actionButton(
        ns("open"), "Como usar",
        icon = shiny::icon("circle-question"), class = "help-trigger"
    )
}

#' One numbered step of the walkthrough.
#' @noRd
help_step <- function(n, title, ...) {
    shiny::tags$li(
        class = "help-step",
        shiny::tags$span(class = "help-step__n", n),
        shiny::tags$div(
            class = "help-step__body",
            shiny::tags$h3(class = "help-step__title", title),
            ...
        )
    )
}

#' The "Como usar" dialog (pure — condensed from docs/guia_de_uso.md).
#'
#' @return A `shiny::modalDialog`.
#' @noRd
help_modal <- function() {
    shiny::modalDialog(
        title = "Como usar a ferramenta",
        easyClose = TRUE,
        size = "l",
        footer = shiny::modalButton("Fechar"),
        shiny::tags$p(
            class = "help-lede",
            "A ferramenta padroniza uma lista de espécies para Darwin Core, confere ",
            "os nomes na Flora e na Fauna do Brasil (com o GBIF como reforço), cruza ",
            "o status de conservação e verifica a distribuição em volta da sua área ",
            "de operação. O trabalho acontece em quatro passos."
        ),
        shiny::tags$ol(
            class = "help-steps",
            help_step(
                1, "Enviar",
                shiny::tags$p(
                    "Envie a ", shiny::tags$b("planilha de espécies"),
                    " (.xlsx, .csv, .tsv ou .txt) e as ",
                    shiny::tags$b("áreas de estudo"),
                    " — um arquivo por área."
                ),
                shiny::tags$ul(
                    shiny::tags$li(
                        shiny::tags$b("Formatos de área"), " — shapefile em .zip ",
                        "(com .shp, .shx, .dbf e .prj dentro), ou .kmz e .kml, ",
                        "que é o que o Google Earth exporta."
                    ),
                    shiny::tags$li(
                        "A planilha só precisa ter uma coluna ",
                        shiny::tags$code("scientificName"),
                        " — a linha do cabeçalho é detectada automaticamente, e as ",
                        "demais colunas do seu modelo são preservadas na saída."
                    ),
                    shiny::tags$li(
                        shiny::tags$b("Vínculo com locality"), " — com mais de uma área, ",
                        "ligue cada arquivo aos valores de ",
                        shiny::tags$code("locality"), " que ele cobre. Cada registro é ",
                        "verificado contra a área da sua localidade, e só os registros ",
                        "vinculados entram na verificação geográfica. Com uma única área ",
                        "e sem ", shiny::tags$code("locality"),
                        ", ela vale para todos os registros."
                    ),
                    shiny::tags$li(
                        "Linhas já validadas (com ", shiny::tags$code("taxonID"),
                        " e ", shiny::tags$code("kingdom"), " preenchidos) são respeitadas ",
                        "e não voltam para a validação."
                    ),
                    shiny::tags$li(
                        shiny::tags$b("Opções avançadas"), " — cole a sua chave da API da ",
                        "IUCN (opcional) para preencher também a coluna ",
                        shiny::tags$code("criteria"), ". Não é login e a chave não é salva: ",
                        "ela vive só enquanto a aba está aberta."
                    )
                )
            ),
            help_step(
                2, "Processar",
                shiny::tags$p(
                    "Um botão só. Cada nome novo passa, em cascata, pela Flora do Brasil, ",
                    "pela Fauna do Brasil e pelo GBIF; o status de conservação (MMA + IUCN) ",
                    "e as listas nacionais de espécies exóticas invasoras são cruzados; e ",
                    "cada área ganha um buffer de 10 km onde as ocorrências GBIF das suas ",
                    "espécies são buscadas."
                ),
                shiny::tags$p(
                    class = "muted",
                    "Depende de internet, e pode levar alguns segundos por espécie nova."
                )
            ),
            help_step(
                3, "Resultado",
                shiny::tags$p(
                    "Uma tela de conferência, ", shiny::tags$b("somente leitura"),
                    ": cartões de resumo, o mapa (as suas áreas, o buffer de 10 km e os ",
                    shiny::tags$b("registros de ocorrência do GBIF"),
                    ") e a tabela padronizada."
                ),
                shiny::tags$ul(
                    shiny::tags$li(
                        shiny::tags$b("Filtros"), " — área, distribuição, táxon, conservação ",
                        "e status taxonômico. Eles filtram a ",
                        shiny::tags$b("tabela e o mapa "),
                        "ao mesmo tempo, e podem ser combinados. O filtro de ",
                        shiny::tags$b("área"), " também reenquadra o mapa na área escolhida."
                    ),
                    shiny::tags$li(
                        "Clique numa linha para abrir o painel da espécie (taxonomia, ",
                        "conservação, ocorrências) e use ", shiny::tags$b("Ver no mapa"),
                        " para ir até os pontos dela."
                    ),
                    shiny::tags$li(
                        "O badge ", shiny::tags$b("exótica invasora"), " aparece ao lado do ",
                        "nome quando o táxon consta de uma lista nacional — passe o mouse ",
                        "para ver a fonte."
                    )
                )
            ),
            help_step(
                4, "Exportar",
                shiny::tags$p("Dois arquivos Excel:"),
                shiny::tags$ul(
                    shiny::tags$li(
                        shiny::tags$b("Dados Darwin Core"), " — a planilha padronizada, uma ",
                        "linha por registro, pronta para envio."
                    ),
                    shiny::tags$li(
                        shiny::tags$b("Relatório de auditoria"), " — a memória das decisões, ",
                        "em duas abas: ", shiny::tags$code("auditoria"), " (o que foi decidido ",
                        "para cada nome, e por quê) e ", shiny::tags$code("nao_resolvidos"),
                        " (os nomes que ficaram para revisão manual)."
                    )
                )
            )
        ),
        shiny::tags$p(
            class = "help-note",
            shiny::icon("circle-info"),
            shiny::tags$span(
                shiny::tags$b("O distributionFlag é um alerta, não um veredito."),
                " Um \"sem registro no estado/bioma\" não quer dizer que a espécie está ",
                "errada — é um convite a olhar com mais atenção. O mesmo vale para o ",
                "badge de espécie exótica invasora: ele diz que o táxon consta de uma ",
                "lista nacional, não que aquele registro seja, ali, um problema."
            )
        ),
        bases_versions_block("Bases de referência em uso")
    )
}

#' Help module server — opens the walkthrough.
#'
#' @param id Module id.
#' @return Invisibly NULL.
#' @noRd
mod_help_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        shiny::observeEvent(input$open, {
            shiny::showModal(help_modal())
        }, ignoreInit = TRUE)

        invisible(NULL)
    })
}
