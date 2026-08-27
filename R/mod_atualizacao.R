#' modulo_atualizacao UI Functions
#'
#' "Painel de Atualização" (fase C4 do roadmap_aedi_agendamento.md): status de
#' cada script de coleta (controle_execucao no aedidb), verificação de
#' desatualização por indicador (max refdate no DW) e execução manual por
#' script em processo background (callr).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_atualizacao_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Controle de atualização dos indicadores", width = 12,
        solidHeader = TRUE, status = "primary", collapsible = FALSE,
        shiny::actionButton(ns("atualizar_status"), "Recarregar status",
                            icon = shiny::icon("refresh")),
        shiny::actionButton(ns("atualizar_todos"),
                            "Atualizar todos os scripts",
                            icon = shiny::icon("play")),
        shiny::helpText("Execuções ficam registradas em controle_execucao/",
                        "controle_execucao_historico no banco aedidb. ",
                        "Scripts ignorados via coleta/<script>.R.ignore."),
        DT::DTOutput(ns("tabela_controle")),
        shiny::uiOutput(ns("processo_atual"))
      )
    )
  )
}

#' modulo_atualizacao Server Functions
#'
#' @noRd
mod_atualizacao_server <- function(id, raiz = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(raiz)) raiz <- getwd()

    rv <- shiny::reactiveValues(
      processo = NULL,          # processo callr em andamento
      nome = character(0),
      inicio = NULL
    )

    status_df <- function() {
      ctl <- tryCatch(AEDi:::ler_controle(), error = function(e) NULL)
      scripts <- AEDi:::listar_scripts_coleta(raiz)
      if (is.null(ctl)) ctl <- data.frame(
        nome_script = character(), etapa = character(),
        ultima_atualizacao = structure(list(), class = c("POSIXct", "POSIXt")),
        status = character(), detalhe = character())
      d <- data.frame(nome_script = scripts,
                      stringsAsFactors = FALSE)
      d <- merge(d, ctl[, c("nome_script", "etapa", "ultima_atualizacao",
                            "status", "detalhe")],
                 by = "nome_script", all.x = TRUE)
      d$status[is.na(d$status)] <- "nunca executado"
      d$ultima_atualizacao <- format(d$ultima_atualizacao, "%Y-%m-%d %H:%M")
      d$ultima_atualizacao[is.na(d$ultima_atualizacao)] <- "-"
      d <- d[order(d$nome_script), c("nome_script", "etapa",
                                     "ultima_atualizacao", "status",
                                     "detalhe")]
      rownames(d) <- NULL
      d
    }

    output$tabela_controle <- DT::renderDT({
      input$atualizar_status
      DT::datatable(
        status_df(), escape = FALSE, selection = "single",
        options = list(pageLength = 15, language = list(url =
          "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")),
        rownames = FALSE)
    })

    output$processo_atual <- shiny::renderUI({
      if (is.null(rv$processo)) return(NULL)
      shiny::tags$p(
        style = "margin-top:8px;",
        sprintf("Executando '%s' desde %s (PID %s)... ",
                rv$nome, format(rv$inicio, "%H:%M:%S"), rv$processo$get_pid()),
        shinybusy::spin_dots()
      )
    })

    # poll do processo em andamento
    shiny::observe({
      shiny::invalidateLater(4000, session)
      if (is.null(rv$processo)) return()
      if (rv$processo$is_alive()) return()
      res <- tryCatch(rv$processo$get_result(), error = function(e) NULL)
      rv$processo <- NULL
      shiny::showNotification(
        sprintf("Execução de '%s' concluída.", rv$nome),
        type = "message")
    })

    shiny::observeEvent(input$atualizar_todos, {
      if (!is.null(rv$processo)) {
        shiny::showNotification("Já existe uma execução em andamento.",
                                type = "warning"); return()
      }
      rv$nome <- "(todos)"
      rv$inicio <- Sys.time()
      rv$processo <- callr::r_bg(
        function(raiz) {
          pkgload::load_all("/home/wlvdbaj/pRojetos/AEDi")
          AEDi:::atualizar_indicadores()
        }, args = list(raiz = raiz))
    })
  })
}

## To be copied in the UI
# mod_atualizacao_ui("atualizacao_1")

## To be copied in the server
# mod_atualizacao_server("atualizacao_1")
