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
                        "Scripts ignorados via coleta/<script>.R.ignore. ",
                        "Scripts nunca executados destacam-se em vermelho quando ",
                        "desatualizados: hoje além da última versão dos ",
                        "metadados em BD ou 3 meses após o max(refdate)."),
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
      inicio = NULL,
      versao = 0L               # contador para recarregar a tabela
    )

    # tabela de status compartilhada com o painel admin (admin_app.R):
    # fonte unica da regra de "desatualizado"
    status_df <- function() {
      .tabela_status_lote(raiz, projeto = AEDi:::.nome_projeto(raiz),
                          com_acao = TRUE, id_acao = ns("atualizar_um"))
    }

    output$tabela_controle <- DT::renderDT({
      input$atualizar_status
      rv$versao
      DT::datatable(
        status_df(), escape = FALSE, selection = "single",
        options = list(pageLength = 15, language = list(url =
          "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")),
        rownames = FALSE) |>
        DT::formatStyle(
          "Situação", target = "row",
          backgroundColor = DT::styleEqual(
            c("nunca executado (desatualizado)", "nunca executado"),
            c("#f5c6cb", "#fff3cd")))
    })

    output$processo_atual <- shiny::renderUI({
      if (is.null(rv$processo)) return(NULL)
      shiny::tags$p(
        style = "margin-top:8px;",
        sprintf("Executando '%s' desde %s (PID %s)... ",
                rv$nome, format(rv$inicio, "%H:%M:%S"), rv$processo$get_pid()),
        shiny::icon("spinner", "fa-spin")
      )
    })

    # poll do processo em andamento
    shiny::observe({
      shiny::invalidateLater(4000, session)
      if (is.null(rv$processo)) return()
      if (rv$processo$is_alive()) return()
      res <- tryCatch(rv$processo$get_result(), error = function(e) NULL)
      rv$processo <- NULL
      rv$versao <- rv$versao + 1L
      shiny::showNotification(
        sprintf("Execução de '%s' concluída.", rv$nome),
        type = "message")
    })

    # atualiza um unico indicador em background (sem snapshot: pg_dump fica
    # para o ciclo completo de "Atualizar todos")
    shiny::observeEvent(input$atualizar_um, {
      nome <- input$atualizar_um
      if (!length(nome) || !nzchar(nome)) return()
      if (!is.null(rv$processo)) {
        shiny::showNotification("Já existe uma execução em andamento.",
                                type = "warning"); return()
      }
      if (!file.exists(file.path(raiz, "coleta", paste0(nome, ".R")))) {
        shiny::showNotification(
          sprintf("Script '%s' não encontrado em coleta/.", nome),
          type = "error"); return()
      }
      rv$nome <- nome
      rv$inicio <- Sys.time()
      rv$processo <- callr::r_bg(
        function(raiz, nome) {
          setwd(raiz)
          AEDi::atualizar_indicadores(apenas = nome, snapshot = FALSE)
        }, args = list(raiz = raiz, nome = nome))
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
          setwd(raiz)
          AEDi::atualizar_indicadores()
        }, args = list(raiz = raiz))
    })
  })
}

## To be copied in the UI
# mod_atualizacao_ui("atualizacao_1")

## To be copied in the server
# mod_atualizacao_server("atualizacao_1")
