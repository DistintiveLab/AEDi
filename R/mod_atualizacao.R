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

    # "matiza" scripts nunca executados: desatualizado quando hoje supera a
    # última versão dos metadados em BD (mdata_timetable.last_update) OU
    # 3 meses após o max(refdate) gravado em data_values (regra OU = pmax)
    situacao_nunca <- function(meta_update, max_refdate) {
      prazo <- lubridate::`%m+%`(as.Date(max_refdate),
                                 lubridate::period(3, "months"))
      limite <- as.Date(meta_update)
      ambos <- !is.na(prazo) & !is.na(limite)
      limite[ambos] <- pmax(limite[ambos], prazo[ambos])
      so_prazo <- is.na(limite) & !is.na(prazo)
      limite[so_prazo] <- prazo[so_prazo]
      desat <- !is.na(limite) & Sys.Date() > limite
      ifelse(desat, "nunca executado (desatualizado)", "nunca executado")
    }

    status_df <- function() {
      ctl <- tryCatch(AEDi:::ler_controle(), error = function(e) NULL)
      dw <- tryCatch(AEDi:::resumo_indicadores_dw(), error = function(e) NULL)
      scripts <- AEDi:::listar_scripts_coleta(raiz)
      if (is.null(ctl)) ctl <- data.frame(
        nome_script = character(), etapa = character(),
        ultima_atualizacao = structure(list(), class = c("POSIXct", "POSIXt")),
        status = character(), detalhe = character())
      # nomes sem extensao (convencao do controle_execucao; o merge com
      # scripts ".R" jamais casaria)
      d <- data.frame(nome_script = sub("\\.R$", "", scripts,
                                        ignore.case = TRUE),
                      stringsAsFactors = FALSE)
      d <- merge(d, ctl[, c("nome_script", "etapa", "ultima_atualizacao",
                            "status", "detalhe")],
                 by = "nome_script", all.x = TRUE)
      if (!is.null(dw) && nrow(dw)) {
        d <- merge(d, dw, by.x = "nome_script", by.y = "orig_name",
                   all.x = TRUE)
      } else {
        d$meta_update <- as.Date(NA)
        d$max_refdate <- as.Date(NA)
      }
      d$meta_update <- as.Date(d$meta_update)
      d$max_refdate <- as.Date(d$max_refdate)
      d$status[is.na(d$status)] <- "nunca executado"
      d$situacao <- d$status
      nunca <- d$status == "nunca executado"
      d$situacao[nunca] <- situacao_nunca(d$meta_update[nunca],
                                          d$max_refdate[nunca])
      atrasado <- d$situacao == "nunca executado (desatualizado)"
      fmt_data <- function(x) {
        x <- format(x, "%Y-%m-%d"); x[is.na(x)] <- "-"; x
      }
      d$meta_update <- fmt_data(d$meta_update)
      d$max_refdate <- fmt_data(d$max_refdate)
      d$ultima_atualizacao <- format(d$ultima_atualizacao, "%Y-%m-%d %H:%M")
      d$ultima_atualizacao[is.na(d$ultima_atualizacao)] <- "-"
      d$acao <- sprintf(
        paste0('<button class="btn btn-default btn-xs action-button" ',
               'data-script="%s" onclick="Shiny.setInputValue(\'%s\', ',
               'this.dataset.script, {priority: \'event\'})">',
               'Atualizar</button>'),
        d$nome_script, ns("atualizar_um"))
      d <- d[order(!atrasado, d$nome_script),
             c("nome_script", "etapa", "ultima_atualizacao", "meta_update",
               "max_refdate", "situacao", "detalhe", "acao")]
      colnames(d) <- c("Script", "Etapa", "Última execução",
                       "Metadados (BD)", "Máx. refdate", "Situação",
                       "Detalhe", "Ação")
      rownames(d) <- NULL
      d
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
