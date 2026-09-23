# Painel admin (somente leitura) do lote de um projeto ----------------------
#
# Materializa como app Shiny autonoma o que hoje so existe dentro do app
# construtor (mod_atualizacao): status do lote, dependencias declaradas,
# frescor das series e historico de execucoes. Somente leitura por design:
# execucao continua sendo via cron/AEDi (ver roadmap_dependencias_
# orquestrador.md, "deploy do painel admin por projeto").

# Situacao dos scripts nunca executados: desatualizado quando hoje supera a
# ultima versao dos metadados em BD (mdata_timetable.last_update) OU 3 meses
# apos o max(refdate) gravado em data_values (regra OU = pmax)
.situacao_nunca_executado <- function(meta_update, max_refdate) {
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

# Tabela de status do lote: scripts ativos de <raiz>/coleta + controle do
# aedidb + frescor do DW. Fonte unica da regra de "desatualizado" (usada
# pelo mod_atualizacao e pelo painel admin). com_acao = TRUE acrescenta o
# botao por linha (id_acao = id do input Shiny que recebe o nome do script).
.tabela_status_lote <- function(raiz, projeto, com_acao = FALSE,
                                id_acao = NULL) {
  ctl <- tryCatch(ler_controle(projeto = projeto), error = function(e) NULL)
  dw <- tryCatch(resumo_indicadores_dw(), error = function(e) NULL)
  scripts <- listar_scripts_coleta(raiz)
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
  d$situacao[nunca] <- .situacao_nunca_executado(
    d$meta_update[nunca], d$max_refdate[nunca])
  atrasado <- d$situacao == "nunca executado (desatualizado)"
  fmt_data <- function(x) {
    x <- format(x, "%Y-%m-%d"); x[is.na(x)] <- "-"; x
  }
  d$meta_update <- fmt_data(d$meta_update)
  d$max_refdate <- fmt_data(d$max_refdate)
  d$ultima_atualizacao <- format(d$ultima_atualizacao, "%Y-%m-%d %H:%M")
  d$ultima_atualizacao[is.na(d$ultima_atualizacao)] <- "-"
  if (com_acao) {
    d$acao <- sprintf(
      paste0('<button class="btn btn-default btn-xs action-button" ',
             'data-script="%s" onclick="Shiny.setInputValue(\'%s\', ',
             'this.dataset.script, {priority: \'event\'})">',
             'Atualizar</button>'),
      d$nome_script, id_acao)
  }
  d <- d[order(!atrasado, d$nome_script),
         c("nome_script", "etapa", "ultima_atualizacao", "meta_update",
           "max_refdate", "situacao", "detalhe",
           if (com_acao) "acao")]
  colnames(d) <- c("Script", "Etapa", "Última execução",
                   "Metadados (BD)", "Máx. refdate", "Situação",
                   "Detalhe", if (com_acao) "Ação")
  rownames(d) <- NULL
  d
}

# Historico recente de execucoes do projeto (controle_execucao_historico)
.dados_historico_lote <- function(projeto, con = NULL) {
  if (is.null(con)) { con <- controle_con(); on.exit(DBI::dbDisconnect(con)) }
  DBI::dbGetQuery(con, "
    SELECT nome_script, inicio, fim, sucesso, mensagem, linhas
      FROM controle_execucao_historico
     WHERE projeto = $1
     ORDER BY inicio DESC
     LIMIT 500", params = list(projeto))
}

# Grafo de dependencias do projeto: linhas com dependencias_json no DW,
# com fallback para o CSV semente (coleta/dependencias.csv) por script sem
# grafo no DW. Retorna data.frame plano Script x Dependencia.
.dados_dependencias_lote <- function(projeto, raiz, con = NULL) {
  vazio <- data.frame(script = character(), serie = character(),
                      regua = character(), acao = character(),
                      series_proprias = character())
  if (is.null(con)) { con <- controle_con(); on.exit(DBI::dbDisconnect(con)) }
  linhas <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT nome_script, dependencias_json
        FROM controle_execucao
       WHERE projeto = $1 AND dependencias_json IS NOT NULL",
      params = list(projeto)),
    error = function(e) NULL)
  grafo <- list()
  if (!is.null(linhas) && nrow(linhas)) {
    for (i in seq_len(nrow(linhas))) {
      reg <- tryCatch(
        .normalizar_registro(
          jsonlite::fromJSON(linhas$dependencias_json[[i]])),
        error = function(e) NULL)
      if (!is.null(reg)) grafo[[linhas$nome_script[[i]]]] <- reg
    }
  }
  # CSV semente cobre scripts sem grafo no DW
  manifesto <- tryCatch(ler_dependencias(raiz), error = function(e) NULL)
  if (!is.null(manifesto)) {
    faltantes <- setdiff(unique(manifesto$script), names(grafo))
    for (nm in faltantes) {
      linhas_script <- manifesto[manifesto$script == nm, , drop = FALSE]
      grafo[[nm]] <- list(
        deps = data.frame(
          serie = linhas_script$dep_orig_name,
          regua = linhas_script$regua,
          acao = linhas_script$acao),
        series_proprias = .series_proprias_manifesto(linhas_script))
    }
  }
  if (!length(grafo)) return(vazio)
  do.call(rbind, lapply(names(grafo), function(nm) {
    g <- grafo[[nm]]
    propias <- if (length(g$series_proprias))
      paste(g$series_proprias, collapse = "; ") else "-"
    if (!nrow(g$deps))
      return(data.frame(script = nm, serie = "-", regua = "-", acao = "-",
                        series_proprias = propias))
    data.frame(script = nm, serie = g$deps$serie, regua = g$deps$regua,
               acao = g$deps$acao, series_proprias = propias)
  }))
}

# DT com idioma/estilo padrao do AEDi (mesmo visual do mod_atualizacao)
.admin_datatable <- function(d, estilo_situacao = FALSE) {
  tab <- DT::datatable(
    d, escape = FALSE, selection = "none", rownames = FALSE,
    options = list(pageLength = 15, language = list(url =
      "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")))
  if (estilo_situacao)
    tab <- DT::formatStyle(
      tab, "Situação", target = "row",
      backgroundColor = DT::styleEqual(
        c("nunca executado (desatualizado)", "nunca executado"),
        c("#f5c6cb", "#fff3cd")))
  tab
}

#' Painel admin (somente leitura) do lote de um projeto
#'
#' App Shiny autonoma de monitoramento do orquestrador para o projeto
#' chamador: status de cada script de coleta (com o motivo pelo qual foi
#' pulado, inclusive dependencias), grafo de dependencias declarado, frescor
#' das series no banco e historico das execucoes. Somente leitura por
#' design: disparar execucoes continua sendo via cron ou app AEDi (o
#' gatilho manual exige lock single-flight, ver
#' `roadmap_dependencias_orquestrador.md`).
#'
#' Companheira de [deploy_admin()], que materializa uma launcher desta app
#' na raiz do projeto. Conexao: banco aedidb via variaveis de ambiente
#' (`user`, `password`, `host`, `dbname`), as mesmas do orquestrador.
#'
#' @param raiz raiz do projeto orquestrado (default: diretorio corrente);
#'   define o lote (`coleta/`) e o projeto no controle (`basename`)
#' @param projeto nome do projeto no controle_execucao (default:
#'   `.nome_projeto(raiz)`); informar explicitamente para monitorar um
#'   lote hospedado em outro caminho
#' @param titulo titulo da janela/aba
#'
#' @return objeto `shiny_app` (usar via [deploy_admin()] ou
#'   `shiny::runApp()`)
#' @export
admin_app <- function(raiz = NULL, projeto = NULL, titulo = NULL) {
  if (is.null(raiz)) raiz <- getwd()
  raiz <- normalizePath(raiz, mustWork = FALSE)
  if (is.null(projeto)) projeto <- .nome_projeto(raiz)
  if (is.null(titulo))
    titulo <- sprintf("Admin do lote - %s", projeto)
  versao <- as.character(utils::packageVersion("AEDi"))

  ui <- shiny::navbarPage(
    titulo, id = "admin_nav", windowTitle = titulo, collapsible = TRUE,
    lang = "pt-BR",
    shiny::tabPanel(
      "Status do lote",
      shiny::fluidRow(
        shiny::column(12,
          shiny::actionButton("recarregar", "Recarregar",
                              icon = shiny::icon("refresh")),
          shiny::helpText("Somente leitura: execuções seguem via cron ou ",
                          "app AEDi. Scripts ignorados via ",
                          "coleta/<script>.R.ignore; nunca executados em ",
                          "destaque quando desatualizados (metadados em BD ",
                          "ou 3 meses após o máx. refdate). Detalhe traz o ",
                          "motivo do pulo (ex.: dependência desatualizada)."),
          shiny::verbatimTextOutput("admin_cabecalho"),
          DT::DTOutput("tabela_status")))),
    shiny::tabPanel(
      "Dependências",
      shiny::fluidRow(shiny::column(12,
        shiny::helpText("Grafo declarado (dependencias_json no controle; ",
                        "CSV coleta/dependencias.csv como semente e ",
                        "fallback). Ação 'pular' adia a execução quando o ",
                        "insumo está menos fresco (por ano) que a série ",
                        "própria; 'avisar' apenas registra."),
        DT::DTOutput("tabela_deps")))),
    shiny::tabPanel(
      "Frescor das séries",
      shiny::fluidRow(shiny::column(12,
        shiny::helpText("Máx. refdate e última atualização de metadados ",
                        "por série no banco de dados do painel; ordenado ",
                        "da mais atrasada para a mais fresca."),
        DT::DTOutput("tabela_frescor")))),
    shiny::tabPanel(
      "Histórico",
      shiny::fluidRow(shiny::column(12,
        shiny::helpText("Últimas 500 execuções registradas em ",
                        "controle_execucao_historico."),
        DT::DTOutput("tabela_historico")))))

  server <- function(input, output, session) {
    dados <- shiny::reactive({
      input$recarregar
      shiny::invalidateLater(60000, session)
      list(
        status = tryCatch(.tabela_status_lote(raiz, projeto), error = function(e) NULL),
        deps = tryCatch(.dados_dependencias_lote(projeto, raiz), error = function(e) NULL),
        frescor = tryCatch(resumo_indicadores_dw(), error = function(e) NULL),
        historico = tryCatch(.dados_historico_lote(projeto), error = function(e) NULL),
        consulta = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    })
    .erro_df <- function(msg) data.frame(Aviso = msg)

    output$admin_cabecalho <- shiny::renderText({
      sprintf("projeto: %s | lote: %s | AEDi %s | consulta: %s",
              projeto, raiz, versao, dados()$consulta)
    })
    output$tabela_status <- DT::renderDT({
      d <- dados()$status
      if (is.null(d)) return(.admin_datatable(.erro_df("Sem acesso ao banco de dados do controle.")))
      .admin_datatable(d, estilo_situacao = TRUE)
    })
    output$tabela_deps <- DT::renderDT({
      d <- dados()$deps
      if (is.null(d) || !nrow(d))
        return(.admin_datatable(.erro_df("Nenhuma dependência declarada (sem grafo no DW e sem coleta/dependencias.csv).")))
      colnames(d) <- c("Script", "Dependência", "Régua", "Ação", "Séries próprias")
      .admin_datatable(d)
    })
    output$tabela_frescor <- DT::renderDT({
      d <- dados()$frescor
      if (is.null(d)) return(.admin_datatable(.erro_df("Sem acesso ao banco de dados do painel.")))
      d$meta_update <- format(as.Date(d$meta_update), "%Y-%m-%d")
      d$max_refdate <- format(as.Date(d$max_refdate), "%Y-%m-%d")
      d <- d[order(d$max_refdate), c("orig_name", "max_refdate", "meta_update")]
      colnames(d) <- c("Série", "Máx. refdate", "Metadados (BD)")
      .admin_datatable(d)
    })
    output$tabela_historico <- DT::renderDT({
      d <- dados()$historico
      if (is.null(d) || !nrow(d))
        return(.admin_datatable(.erro_df("Sem execuções registradas para este projeto.")))
      d$inicio <- format(d$inicio, "%Y-%m-%d %H:%M")
      d$fim <- format(d$fim, "%Y-%m-%d %H:%M")
      d$sucesso <- ifelse(d$sucesso, "sim", "não")
      d$linhas <- ifelse(is.na(d$linhas), "-", as.character(d$linhas))
      d <- d[, c("nome_script", "inicio", "fim", "sucesso", "linhas", "mensagem")]
      colnames(d) <- c("Script", "Início", "Fim", "Sucesso", "Linhas", "Mensagem")
      .admin_datatable(d)
    })
  }
  shiny::shinyApp(ui, server)
}
