#' UI do ranking de preditores (aba "Relações preditoras", F4)
#' @keywords internal
mod_explorar_preditores_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, title = "Preditores de um indicador (ranking de |r|)",
      shiny::fluidRow(
        shiny::column(6, shiny::selectizeInput(
          ns("alvo"), "Indicador alvo (Y)", choices = NULL,
          options = list(placeholder = "Selecione", maxOptions = 200))),
        shiny::column(6, shiny::selectInput(
          ns("leitura"), "Leitura do painel",
          choices = c("Pooled (todas as observações)" = "pooled",
                      "Between (médias por localidade)" = "between",
                      "Within (desvios da localidade)" = "within")))),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(
          ns("sem_degeneradas"), "Excluir séries degeneradas (dp ≈ 0)",
          value = TRUE)),
        shiny::column(6, shiny::checkboxInput(
          ns("esconder_insuficientes"),
          "Esconder linhas com n insuficiente (n < 30)", value = FALSE))),
      shiny::actionButton(ns("calcular"), "Calcular ranking",
                          class = "btn-primary",
                          icon = shiny::icon("list-ol")),
      shiny::helpText("Correlação do alvo com cada um dos demais com n de ",
                      "pares e aviso quando insuficiente; clique numa ",
                      "linha para abrir o par na correlação par a par."),
      DT::DTOutput(ns("tabela"))))
}

#' Server do ranking de preditores: carrega alvo + demais séries sob
#' demanda (com progresso), calcula a matriz na leitura pedida e devolve
#' o par clicado como reactive list(x = mdata_id, y = mdata_id)
#' @keywords internal
mod_explorar_preditores_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogo <- shiny::reactive({
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(painel_mdata(con), error = function(e) NULL)
    })
    shiny::observe({
      md <- catalogo()
      if (is.null(md)) return(NULL)
      rotulos <- setNames(as.character(md$mdata_id), md$rotulo)
      shiny::updateSelectizeInput(session, "alvo", choices = rotulos)
    })
    ranking <- shiny::eventReactive(input$calcular, {
      md <- catalogo()
      if (is.null(md) || !length(input$alvo) || !nzchar(input$alvo))
        return(NULL)
      alvo <- as.integer(input$alvo)
      if (is.na(alvo)) return(NULL)
      ids <- setdiff(md$mdata_id, alvo)
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      origens <- setNames(md$orig_name, as.character(md$mdata_id))
      todos <- c(alvo, ids)
      shiny::withProgress(
        message = "Carregando séries do alvo e demais indicadores...",
        value = 0, {
          series <- list()
          for (i in todos) {
            shiny::incProgress(1 / length(todos),
                               detail = origens[as.character(i)])
            s <- tryCatch(painel_valores_por_ano(con, i),
                          error = function(e) NULL)
            if (!is.null(s) && NROW(s)) series[[as.character(i)]] <- s
          }
        })
      w <- explorar_wide(series)
      if (is.null(w)) return(NULL)
      if (isTRUE(input$sem_degeneradas)) {
        deg <- explorar_degeneradas(w)
        mantem <- c(names(deg)[!deg], as.character(alvo))
        mantem <- intersect(names(w), unique(mantem))
        w <- w[, c("local_id", "ano", mantem), drop = FALSE]
      }
      rk <- explorar_preditores(explorar_matriz(w, input$leitura), alvo)
      rk$rotulo <- md$rotulo[match(as.integer(rk$mdata_id), md$mdata_id)]
      rk
    }, ignoreInit = TRUE)
    output$tabela <- DT::renderDT({
      rk <- ranking()
      if (is.null(rk))
        return(DT::datatable(
          data.frame(Aviso = "Selecione o alvo e clique em Calcular."),
          selection = "none", rownames = FALSE))
      d <- rk
      if (isTRUE(input$esconder_insuficientes))
        d <- d[is.na(d$n) | d$n >= explorar_n_min, , drop = FALSE]
      out <- data.frame(
        Indicador = d$rotulo,
        r = signif(d$r, 4),
        `|r|` = signif(abs(d$r), 4),
        n = d$n,
        Aviso = vapply(d$n, function(nn) {
          a <- explorar_badge_n(nn)
          if (is.na(a)) "" else a
        }, character(1)), check.names = FALSE)
      DT::datatable(
        out, selection = "single", rownames = FALSE,
        options = list(
          pageLength = 15, order = list(2, "desc"),
          language = list(url = paste0("//cdn.datatables.net/plug-ins/",
                                       "1.10.25/i18n/Portuguese-Brasil.json"))))
    })
    par <- shiny::reactiveVal(NULL)
    shiny::observe({
      rk <- ranking()
      s <- input$tabela_rows_selected
      if (is.null(rk) || !NROW(rk) || !length(s)) return(NULL)
      alvo <- suppressWarnings(as.integer(input$alvo))
      outro <- suppressWarnings(as.integer(rk$mdata_id[s[1L]]))
      if (is.na(alvo) || is.na(outro)) return(NULL)
      par(list(x = outro, y = alvo))
    })
    par
  })
}
