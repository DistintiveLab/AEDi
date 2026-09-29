#' UI da correlacao par a par (aba "Análise Bi-Variada")
#' @keywords internal
mod_explorar_bivar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, title = "Correlação par a par",
      shiny::fluidRow(
        shiny::column(6, shiny::selectizeInput(
          ns("x"), "Indicador X", choices = NULL,
          options = list(placeholder = "Selecione", maxOptions = 200))),
        shiny::column(6, shiny::selectizeInput(
          ns("y"), "Indicador Y", choices = NULL,
          options = list(placeholder = "Selecione", maxOptions = 200)))),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(
          ns("preset"), "Janela de anos",
          choices = c("Sobreposição completa" = "sobreposicao",
                      "Últimos 10 anos (sugerido)" = "dez",
                      "Personalizada (slider)" = "personalizada"))),
        shiny::column(6, shiny::selectInput(
          ns("leitura"), "Leitura do painel",
          choices = c("Pooled (todas as observações)" = "pooled",
                      "Between (médias por localidade)" = "between",
                      "Within (desvios da localidade)" = "within")))),
      shiny::sliderInput(ns("anos"), "Recorte de anos", min = 0, max = 1,
                         value = c(0, 1), step = 1),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("log_x"), "Escala log em X")),
        shiny::column(6, shiny::checkboxInput(ns("log_y"), "Escala log em Y"))),
      shiny::uiOutput(ns("badge")),
      shiny::verbatimTextOutput(ns("stats")),
      plotly::plotlyOutput(ns("scatter"))))
}

#' Server da correlacao: pares completos alinhados por (localidade, ano)
#' com NAs removidos por pares; Pearson e Spearman com n de pares sempre
#' visível (badge quando insuficiente), leitura pooled/between/within e
#' janela com preset sugerido de últimos 10 anos. `par_inicial` é um
#' reactive que recebe list(x = mdata_id, y = mdata_id) vindo da matriz
#' ou do ranking de preditores
#' @keywords internal
mod_explorar_bivar_server <- function(id, par_inicial = NULL) {
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
      shiny::updateSelectizeInput(session, "x", choices = rotulos)
      shiny::updateSelectizeInput(session, "y", choices = rotulos)
    })
    if (!is.null(par_inicial)) {
      shiny::observeEvent(par_inicial(), {
        p <- par_inicial()
        if (is.null(p) || is.null(p$x) || is.null(p$y)) return(NULL)
        md <- catalogo()
        if (is.null(md)) return(NULL)
        rotulos <- setNames(as.character(md$mdata_id), md$rotulo)
        shiny::updateSelectizeInput(session, "x", choices = rotulos,
                                    selected = as.character(p$x))
        shiny::updateSelectizeInput(session, "y", choices = rotulos,
                                    selected = as.character(p$y))
      })
    }
    pega_serie <- function(selecao) {
      if (!length(selecao) || !nzchar(selecao)) return(NULL)
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(painel_valores_por_ano(con, as.integer(selecao)),
               error = function(e) NULL)
    }
    serie_x <- shiny::reactive(pega_serie(input$x))
    serie_y <- shiny::reactive(pega_serie(input$y))
    nomes <- shiny::reactive({
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(explorar_nomes_locais(con), error = function(e) NULL)
    })
    anos_disp <- shiny::reactive({
      sx <- serie_x(); sy <- serie_y()
      if (is.null(sx) || is.null(sy)) return(integer(0))
      a <- intersect(unique(sx$ano), unique(sy$ano))
      if (!length(a)) return(integer(0))
      sort(a)
    })
    janela_estado <- shiny::reactiveValues(ultima = NULL)
    aplicar_preset <- function(preset, anos) {
      if (length(anos) < 2L) return(NULL)
      valor <- if (identical(preset, "dez"))
        c(max(min(anos), max(anos) - 9L), max(anos)) else
        c(min(anos), max(anos))
      janela_estado$ultima <- valor
      shiny::updateSliderInput(session, "anos", min = min(anos),
                               max = max(anos), value = valor)
    }
    shiny::observeEvent(input$preset, {
      if (identical(input$preset, "personalizada")) return(NULL)
      aplicar_preset(input$preset, anos_disp())
    })
    shiny::observeEvent(anos_disp(), {
      aplicar_preset(input$preset, anos_disp())
    })
    shiny::observe({
      if (is.null(input$anos) || is.null(janela_estado$ultima)) return(NULL)
      if (!identical(as.integer(input$anos), janela_estado$ultima) &&
          !identical(input$preset, "personalizada")) {
        janela_estado$ultima <- as.integer(input$anos)
        shiny::updateSelectInput(session, "preset",
                                 selected = "personalizada")
      }
    })
    pares <- shiny::reactive({
      p <- explorar_pares(serie_x(), serie_y())
      if (!NROW(p)) return(p)
      janela <- input$anos
      p[p$ano >= janela[1] & p$ano <= janela[2], , drop = FALSE]
    })
    pares_leitura <- shiny::reactive(explorar_leituras(pares(),
                                                       input$leitura))
    cor_leitura <- shiny::reactive({
      pl <- pares_leitura()
      if (is.null(pl) || !NROW(pl))
        return(data.frame(n = 0L, pearson = NA_real_,
                          spearman = NA_real_,
                          aviso = "sem pares completos"))
      explorar_cor(pl)
    })
    output$badge <- shiny::renderUI({
      c <- cor_leitura()
      aviso <- if (!is.na(c$aviso)) c$aviso else explorar_badge_n(c$n)
      if (is.na(aviso)) return(NULL)
      fundo <- if (c$n < 3L) "#D6140A" else "#E5A700"
      shiny::tags$span(aviso, style = sprintf(
        "display:inline-block;background:%s;color:#111;border-radius:4px;",
        fundo))
    })
    output$stats <- shiny::renderText({
      if (!NROW(pares()) && !length(input$x))
        return("Selecione dois indicadores.")
      p <- pares()
      if (is.null(p) || !NROW(p))
        return("Sem pares completos (séries sem sobreposição de localidade/ano).")
      rot <- catalogo()
      rotula <- function(sel)
        if (!is.null(rot) && length(sel))
          rot$rotulo[match(as.integer(sel), rot$mdata_id)][1] else "?"
      c_pooled <- explorar_cor(p)
      c <- cor_leitura()
      nome_leitura <- switch(input$leitura,
        between = "between (médias por localidade)",
        within = "within (desvios da localidade)", "pooled")
      base <- sprintf("X: %s\nY: %s\nanos: %d-%d",
                      rotula(input$x), rotula(input$y),
                      min(p$ano), max(p$ano))
      if (identical(input$leitura, "pooled")) {
        base <- paste0(base, sprintf("\npares completos: %d", c_pooled$n))
      } else {
        base <- paste0(base, sprintf(
          "\npooled: n = %d, r = %.4f, rho = %.4f\n%s: n = %d",
          c_pooled$n, c_pooled$pearson, c_pooled$spearman,
          nome_leitura, c$n))
      }
      base <- paste0(base, sprintf("\nPearson r = %.4f | Spearman rho = %.4f",
                                   c$pearson, c$spearman))
      if (!is.na(c$aviso))
        base <- paste0(base, "\naviso: ", c$aviso,
                       " (valores exibidos apenas para referência)")
      base
    })
    output$scatter <- plotly::renderPlotly({
      p <- pares_leitura()
      if (is.null(p) || !NROW(p)) return(NULL)
      nm <- nomes()
      p$local <- if (!is.null(nm))
        unname(nm[as.character(p$local_id)]) else as.character(p$local_id)
      p$txt <- if (all(is.na(p$ano))) p$local else
        paste0(p$local, "\n", p$ano)
      titulo <- switch(input$leitura,
        between = "Between: médias por localidade",
        within = "Within: desvios da localidade",
        "Pares completos por (localidade, ano)")
      g <- ggplot2::ggplot(p, ggplot2::aes(.data$x, .data$y,
                                          text = .data$txt)) +
        ggplot2::geom_point(alpha = 0.45, color = "#1351B4", na.rm = TRUE) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#4A4A4A",
                             formula = y ~ x, na.rm = TRUE) +
        ggplot2::labs(x = "X", y = "Y", title = titulo)
      if (isTRUE(input$log_x)) g <- g + ggplot2::scale_x_log10()
      if (isTRUE(input$log_y)) g <- g + ggplot2::scale_y_log10()
      plotly::ggplotly(g, tooltip = c("x", "y", "text")) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
