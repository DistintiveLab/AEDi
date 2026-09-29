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
      shiny::sliderInput(ns("anos"), "Janela de anos", min = 0, max = 1,
                         value = c(0, 1), step = 1),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("log_x"), "Escala log em X")),
        shiny::column(6, shiny::checkboxInput(ns("log_y"), "Escala log em Y"))),
      shiny::verbatimTextOutput(ns("stats")),
      plotly::plotlyOutput(ns("scatter"))))
}

#' Server da correlacao: pares completos alinhados por (localidade, ano)
#' com NAs removidos por pares; Pearson e Spearman com n de pares
#' @keywords internal
mod_explorar_bivar_server <- function(id) {
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
    pares <- shiny::reactive({
      p <- explorar_pares(serie_x(), serie_y())
      if (!NROW(p)) return(p)
      janela <- input$anos
      p[p$ano >= janela[1] & p$ano <= janela[2], , drop = FALSE]
    })
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
    shiny::observe({
      a <- anos_disp()
      if (length(a) < 2) return(NULL)
      shiny::updateSliderInput(session, "anos", min = min(a), max = max(a),
                               value = c(min(a), max(a)))
    })
    output$stats <- shiny::renderText({
      if (!NROW(pares()) && !length(input$x))
        return("Selecione dois indicadores.")
      p <- pares()
      if (is.null(p) || !NROW(p))
        return("Sem pares completos (séries sem sobreposição de localidade/ano).")
      c <- explorar_cor(p)
      rot <- catalogo()
      rotula <- function(sel)
        if (!is.null(rot) && length(sel))
          rot$rotulo[match(as.integer(sel), rot$mdata_id)][1] else "?"
      base <- sprintf(paste0("X: %s\nY: %s\npares completos: %d | ",
                             "anos: %d-%d\nPearson r = %.4f | ",
                             "Spearman rho = %.4f"),
                      rotula(input$x), rotula(input$y), c$n,
                      min(p$ano), max(p$ano), c$pearson, c$spearman)
      if (!is.na(c$aviso))
        paste0(base, "\naviso: ", c$aviso,
               " (valores exibidos apenas para referência)")
      else base
    })
    output$scatter <- plotly::renderPlotly({
      p <- pares()
      if (is.null(p) || !NROW(p)) return(NULL)
      nm <- nomes()
      p$local <- if (!is.null(nm))
        unname(nm[as.character(p$local_id)]) else as.character(p$local_id)
      g <- ggplot2::ggplot(p, ggplot2::aes(.data$x, .data$y,
                                          text = paste0(.data$local, "\n",
                                                        .data$ano))) +
        ggplot2::geom_point(alpha = 0.45, color = "#1351B4", na.rm = TRUE) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#4A4A4A",
                             formula = y ~ x, na.rm = TRUE) +
        ggplot2::labs(x = "X", y = "Y", title = "Pares completos por (localidade, ano)")
      if (isTRUE(input$log_x)) g <- g + ggplot2::scale_x_log10()
      if (isTRUE(input$log_y)) g <- g + ggplot2::scale_y_log10()
      plotly::ggplotly(g, tooltip = c("x", "y", "text")) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
