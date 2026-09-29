#' UI das distribuicoes do indicador (aba "Distribuições")
#' @keywords internal
mod_explorar_dist_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, title = "Distribuições",
      shiny::selectizeInput(
        ns("indicador"), "Indicador", choices = NULL,
        options = list(placeholder = "Selecione um indicador",
                       maxOptions = 200)),
      shiny::selectInput(ns("ano"), "Ano (histograma)", choices = NULL),
      shiny::checkboxInput(ns("log"), "Escala logarítmica (caudas pesadas)",
                           value = FALSE),
      plotly::plotlyOutput(ns("hist")),
      plotly::plotlyOutput(ns("box"))))
}

#' Server das distribuicoes: histograma do ano escolhido e boxplot da
#' serie toda por ano, sobre a serie achatada
#' @keywords internal
mod_explorar_dist_server <- function(id) {
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
      shiny::updateSelectizeInput(
        session, "indicador", choices = setNames(md$mdata_id, md$rotulo))
    })
    serie <- shiny::reactive({
      if (!length(input$indicador) || !nzchar(input$indicador))
        return(NULL)
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(painel_valores_por_ano(con, as.integer(input$indicador)),
               error = function(e) NULL)
    })
    anos <- shiny::reactive({
      s <- serie()
      if (is.null(s)) return(integer(0))
      sort(unique(s$ano[is.finite(s$value)]))
    })
    shiny::observe({
      a <- anos()
      if (!length(a)) return(NULL)
      shiny::updateSelectInput(session, "ano", choices = as.character(a),
                               selected = as.character(max(a)))
    })
    eixo <- function(p) {
      if (isTRUE(input$log))
        p + ggplot2::scale_y_log10() else p
    }
    output$hist <- plotly::renderPlotly({
      shiny::req(input$ano)
      s <- serie()
      if (is.null(s)) return(NULL)
      d <- s[s$ano == as.integer(input$ano) & is.finite(s$value), ]
      if (!NROW(d)) return(NULL)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$value)) +
        ggplot2::geom_histogram(
          bins = 40, fill = "#1351B4", color = "white", na.rm = TRUE) +
        ggplot2::labs(x = "valor", y = "localidades",
                      title = sprintf("Histograma - %d", as.integer(input$ano)))
      plotly::ggplotly(eixo(p)) |> plotly::config(displayModeBar = FALSE)
    })
    output$box <- plotly::renderPlotly({
      s <- serie()
      if (is.null(s)) return(NULL)
      d <- s[is.finite(s$value), ]
      if (!NROW(d)) return(NULL)
      d$ano <- factor(d$ano)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$ano, .data$value)) +
        ggplot2::geom_boxplot(fill = "#1351B4", alpha = 0.6, na.rm = TRUE) +
        ggplot2::labs(x = "Ano", y = "valor", title = "Boxplot por ano")
      plotly::ggplotly(eixo(p)) |> plotly::config(displayModeBar = FALSE)
    })
  })
}
