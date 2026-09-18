#' panel_regiao UI Function
#'
#' @description Serie temporal de um indicador do DW (aedidb) para a
#'   localidade escolhida dentro de um nivel territorial (regiao, UF,
#'   regiao intermediaria, microrregiao, regiao imediata, municipio).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_regiao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "painel-toolbar", role = "search", `aria-label` = "Seleção de indicador e localidade",
      tags$div(class = "form-group",
        tags$label(`for` = ns("indicador"), "Indicador"),
        shiny::selectizeInput(ns("indicador"), NULL, choices = NULL,
                              width = "100%", options = list(
                                placeholder = "Escolha um indicador"))),
      tags$div(class = "form-group",
        tags$label(`for` = ns("nivel"), "Nível territorial"),
        shiny::selectInput(ns("nivel"), NULL, choices = NULL,
                           selectize = FALSE, width = "auto")),
      tags$div(class = "form-group",
        tags$label(`for` = ns("localidade"), "Localidade"),
        shiny::selectizeInput(ns("localidade"), NULL, choices = NULL,
                              width = "100%", options = list(
                                placeholder = "Escolha uma localidade")))),
    tags$div(class = "painel-card",
      tags$h3(shiny::textOutput(ns("titulo")), class = "sr-only"),
      plotly::plotlyOutput(ns("serie"), height = "420px"),
      tags$p(class = "painel-nota",
        "Série do DW de indicadores do AEDi. Use o seletor de nível",
        "territorial para mudar de recorte (região, UF, divisões",
        "regionais do IBGE ou município) e escolher a localidade desejada."))
  )
}

#' panel_regiao Server Functions
#'
#' @param paleta reactive com a paleta ativa ("govbr" ou "pb") — define a
#'   cor da serie
#'
#' @noRd
mod_panel_regiao_server <- function(id,
                                    paleta = shiny::reactive("govbr")) {
  moduleServer(id, function(input, output, session) {

    con <- painel_con()
    md <- painel_mdata(con)
    niveis <- painel_niveis(con)
    DBI::dbDisconnect(con)
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = md$mdata_id[1], server = TRUE)
    shiny::updateSelectInput(session, "nivel",
      choices = setNames(niveis$nivel_id,
                         paste0(niveis$rotulo, " (", niveis$n_locais, ")")),
      selected = "2")

    locais <- shiny::reactive({
      shiny::req(input$nivel)
      con <- painel_con()
      on.exit(DBI::dbDisconnect(con))
      painel_locais_nivel(con, input$nivel)
    })

    # Ao trocar de nivel territorial: recarrega localidades e seleciona a
    # com maior cobertura do indicador corrente naquele nivel
    shiny::observeEvent(input$nivel, {
      escolhas <- locais()
      shiny::req(length(escolhas))
      indicador <- if (length(input$indicador)) input$indicador else md$mdata_id[1]
      con <- painel_con()
      on.exit(DBI::dbDisconnect(con))
      topo <- painel_local_top(con, indicador, input$nivel)
      if (is.null(topo)) topo <- as.integer(escolhas[[1]])
      shiny::updateSelectizeInput(session, "localidade",
                                  choices = escolhas, selected = topo,
                                  server = TRUE)
    })

    serie_loc <- shiny::reactive({
      shiny::req(input$indicador, input$localidade)
      con <- painel_con()
      on.exit(DBI::dbDisconnect(con))
      v <- painel_valores(con, input$indicador)
      v[v$local_id == as.integer(input$localidade), ]
    })

    titulo <- shiny::reactive({
      shiny::req(input$indicador, input$localidade)
      nome <- md$data_name[md$mdata_id == input$indicador]
      if (is.na(nome)) nome <- md$orig_name[md$mdata_id == input$indicador]
      rotulos <- locais()
      local <- names(rotulos)[match(as.integer(input$localidade), rotulos)]
      paste0(nome, " — ", local)
    })

    cor <- shiny::reactive({
      if (identical(paleta(), "pb")) "#78529D" else "#1351B4"
    })

    output$titulo <- shiny::renderText(titulo())

    output$serie <- plotly::renderPlotly({
      v <- serie_loc()
      shiny::validate(shiny::need(nrow(v),
        "Sem dados para esta combinação de indicador e localidade."))
      p <- ggplot2::ggplot(v, ggplot2::aes(x = as.Date(refdate), y = value)) +
        ggplot2::geom_line(color = cor(), linewidth = 0.9) +
        ggplot2::geom_point(color = cor(), size = 1.8) +
        ggplot2::labs(title = titulo(), x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_panel_regiao_ui("panel_regiao_1")

## To be copied in the server
# mod_panel_regiao_server("panel_regiao_1")
