#' panel_series UI Function
#'
#' @description A shiny Module. Serie temporal de um indicador do DW
#'   (aedidb) para a localidade escolhida.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_series_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::selectizeInput(ns("indicador"), "Indicador",
                                             choices = NULL, width = "100%")),
      shiny::column(6, shiny::selectizeInput(ns("localidade"),
                                             "Localidade", choices = NULL,
                                             width = "100%"))
    ),
    plotly::plotlyOutput(ns("serie"), height = "320px")
  )
}

#' panel_series Server Functions
#'
#' @noRd
mod_panel_series_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    con <- painel_con()
    md <- painel_mdata(con)
    locais <- painel_locais(con)
    # default: localidade com maior cobertura do primeiro indicador
    top_loc <- DBI::dbGetQuery(con, sprintf(paste(
      "SELECT local_id FROM data_values WHERE mdata_id = %d",
      "GROUP BY local_id ORDER BY count(*) DESC LIMIT 1"),
      as.integer(md$mdata_id[1])))$local_id
    DBI::dbDisconnect(con)
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = md$mdata_id[1], server = TRUE)
    shiny::updateSelectizeInput(session, "localidade", choices = locais,
                                selected = as.integer(top_loc), server = TRUE)

    serie_loc <- shiny::reactive({
      shiny::req(input$indicador, input$localidade)
      con <- painel_con()
      on.exit(DBI::dbDisconnect(con))
      v <- painel_valores(con, input$indicador)
      v[v$local_id == as.integer(input$localidade), ]
    })

    titulo <- shiny::reactive({
      shiny::req(input$indicador)
      nome <- md$data_name[md$mdata_id == input$indicador]
      if (is.na(nome)) nome <- md$orig_name[md$mdata_id == input$indicador]
      paste0(nome, " — ", names(locais)[locais == input$localidade])
    })

    output$serie <- plotly::renderPlotly({
      v <- serie_loc()
      shiny::validate(shiny::need(nrow(v), "Sem dados para esta combinacao."))
      p <- ggplot2::ggplot(v, ggplot2::aes(x = as.Date(refdate), y = value)) +
        ggplot2::geom_line(color = "#1351B4", linewidth = 0.9) +
        ggplot2::geom_point(color = "#1351B4", size = 1.8) +
        ggplot2::labs(title = titulo(), x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_panel_series_ui("panel_series_1")

## To be copied in the server
# mod_panel_series_server("panel_series_1")
