#' panel_map UI Function
#'
#' @description A shiny Module. Mapa coropletico municipal de um indicador
#'   do DW (aedidb) no ano escolhido (ultimo refdate do ano).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(7, shiny::selectizeInput(ns("indicador"), "Indicador",
                                             choices = NULL, width = "100%")),
      shiny::column(3, shiny::selectInput(ns("ano"), "Ano", choices = NULL)),
      shiny::column(2, shiny::br(),
                    shiny::actionButton(ns("atualizar"), "Atualizar",
                                        class = "btn-primary"))
    ),
    leaflet::leafletOutput(ns("mapa"), height = "calc(100vh - 330px)")
  )
}

#' panel_map Server Functions
#'
#' @noRd
mod_panel_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    con <- painel_con()
    md <- painel_mdata(con)
    geo <- painel_geo_mun(con)
    DBI::dbDisconnect(con)
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = md$mdata_id[1], server = TRUE)

    serie_ind <- shiny::eventReactive(
      c(input$indicador, input$atualizar), {
        shiny::req(input$indicador)
        con <- painel_con()
        on.exit(DBI::dbDisconnect(con))
        painel_valores(con, input$indicador)
      })

    anos_ind <- shiny::reactive({
      unique(format(as.Date(serie_ind()$refdate), "%Y"))
    })

    shiny::observeEvent(serie_ind(), {
      anos <- sort(anos_ind())
      shiny::updateSelectInput(session, "ano", choices = anos,
                               selected = rev(anos)[1])
    })

    # ultimo refdate dentro do ano escolhido (valor mais recente do ano)
    mapa_dados <- shiny::reactive({
      shiny::req(input$ano)
      v <- serie_ind()
      v <- v[format(as.Date(v$refdate), "%Y") == input$ano &
               !is.na(v$value), ]
      if (!nrow(v)) return(NULL)
      ult <- stats::aggregate(refdate ~ local_id, v, max)
      merge(v, ult, by = c("local_id", "refdate"))
    })

    pal <- shiny::reactive({
      d <- mapa_dados()$value
      leaflet::colorNumeric("YlOrRd", domain = c(min(d), max(d)),
                            na.color = "transparent")
    })

    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet(geo) |>
        leaflet::setView(lng = -53.633308, lat = -13.550520, zoom = 4) |>
        leaflet::setMaxBounds(-77, -38, -27, 10) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels)
    })

    shiny::observeEvent(mapa_dados(), {
      g <- merge(geo, mapa_dados(), by = "local_id", all.x = FALSE)
      if (!nrow(g)) return(NULL)
      rotulo <- sprintf("<b>%s</b><br>%s",
                        g$local_name,
                        format(g$value, big.mark = ".", decimal.mark = ",",
                               scientific = FALSE, trim = TRUE))
      leaflet::leafletProxy(ns("mapa"), data = g) |>
        leaflet::clearShapes() |>
        leaflet::clearControls() |>
        leaflet::addPolygons(
          weight = 0.4, color = "#666666", opacity = 0.6,
          fillColor = ~ pal()(value), fillOpacity = 0.85,
          highlight = leaflet::highlightOptions(weight = 2, color = "white",
                                                bringToFront = TRUE),
          label = ~htmltools::HTML(rotulo)) |>
        leaflet::addLegend(pal = pal(), values = ~value, opacity = 0.8,
                           position = "bottomright",
                           title = md$orig_name[md$mdata_id == input$indicador])
    })
  })
}

## To be copied in the UI
# mod_panel_map_ui("panel_map_1")

## To be copied in the server
# mod_panel_map_server("panel_map_1")
