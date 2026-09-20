#' panel_map UI Function
#'
#' @description A shiny Module. Mapa coropletico municipal de um indicador
#'   do DW (aedidb) no ano escolhido (ultimo refdate do ano), com slider
#'   de ano animado, paleta divergente centrada em 0 (invertivel) e
#'   atualizacao incremental (port do labourvaluesdatapanel): a geometria
#'   e enviada uma unica vez e apenas cores/tooltips atravessam a conexao.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "painel-toolbar painel-mapa-toolbar", role = "search",
             `aria-label` = "Seleção de indicador e ano",
      tags$div(class = "form-group painel-mapa-indicador",
        tags$label(`for` = ns("indicador"), "Indicador"),
        tags$div(class = "painel-mapa-indicador-linha",
          shiny::selectizeInput(ns("indicador"), NULL, choices = NULL,
                                width = "100%", options = list(
                                  placeholder = "Escolha um indicador")),
          shiny::actionButton(ns("info"), NULL,
            icon = shiny::icon("circle-info"),
            class = "painel-info-btn",
            `aria-label` = "Sobre o indicador"))),
      tags$div(class = "form-group painel-mapa-ano",
        tags$label(`for` = ns("ano"), "Ano"),
        shiny::sliderInput(ns("ano"), NULL, min = 2000, max = 2025,
                           value = 2025, step = 1, sep = "", ticks = FALSE,
                           animate = shiny::animationOptions(interval = 500,
                                                             loop = FALSE),
                           width = "100%")),
      tags$div(class = "form-group painel-mapa-inverter",
        tags$label(`for` = ns("inverter"), "Escala"),
        shiny::checkboxInput(ns("inverter"), "Inverter cores"))),
    tags$p(class = "painel-mapa-status",
           shiny::textOutput(ns("status"), inline = TRUE)),
    leaflet::leafletOutput(ns("mapa"), height = "calc(100vh - 370px)")
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
    geo$layer_id <- as.character(geo$local_id)
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = md$mdata_id[1], server = TRUE)

    serie_ind <- shiny::reactive({
      shiny::req(input$indicador)
      con <- painel_con()
      on.exit(DBI::dbDisconnect(con))
      painel_valores(con, input$indicador)
    })

    anos_ind <- shiny::reactive({
      sort(unique(format(as.Date(serie_ind()$refdate), "%Y")))
    })

    # Envia apenas os limites: o navegador conserva a selecao de ano mais
    # recente e a ajusta a cobertura recebida (snap direcional sobre lacunas).
    shiny::observe({
      anos <- anos_ind()
      session$sendCustomMessage("painel-map-years", list(
        sliderId = session$ns("ano"),
        indicatorId = session$ns("indicador"),
        indicator = input$indicador,
        years = as.integer(anos)))
    })

    # ultimo refdate dentro do ano escolhido (valor mais recente do ano)
    mapa_dados <- shiny::reactive({
      shiny::req(input$ano)
      v <- serie_ind()
      v <- v[format(as.Date(v$refdate), "%Y") == as.character(input$ano) &
               !is.na(v$value), ]
      if (!nrow(v)) return(NULL)
      ult <- stats::aggregate(refdate ~ local_id, v, max)
      merge(v, ult, by = c("local_id", "refdate"))
    })

    paleta <- shiny::reactive({
      d <- if (is.null(mapa_dados())) NA_real_ else mapa_dados()$value
      painel_paleta(d, invertida = isTRUE(input$inverter))
    })

    output$status <- shiny::renderText({
      if (!is.null(mapa_dados()) && nrow(mapa_dados())) return("")
      paste0("Ano ", input$ano, ": sem observações. ",
             "Escolha outro ano ou indicador.")
    })

    # Rotulos e cores de TODOS os municipios (NA onde sem dados): o delta e
    # computado sobre o conjunto integral, como no labourvaluesdatapanel.
    camadas <- shiny::reactive({
      pal <- paleta()
      dados <- mapa_dados()
      valor <- if (is.null(dados)) rep(NA_real_, nrow(geo)) else
        dados$value[match(geo$local_id, dados$local_id)]
      cores <- pal(valor)
      texto <- ifelse(is.na(valor), "Sem dados", painel_num(valor))
      lapply(seq_len(nrow(geo)), function(i) list(
        id = geo$layer_id[i],
        color = cores[[i]],
        label = sprintf("<b>%s</b><br>%s", geo$local_name[i], texto[i])))
    })

    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet(geo) |>
        leaflet::setView(lng = -53.633308, lat = -13.550520, zoom = 4) |>
        leaflet::setMaxBounds(-77, -38, -27, 10) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels) |>
        htmlwidgets::onRender("function(el, x) { window.PainelMap.attach(el, this); }")
    })

    # Geometria persistente e atualizacao incremental: cada widget novo (por
    # exemplo, apos reconexao) recebe a geometria; ao mudar indicador, ano ou
    # paleta, somente cores e textos atravessam a conexao.
    mapa_instancia <- NULL
    geometria_enviada <- FALSE
    entregues <- list()
    shiny::observe({
      pronto <- input$mapa_painel_map_ready
      shiny::req(pronto)
      camadas <- camadas()
      reset <- !identical(mapa_instancia, pronto)
      if (reset) {
        geometria_enviada <<- FALSE
        entregues <<- list()
        mapa_instancia <<- pronto
      }
      proxy <- leaflet::leafletProxy("mapa", session = session, data = geo)
      if (!isTRUE(geometria_enviada)) {
        proxy |> leaflet::addPolygons(
          layerId = ~layer_id,
          fillColor = "transparent", fillOpacity = 0.85,
          weight = 0.4, color = "#666666", opacity = 0.6,
          highlight = leaflet::highlightOptions(weight = 2, color = "white",
                                                bringToFront = TRUE))
        geometria_enviada <<- TRUE
      }
      delta <- painel_delta_camadas(entregues, camadas)
      if (length(delta) || reset) {
        session$sendCustomMessage("painel-map-delta", list(
          id = session$ns("mapa"), instance = pronto, reset = reset,
          layers = delta))
        for (camada in delta) entregues[[camada$id]] <<- camada
      }
    })

    # Legenda colapsavel (o cliente a reempacota em details/summary)
    legenda_chave <- NULL
    shiny::observe({
      pronto <- input$mapa_painel_map_ready
      shiny::req(pronto, mapa_dados())
      dados <- mapa_dados()$value
      chave <- list(instance = pronto, indicador = input$indicador,
                    ano = input$ano, inverter = isTRUE(input$inverter),
                    faixa = range(dados, finite = TRUE))
      if (identical(chave, legenda_chave)) return()
      legenda_chave <<- chave
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy |> leaflet::removeControl("painel-legenda")
      if (any(is.finite(dados))) {
        limite <- range(dados, finite = TRUE)
        if (limite[1] == limite[2]) {
          margem <- max(abs(limite[1]) * 1e-8, 1e-12)
          limite <- limite + c(-margem, margem)
        }
        titulo <- md$data_name[md$mdata_id == input$indicador]
        if (is.na(titulo)) titulo <- md$orig_name[md$mdata_id == input$indicador]
        proxy |> leaflet::addLegend(
          "bottomright", layerId = "painel-legenda",
          # valores como intervalo para contornar o bug do valor unico
          values = c(limite[1] * 0.99999999, limite[2] * 1.00000001),
          pal = paleta(), opacity = 0.8, title = titulo,
          labFormat = function(type, cuts, ...) painel_num(cuts))
      }
    })

    # Ajuda do indicador: abre o modal sem alterar a selecao do mapa
    shiny::observeEvent(input$info, {
      shiny::req(input$indicador)
      linha <- md[md$mdata_id == input$indicador, ][1, ]
      titulo <- if (!is.na(linha$data_name)) linha$data_name else linha$orig_name
      descricao <- linha$data_desc
      if (is.na(descricao) || !nzchar(trimws(descricao))) {
        descricao <- "Descrição não disponível."
      }
      shiny::showModal(shiny::modalDialog(
        title = titulo,
        tags$p(tags$strong("Código no DW: "), linha$orig_name),
        tags$p(descricao),
        easyClose = TRUE,
        footer = shiny::modalButton("Fechar")))
    })
  })
}

## To be copied in the UI
# mod_panel_map_ui("panel_map_1")

## To be copied in the server
# mod_panel_map_server("panel_map_1")
