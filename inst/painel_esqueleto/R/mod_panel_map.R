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
    tags$div(class = "painel-mapa",
             leaflet::leafletOutput(ns("mapa"), height = "calc(100vh - 370px)"))
  )
}

#' panel_map Server Functions
#'
#' @noRd
mod_panel_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    md <- painel_mdata_cache()
    geo <- painel_geo_mun_cache()
    geo$layer_id <- as.character(geo$local_id)
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = if (nrow(md)) md$mdata_id[1] else NULL,
                                server = TRUE)

    # Anos disponiveis do indicador direto do DW (agregacao cacheada), sem
    # puxar a serie completa para descobri-los
    anos_ind <- shiny::reactive({
      shiny::validate(shiny::need(nrow(md) > 0,
        "DW sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
      shiny::req(input$indicador)
      painel_anos_cache(input$indicador)$ano
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

    # ultimo refdate dentro do ano escolhido (valor mais recente do ano),
    # agregado no proprio SQL e cacheado; NULL quando vazio e o contrato
    # assumido por paleta, status, camadas e legenda abaixo
    mapa_dados <- shiny::reactive({
      shiny::req(input$indicador, input$ano)
      v <- painel_valores_ano_cache(input$indicador, input$ano)
      if (!nrow(v)) return(NULL)
      v
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
        # tiles Carto com CARTO_API_KEY ou fundo neutro vetorial sem tiles
        # (padrao do labourvaluesdatapanel + contorno de UFs do IBGE)
        painel_basemap_adicionar(contorno_uf = painel_geo_uf_cache) |>
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
