#' panel_globe UI Function
#'
#' @description A shiny Module. Globo ortografico das Unidades da Federacao
#'   (geometrias do proprio DW): arrastar gira o globo, clicar numa UF com
#'   dados seleciona a localidade na aba Regiao. Port do globo do
#'   labourvaluesdatapanel, adaptado ao nivel municipal brasileiro do AEDi.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_globe_ui <- function(id) {
  ns <- NS(id)
  tags$div(id = ns("globo"), class = "painel-globo-host",
           `aria-label` = "Globo das Unidades da Federação")
}

#' panel_globe Server Functions
#'
#' @param indicador reactive com o mdata_id corrente — define quais UFs
#'   aparecem como "com dados"
#' @param uf_atual reactive com a localidade de nivel UF selecionada na aba
#'   Regiao (ou NULL) — realimenta o globo quando a escolha muda na caixa
#'
#' @return reactive com o local_id da UF clicada no globo (ou NULL)
#'
#' @noRd
mod_panel_globe_server <- function(id,
                                   indicador = shiny::reactive(NULL),
                                   uf_atual = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    geo <- painel_geo_uf_cache()
    geojson <- painel_geojson(geo)
    ufs <- lapply(seq_len(nrow(geo)), function(i) list(
      code = geo$code[i], label = geo$label[i]))

    disponiveis <- shiny::reactive({
      ind <- suppressWarnings(as.integer(indicador()))[1]
      shiny::req(!is.na(ind))
      painel_locais_com_dados_cache(ind, 2L)
    })

    uf_escolhida <- shiny::reactiveVal(NULL)

    # A geometria viaja na primeira mensagem e apos cada remontagem do host
    # (handshake via input$pronto); nas demais, apenas disponibilidade e
    # selecao. identical(NULL, NULL) e TRUE, por isso a instancia comeca -1.
    instancia <- -1L
    geo_no_ar <- FALSE
    shiny::observe({
      atual <- uf_atual()
      msg <- list(
        hostId = session$ns("globo"),
        inputId = session$ns("uf"),
        readyId = session$ns("pronto"),
        ufs = ufs,
        disponiveis = disponiveis(),
        selected = if (length(atual)) as.character(atual) else "")
      pronto <- if (is.null(input$pronto)) -1L else as.integer(input$pronto)
      if (!identical(pronto, instancia) || !isTRUE(geo_no_ar)) {
        msg$geojson <- geojson
        geo_no_ar <<- TRUE
        instancia <<- pronto
      }
      session$sendCustomMessage("painel-globe", msg)
    })

    shiny::observeEvent(input$uf, {
      uf_escolhida(as.integer(input$uf))
    })

    reactive(uf_escolhida())
  })
}

## To be copied in the UI
# mod_panel_globe_ui(ns("panel_globe_1"))  # dentro do modulo hospedeiro

## To be copied in the server
# mod_panel_globe_server("panel_globe_1", indicador, uf_atual)
