#' panel_globe UI Function
#'
#' @description A shiny Module. Globo ortografico das delimitacoes
#'   territoriais do nivel corrente (geometrias do banco de dados do
#'   painel): arrastar gira livremente pelo mundo, a roda e os botoes
#'   aproximam estilo Google Earth e clicar numa area com dados escolhe a
#'   localidade na aba Regiao. No nivel municipal (sem geometria coletiva
#'   leve) a base sao as UFs com o municipio escolhido destacado e a UF
#'   inteira em foco. Port do globo do labourvaluesdatapanel.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
mod_panel_globe_ui <- function(id) {
  ns <- NS(id)
  tags$div(id = ns("globo"), class = "painel-globo-host",
           `aria-label` = "Globo das delimitações territoriais do IBGE")
}

#' panel_globe Server Functions
#'
#' @param nivel reactive com o nivel territorial corrente da aba Regiao —
#'   define as delimitacoes desenhadas no globo
#' @param indicador reactive com o mdata_id corrente — define quais areas
#'   aparecem como "com dados"
#' @param localidade reactive com a localidade selecionada na aba Regiao
#'   (ou NULL) — vira o destaque do globo quando nao esta entre as feicoes
#'
#' @return reactive com a list(code, modo) clicada no globo: modo
#'   "nivel" seleciona direto a localidade do nivel corrente; modo "uf"
#'   pede ao chamador o destino do clique na UF da base estadual
#'
#' @noRd
mod_panel_globe_server <- function(id,
                                   nivel = shiny::reactive("2"),
                                   indicador = shiny::reactive(NULL),
                                   localidade = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Delimitacoes do nivel corrente; niveis com muitas feicoes
    # (municipio: 5.7 mil poligonos) usam as UFs como base e a
    # localidade escolhida vira um destaque sobre o estado
    modo_geo <- shiny::reactive({
      n <- suppressWarnings(as.integer(nivel()))[1]
      if (is.na(n)) n <- 2L
      g <- painel_geo_nivel_cache(n)
      if (nrow(g)) list(geo = g, modo = "nivel") else
        list(geo = painel_geo_uf_cache(), modo = "uf")
    })

    geojson <- shiny::reactive(painel_geojson(modo_geo()$geo))
    locais <- shiny::reactive({
      g <- modo_geo()$geo
      lapply(seq_len(nrow(g)), function(i)
        list(code = g$code[i], label = g$label[i]))
    })

    disponiveis <- shiny::reactive({
      ind <- suppressWarnings(as.integer(indicador()))[1]
      n <- suppressWarnings(as.integer(nivel()))[1]
      shiny::req(!is.na(ind), !is.na(n))
      if (identical(modo_geo()$modo, "uf"))
        painel_ufs_com_dados_cache(ind, n)
      else
        painel_locais_com_dados_cache(ind, n)
    })

    clique <- shiny::reactiveVal(NULL)

    # A geometria do nivel viaja na primeira mensagem, apos cada
    # remontagem do host (handshake via input$pronto) e quando o nivel
    # muda; nas demais, apenas disponibilidade e selecao. Destaque (a
    # localidade fora das feicoes) e contexto (a UF que a contem) so
    # viajam quando o par muda — o cliente guarda o que ja recebeu.
    instancia <- -1L
    enviado <- list(geo = NULL, destaque = NULL, contexto = NULL)
    shiny::observe({
      atual <- localidade()
      md <- modo_geo()
      msg <- list(
        hostId = session$ns("globo"),
        inputId = session$ns("clique"),
        readyId = session$ns("pronto"),
        modo = md$modo,
        nivel = as.character(nivel()),
        locais = locais(),
        disponiveis = disponiveis(),
        selected = if (identical(md$modo, "nivel") && length(atual) &&
                       !is.na(atual)) as.character(atual) else "",
        destaque = "",
        contexto = "")
      pronto <- if (is.null(input$pronto)) -1L else as.integer(input$pronto)
      geo_chave <- paste(md$modo, nivel())
      remontou <- !identical(pronto, instancia)
      if (remontou || !identical(enviado$geo, geo_chave)) {
        msg$geojson <- geojson()
        enviado$geo <<- geo_chave
        instancia <<- pronto
      }
      if (identical(md$modo, "uf") && length(atual) && !is.na(atual)) {
        msg$destaque <- as.character(atual)
        if (remontou || !identical(enviado$destaque, msg$destaque)) {
          dest <- painel_geo_local_cache(atual)
          if (nrow(dest)) {
            msg$destaqueGeojson <- painel_geojson(dest)
            enviado$destaque <<- msg$destaque
          } else msg$destaque <- ""
        }
        pai <- painel_geo_pai_uf_cache(atual)
        if (nrow(pai)) {
          msg$contexto <- pai$code[1]
          if (remontou || !identical(enviado$contexto, msg$contexto)) {
            msg$contextoGeojson <- painel_geojson(pai)
            enviado$contexto <<- msg$contexto
          }
        } else enviado$contexto <<- NULL
      } else {
        enviado$destaque <<- NULL
        enviado$contexto <<- NULL
      }
      session$sendCustomMessage("painel-globe", msg)
    })

    shiny::observeEvent(input$clique, {
      escolha <- input$clique
      if (is.list(escolha) && !is.null(escolha$code) &&
          !is.na(suppressWarnings(as.integer(escolha$code))[1])) {
        clique(list(code = suppressWarnings(as.integer(escolha$code))[1],
                    modo = escolha$modo))
      }
    })

    reactive(clique())
  })
}

## To be copied in the UI
# mod_panel_globe_ui(ns("panel_globe_1"))  # dentro do modulo hospedeiro

## To be copied in the server
# mod_panel_globe_server("panel_globe_1", nivel, indicador, localidade)
