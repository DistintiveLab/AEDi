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
    tags$div(class = "painel-regiao-grade",
      tags$div(class = "painel-card",
        tags$h3(shiny::textOutput(ns("titulo")), class = "sr-only"),
        plotly::plotlyOutput(ns("serie"), height = "420px"),
        tags$p(class = "painel-nota",
          "Série do DW de indicadores do AEDi. Use o seletor de nível",
          "territorial para mudar de recorte (região, UF, divisões",
          "regionais do IBGE ou município) e escolher a localidade desejada.")),
      tags$div(class = "painel-card painel-globo-card",
        tags$h3("Globo de UFs"),
        mod_panel_globe_ui(ns("panel_globe_1")),
        tags$p(class = "painel-nota",
          "Arraste para girar e clique em uma UF com dados para selecioná-la",
          "no nível Unidade da Federação.")))
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
    md <- painel_mdata_cache()
    niveis <- painel_niveis_cache()
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = if (nrow(md)) md$mdata_id[1] else NULL,
                                server = TRUE)
    shiny::updateSelectInput(session, "nivel",
      choices = setNames(niveis$nivel_id,
                         paste0(niveis$rotulo, " (", niveis$n_locais, ")")),
      selected = "2")

    locais <- shiny::reactive({
      shiny::req(input$nivel)
      painel_locais_nivel_cache(input$nivel)
    })

    # Globo de UFs: clicar numa UF seleciona a localidade (e o nivel UF)
    uf_pendente <- shiny::reactiveVal(NULL)
    uf_globo <- mod_panel_globe_server("panel_globe_1",
      indicador = shiny::reactive({
        ind <- suppressWarnings(as.integer(input$indicador))[1]
        if (!is.na(ind)) ind else NULL
      }),
      uf_atual = shiny::reactive(
        if (identical(input$nivel, "2") && length(input$localidade) &&
            nzchar(input$localidade)) as.integer(input$localidade) else NULL))

    # Clique no globo: seleciona a UF e, se preciso, muda o nivel para UF.
    # A UF fica pendente para o observador do nivel aplica-la sobre as choices
    # ja recarregadas (locais() ainda veria o nivel antigo neste momento).
    shiny::observeEvent(uf_globo(), {
      uf <- uf_globo()
      shiny::req(length(uf), !is.na(uf))
      if (!identical(input$nivel, "2")) {
        uf_pendente(uf)
        shiny::updateSelectInput(session, "nivel", selected = "2")
      } else {
        shiny::updateSelectizeInput(session, "localidade",
                                    choices = locais(), selected = uf,
                                    server = TRUE)
      }
    })

    # Ao trocar de nivel territorial: recarrega localidades e seleciona a
    # com maior cobertura do indicador corrente naquele nivel — ou a UF
    # clicada no globo, quando a troca de nivel veio de la
    shiny::observeEvent(input$nivel, {
      escolhas <- locais()
      shiny::req(length(escolhas))
      pendente <- uf_pendente()
      uf_pendente(NULL)
      destino <- NULL
      if (length(pendente) && identical(input$nivel, "2") &&
          pendente %in% unlist(escolhas, use.names = FALSE)) {
        destino <- as.integer(pendente)
      } else {
        indicador <- suppressWarnings(as.integer(input$indicador))[1]
        if (is.na(indicador) && nrow(md)) indicador <- md$mdata_id[1]
        topo <- painel_local_top_cache(indicador, input$nivel)
        destino <- if (is.null(topo)) as.integer(escolhas[[1]]) else topo
      }
      shiny::updateSelectizeInput(session, "localidade",
                                  choices = escolhas, selected = destino,
                                  server = TRUE)
    })

    serie_loc <- shiny::reactive({
      shiny::validate(shiny::need(nrow(md) > 0,
        "DW sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
      shiny::req(input$indicador, input$localidade)
      painel_valores_local_cache(input$indicador, input$localidade)
    })

    titulo <- shiny::reactive({
      shiny::validate(shiny::need(nrow(md) > 0,
        "DW sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
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
