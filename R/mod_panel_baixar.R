#' panel_baixar UI Function
#'
#' @description Downloads em planilha (.xlsx) inspirados na aba Download do
#'   labourvaluesdatapanel, em dois recortes: (1) todos os indicadores de uma
#'   regiao — escolha o nivel territorial e a localidade e receba uma planilha
#'   com uma linha por indicador e uma coluna por ano; (2) um indicador por
#'   ano — escolha o indicador e o nivel territorial e receba uma planilha
#'   com uma aba por ano, cada uma com todas as localidades daquele nivel com
#'   dados no ano.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_baixar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "painel-regiao-grade",
      tags$div(class = "painel-card",
        tags$h3("Todos os indicadores de uma região"),
        tags$div(class = "form-group",
          tags$label(`for` = ns("nivel_regiao"), "Nível territorial"),
          shiny::selectizeInput(ns("nivel_regiao"), NULL, choices = NULL,
                                width = "100%", options = painel_opcoes_select(
                                  "Escolha o nível", max_options = 100L))),
        tags$div(class = "form-group",
          tags$label(`for` = ns("regiao"), "Região"),
          shiny::selectizeInput(ns("regiao"), NULL, choices = NULL,
                                width = "100%", options = painel_opcoes_select(
                                  "Digite parte do nome"))),
        tags$p(class = "painel-nota",
          shiny::textOutput(ns("resumo_regiao"))),
        shiny::downloadButton(ns("baixar_regiao"), "Baixar planilha",
                              class = "btn-primary"),
        tags$p(class = "painel-nota",
          "Aba “dados”: uma linha por indicador (código e nome) e uma",
          "coluna por ano — célula = última observação do ano na",
          "localidade escolhida. Aba “metadados”: ficha dos indicadores",
          "incluídos.")),
      tags$div(class = "painel-card",
        tags$h3("Um indicador, uma aba por ano"),
        tags$div(class = "form-group",
          tags$label(`for` = ns("indicador"), "Indicador"),
          shiny::selectizeInput(ns("indicador"), NULL, choices = NULL,
                                width = "100%", options = painel_opcoes_select(
                                  "Escolha um indicador"))),
        tags$div(class = "form-group",
          tags$label(`for` = ns("nivel_ano"), "Nível territorial"),
          shiny::selectizeInput(ns("nivel_ano"), NULL, choices = NULL,
                                width = "100%", options = painel_opcoes_select(
                                  "Escolha o nível", max_options = 100L))),
        tags$p(class = "painel-nota",
          shiny::textOutput(ns("resumo_indicador"))),
        shiny::downloadButton(ns("baixar_indicador"), "Baixar planilha",
                              class = "btn-primary"),
        tags$p(class = "painel-nota",
          "Uma aba por ano, cada uma com todas as localidades do nível",
          "territorial com dados naquele ano (código, nome e valor da",
          "última observação do ano). Aba “metadados”: contexto da",
          "planilha.")))
  )
}

#' panel_baixar Server Functions
#'
#' O download e preguicoso como no lvdp: a planilha so e composta no clique
#' (nada roda enquanto o visitante apenas muda a selecao); combinacoes sem
#' dados geram um arquivo com aba de aviso, nao um erro na tela.
#'
#' @noRd
mod_panel_baixar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    md <- painel_mdata_cache()
    niveis <- painel_niveis_cache()

    opcoes_niveis <- setNames(niveis$nivel_id,
                              paste0(niveis$rotulo, " (", niveis$n_locais, ")"))
    default_nivel <- painel_nivel_default(niveis)
    shiny::updateSelectizeInput(session, "nivel_regiao",
                                choices = opcoes_niveis,
                                selected = default_nivel)
    shiny::updateSelectizeInput(session, "nivel_ano",
                                choices = opcoes_niveis,
                                selected = default_nivel)
    shiny::updateSelectizeInput(session, "indicador",
      choices = painel_opcoes_indicador(md),
      selected = if (nrow(md)) painel_indicador_default(md) else NULL,
      server = TRUE)

    # --- recorte 1: todos os indicadores de uma regiao --------------------
    locais_regiao <- shiny::reactive({
      shiny::req(input$nivel_regiao)
      painel_locais_nivel_cache(input$nivel_regiao)
    })
    shiny::observeEvent(input$nivel_regiao, {
      escolhas <- locais_regiao()
      shiny::req(length(escolhas))
      shiny::updateSelectizeInput(session, "regiao", choices = escolhas,
                                  selected = as.integer(escolhas[[1]]),
                                  server = TRUE)
    })
    rotulo_regiao <- shiny::reactive({
      rotulos <- locais_regiao()
      i <- match(suppressWarnings(as.integer(input$regiao)),
                 unlist(rotulos, use.names = FALSE))
      if (!length(i) || is.na(i)) "—" else names(rotulos)[i]
    })
    rotulo_nivel_regiao <- shiny::reactive({
      rotulo <- niveis$rotulo[as.character(niveis$nivel_id) ==
                               as.character(input$nivel_regiao)]
      if (length(rotulo) && !is.na(rotulo[1])) rotulo[1] else "Localidade"
    })
    output$resumo_regiao <- shiny::renderText({
      if (!length(input$regiao)) "Escolha o nível territorial e a região."
      else paste0(rotulo_nivel_regiao(), ": ", rotulo_regiao())
    })
    output$baixar_regiao <- shiny::downloadHandler(
      filename = function() {
        sprintf("indicadores_%s_%s.xlsx", painel_slug(rotulo_regiao()),
                format(Sys.Date(), "%Y-%m-%d"))
      },
      content = function(arquivo) {
        valores <- if (length(input$regiao))
          painel_valores_local_todos_cache(input$regiao) else
          data.frame(mdata_id = integer(0),
                     refdate = as.Date(character(0)), value = numeric(0))
        painel_xlsx_regiao(arquivo, valores = valores, mdata = md,
                           local_rotulo = rotulo_regiao(),
                           nivel_rotulo = rotulo_nivel_regiao())
      })

    # --- recorte 2: um indicador, uma aba por ano -------------------------
    rotulo_indicador <- shiny::reactive({
      i <- match(suppressWarnings(as.integer(input$indicador)), md$mdata_id)
      if (!length(i) || is.na(i)) "—" else md$rotulo[i]
    })
    rotulo_nivel_ano <- shiny::reactive({
      rotulo <- niveis$rotulo[as.character(niveis$nivel_id) ==
                               as.character(input$nivel_ano)]
      if (length(rotulo) && !is.na(rotulo[1])) rotulo[1] else "Localidade"
    })
    output$resumo_indicador <- shiny::renderText({
      if (!length(input$indicador)) "Escolha o indicador e o nível territorial."
      else paste0(rotulo_indicador(), " — ", rotulo_nivel_ano())
    })
    output$baixar_indicador <- shiny::downloadHandler(
      filename = function() {
        i <- match(suppressWarnings(as.integer(input$indicador)), md$mdata_id)
        orig <- if (!length(i) || is.na(i)) "indicador" else md$orig_name[i]
        sprintf("%s_%s_%s.xlsx", painel_slug(orig),
                painel_slug(rotulo_nivel_ano()),
                format(Sys.Date(), "%Y-%m-%d"))
      },
      content = function(arquivo) {
        indicador <- suppressWarnings(as.integer(input$indicador))[1]
        locais <- painel_locais_nivel_cache(input$nivel_ano)
        por_ano <- list()
        if (!is.na(indicador)) {
          ids <- unlist(locais, use.names = FALSE)
          anos <- painel_anos_cache(indicador)$ano
          por_ano <- lapply(anos, function(ano) {
            v <- painel_valores_ano_cache(indicador, ano)
            v[v$local_id %in% ids, , drop = FALSE]
          })
          names(por_ano) <- as.character(anos)
        }
        painel_xlsx_indicador(arquivo, por_ano = por_ano, locais = locais,
                              rotulo_indicador = rotulo_indicador(),
                              nivel_rotulo = rotulo_nivel_ano())
      })
  })
}
