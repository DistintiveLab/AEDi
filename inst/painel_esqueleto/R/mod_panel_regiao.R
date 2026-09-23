#' panel_regiao UI Function
#'
#' @description Resumo da região (valores recentes dos indicadores
#'   compostos dos 7 eixos, dos 4 objetivos e dos estratos PNAD) com botao
#'   "mostrar mais" expandindo um accordeon por raiz (Eixos, Objetivos,
#'   Estratos PNAD), grupo e indicador com mini-graficos, alem da serie
#'   temporal de um indicador do banco de dados do painel para a
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
                              width = "100%", options = painel_opcoes_select(
                                "Escolha um indicador"))),
      tags$div(class = "form-group",
        tags$label(`for` = ns("nivel"), "Nível territorial"),
        shiny::selectizeInput(ns("nivel"), NULL, choices = NULL,
                              width = "auto", options = painel_opcoes_select(
                                "Escolha o nível", max_options = 100L))),
      tags$div(class = "form-group",
        tags$label(`for` = ns("localidade"), "Localidade"),
        shiny::selectizeInput(ns("localidade"), NULL, choices = NULL,
                              width = "100%", options = painel_opcoes_select(
                                "Digite parte do nome")))),
    tags$div(class = "painel-card",
      tags$h3("Resumo da região"),
      tags$p(class = "painel-nota", shiny::textOutput(ns("resumo_local"))),
      shiny::uiOutput(ns("resumo")),
      shiny::uiOutput(ns("detalhes"))),
    tags$div(class = "painel-regiao-grade",
      tags$div(class = "painel-card",
        tags$h3(shiny::textOutput(ns("titulo")), class = "sr-only"),
        plotly::plotlyOutput(ns("serie"), height = "420px"),
        tags$p(class = "painel-nota",
          "Série do banco de dados do painel. Use o seletor de nível",
          "territorial para mudar de recorte (região, UF, divisões",
          "regionais do IBGE ou município) e escolher a localidade desejada.")),
      tags$div(class = "painel-card painel-globo-card",
        tags$h3("Globo de localidades"),
        mod_panel_globe_ui(ns("panel_globe_1")),
        tags$p(class = "painel-nota",
          "Arraste para girar, aproxime com a roda ou pelos botões e clique",
          "em uma área com dados para escolher a localidade do nível",
          "territorial corrente; no nível municipal o município escolhido",
          "aparece destacado sobre o estado inteiro.")))
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
    hierarquia <- painel_hierarquia_cache()
    compostos <- painel_compostos_cache()
    shiny::updateSelectizeInput(session, "indicador",
                                choices = setNames(md$mdata_id, md$rotulo),
                                selected = if (nrow(md)) md$mdata_id[1] else NULL,
                                server = TRUE)
    shiny::updateSelectizeInput(session, "nivel",
      choices = setNames(niveis$nivel_id,
                         paste0(niveis$rotulo, " (", niveis$n_locais, ")")),
      selected = painel_nivel_default(niveis))

    locais <- shiny::reactive({
      shiny::req(input$nivel)
      painel_locais_nivel_cache(input$nivel)
    })

    # Globo de localidades: as delimitacoes do nivel corrente e o clique
    # contextualizado — numa feicao do nivel seleciona direto; numa UF da
    # base municipal escolhe a localidade com maior cobertura dentro dela
    uf_pendente <- shiny::reactiveVal(NULL)
    clique_globo <- mod_panel_globe_server("panel_globe_1",
      nivel = shiny::reactive(input$nivel),
      indicador = shiny::reactive({
        ind <- suppressWarnings(as.integer(input$indicador))[1]
        if (!is.na(ind)) ind else NULL
      }),
      localidade = shiny::reactive(
        if (length(input$localidade) && nzchar(input$localidade))
          suppressWarnings(as.integer(input$localidade))[1] else NULL))

    # Clique no globo: feicao do proprio nivel vai direto ao seletor;
    # UF (base do nivel municipal) leva a localidade com mais pontos do
    # indicador dentro dela — sem candidatos, cai no nivel UF com ela
    shiny::observeEvent(clique_globo(), {
      escolha <- clique_globo()
      shiny::req(is.list(escolha), length(escolha$code),
                 !is.na(escolha$code))
      if (identical(escolha$modo, "nivel")) {
        escolhas <- locais()
        if (escolha$code %in% unlist(escolhas)) {
          shiny::updateSelectizeInput(session, "localidade",
            choices = escolhas, selected = escolha$code, server = TRUE)
        }
      } else {
        indicador <- suppressWarnings(as.integer(input$indicador))[1]
        if (is.na(indicador) && nrow(md)) indicador <- md$mdata_id[1]
        destino <- if (!is.na(indicador))
          painel_local_top_uf_cache(indicador, input$nivel, escolha$code) else NULL
        if (is.null(destino)) {
          uf_pendente(escolha$code)
          shiny::updateSelectizeInput(session, "nivel", selected = "2")
        } else {
          shiny::updateSelectizeInput(session, "localidade",
            choices = locais(), selected = destino, server = TRUE)
        }
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
        "Banco de dados do painel sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
      shiny::req(input$indicador, input$localidade)
      painel_valores_local_cache(input$indicador, input$localidade)
    })

    # Rotulo da localidade corrente: sai dos proprios rotulos das escolhas
    # do selectize (nome + sigla da UF no nivel municipal)
    rotulo_local <- shiny::reactive({
      rotulos <- locais()
      i <- match(as.integer(input$localidade), rotulos)
      if (!length(i) || is.na(i)) NULL else names(rotulos)[i]
    })

    # Nome do nivel territorial corrente (Município, Região, UF...)
    rotulo_nivel <- shiny::reactive({
      rotulo <- niveis$rotulo[as.character(niveis$nivel_id) ==
                                as.character(input$nivel)]
      if (length(rotulo) && !is.na(rotulo[1])) rotulo[1] else "Localidade"
    })

    titulo <- shiny::reactive({
      shiny::validate(shiny::need(nrow(md) > 0,
        "Banco de dados do painel sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
      shiny::req(input$indicador, rotulo_local())
      indice <- match(as.integer(input$indicador), md$mdata_id)
      nome <- md$data_name[indice]
      if (is.na(nome)) nome <- md$orig_name[indice]
      paste0(nome, " — ", rotulo_local())
    })

    output$resumo_local <- shiny::renderText({
      shiny::req(rotulo_local())
      paste0(rotulo_nivel(), ": ", rotulo_local())
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

    # Resumo da localidade (labourvaluesdatapanel-like): todos os valores
    # da localidade em uma leitura; chips com o ultimo valor de cada
    # indicador composto por objetivo
    resumo_vals <- shiny::reactive({
      shiny::req(input$localidade)
      painel_valores_local_todos_cache(input$localidade)
    })

    expandido <- shiny::reactiveVal(FALSE)

    # Chips do resumo: um por indicador composto do catalogo (7 eixos, 4
    # objetivos e os estratos PNAD), com o valor mais recente na localidade
    resumo <- shiny::reactive({
      painel_resumo_grupos(hierarquia, compostos, resumo_vals())
    })

    output$resumo <- shiny::renderUI({
      shiny::validate(shiny::need(nrow(md) > 0,
        "Banco de dados do painel sem indicadores (tabela mdata vazia): confira as variáveis user, password, host e dbname e reinicie a sessão R antes de relançar o app."))
      shiny::req(input$localidade)
      if (!nrow(hierarquia) || !nrow(compostos)) {
        return(tags$p(class = "painel-resumo-nota",
          "Sem agrupamentos por objetivo e indicadores compostos no catálogo",
          " do banco de dados do painel."))
      }
      r <- resumo()
      chips <- lapply(seq_len(nrow(r)), function(i) {
        tags$div(class = "painel-resumo-chip",
          tags$div(class = "painel-resumo-chip-rotulo", r$rotulo[i]),
          tags$div(class = "painel-resumo-chip-valor",
            painel_num(r$valor[i]),
            tags$small(paste0(" (", format(r$refdate[i], "%Y"), ")"))))
      })
      tagList(
        if (length(chips)) {
          tags$div(class = "painel-resumo-grade", chips)
        } else {
          tags$p(class = "painel-resumo-nota",
            "Sem valores de indicadores compostos para esta localidade.")
        },
        tags$div(class = "painel-resumo-acoes",
          shiny::actionButton(session$ns("mostrar_mais"),
            if (expandido()) "Mostrar menos" else "Mostrar mais")))
    })

    shiny::observeEvent(input$mostrar_mais, {
      expandido(!expandido())
    })

    # Accordeon do "mostrar mais": raizes (Eixos, Objetivos) > grupo >
    # indicador com mini-grafico da serie na localidade corrente
    indicador_bloco <- function(mid) {
      linha <- hierarquia[match(mid, hierarquia$mdata_id), ]
      nome <- linha$data_name[1]
      if (is.na(nome) || !nzchar(nome)) nome <- linha$orig_name[1]
      composto <- !is.na(linha$data_class_id[1]) &&
        linha$data_class_id[1] == 4
      tags$div(class = "painel-grupo-indicador",
        tags$strong(nome),
        if (isTRUE(composto)) tags$small(" · indicador composto"),
        tags$small(paste0(" (", linha$orig_name[1], ")")),
        shiny::plotOutput(session$ns(paste0("mini_", mid)), height = "110px"))
    }

    grupo_bloco <- function(gid) {
      linhas <- hierarquia[hierarquia$datagroup_id == gid, ]
      ids <- unique(linhas$mdata_id[!is.na(linhas$mdata_id)])
      if (!length(ids)) return(NULL)
      tags$details(class = "painel-grupo painel-grupo-nivel2",
        tags$summary(linhas$datagroup_name[1],
          tags$small(paste0(" · ", length(ids), " indicadores"))),
        tags$div(class = "painel-grupo-corpo",
          lapply(ids, indicador_bloco)))
    }

    raiz_bloco <- function(rnome, aberta = FALSE) {
      filhos <- hierarquia[hierarquia$raiz_nome == rnome,
                           c("datagroup_id", "datagroup_name")]
      filhos <- unique(filhos)
      filhos <- filhos[order(filhos$datagroup_id), ]
      ids <- unique(hierarquia$mdata_id[hierarquia$raiz_nome == rnome &
                                          !is.na(hierarquia$mdata_id)])
      tags$details(class = "painel-grupo", open = aberta,
        tags$summary(rnome,
          tags$small(paste0(" · ", length(ids), " indicadores"))),
        tags$div(class = "painel-grupo-corpo",
          lapply(filhos$datagroup_id, grupo_bloco)))
    }

    output$detalhes <- shiny::renderUI({
      shiny::req(expandido())
      if (!nrow(hierarquia)) return(NULL)
      raizes <- c("Eixos", "Objetivos", "Estratos PNAD")
      raizes <- raizes[raizes %in% unique(hierarquia$raiz_nome)]
      raizes <- c(raizes, setdiff(unique(hierarquia$raiz_nome), raizes))
      lapply(seq_along(raizes), function(i)
        raiz_bloco(raizes[i], aberta = i == 1))
    })

    # Mini-graficos: outputs registrados uma unica vez para os indicadores
    # da hierarquia; cada render filtra resumo_vals() pelo proprio id
    ids_hierarquia <- unique(hierarquia$mdata_id[!is.na(hierarquia$mdata_id)])
    invisible(lapply(ids_hierarquia, function(mid) {
      local({
        id <- mid
        output[[paste0("mini_", id)]] <- shiny::renderPlot({
          v <- resumo_vals()[resumo_vals()$mdata_id == id, ]
          if (!nrow(v)) {
            return(ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0, y = 0, label = "Sem dados",
                                color = "#8a8a8a", size = 3.4) +
              ggplot2::theme_void())
          }
          ggplot2::ggplot(v, ggplot2::aes(x = as.Date(refdate), y = value)) +
            ggplot2::geom_line(color = cor(), linewidth = 0.7) +
            ggplot2::geom_point(color = cor(), size = 1.4) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(axis.title = ggplot2::element_blank())
        }, bg = "transparent")
      })
    }))
  })
}

## To be copied in the UI
# mod_panel_regiao_ui("panel_regiao_1")

## To be copied in the server
# mod_panel_regiao_server("panel_regiao_1")
