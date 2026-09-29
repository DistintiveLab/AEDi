#' UI da matriz de correlacoes (aba "Matriz de correlação", F3)
#' @keywords internal
mod_explorar_matriz_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, title = "Matriz de correlação (N×N seletiva)",
      shiny::fluidRow(
        shiny::column(3, shiny::checkboxGroupInput(
          ns("classe"), "Classes no recorte", choices = NULL)),
        shiny::column(3, shiny::checkboxInput(
          ns("visiveis"), "Somente visíveis no painel", value = TRUE)),
        shiny::column(3, shiny::checkboxInput(
          ns("sem_degeneradas"), "Excluir séries degeneradas (dp ≈ 0)",
          value = TRUE)),
        shiny::column(3, shiny::selectInput(
          ns("leitura"), "Leitura do painel",
          choices = c("Pooled (todas as observações)" = "pooled",
                      "Between (médias por localidade)" = "between",
                      "Within (desvios da localidade)" = "within")))),
      shiny::fluidRow(
        shiny::column(3, shiny::numericInput(
          ns("n_min"),
          "n mínimo de pares por célula (abaixo disso fica em branco)",
          value = 30, min = 3, max = 100000, step = 1)),
        shiny::column(3, shiny::actionButton(
          ns("calcular"), "Calcular matriz", class = "btn-primary",
          icon = shiny::icon("th")))),
      shiny::helpText("Pearson com NAs removidos por pares; passe o mouse ",
                      "para ver o n de pares da célula. Clique numa célula ",
                      "para abrir o par na correlação par a par."),
      shiny::textOutput(ns("resumo")),
      plotly::plotlyOutput(ns("heatmap"), height = "620px")))
}

#' Server da matriz: carrega as séries do recorte sob demanda (com
#' progresso), calcula r e n por par na leitura pedida e devolve o par
#' clicado como reactive list(x = mdata_id, y = mdata_id)
#' @keywords internal
mod_explorar_matriz_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogo <- shiny::reactive({
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(painel_mdata(con), error = function(e) NULL)
    })
    shiny::observe({
      md <- catalogo()
      if (is.null(md)) return(NULL)
      cls <- sort(unique(md$data_class_id[!is.na(md$data_class_id)]))
      rotulos <- c("1" = "Dado bruto / insumo", "2" = "Indicador direto",
                   "4" = "Indicador composto")
      nomes <- vapply(as.character(cls), function(k)
        if (k %in% names(rotulos)) unname(rotulos[k]) else
          paste("Classe", k), character(1))
      shiny::updateCheckboxGroupInput(
        session, "classe", choices = setNames(as.character(cls), nomes),
        selected = as.character(setdiff(cls, 1L)))
    })
    wide <- shiny::eventReactive(input$calcular, {
      md <- catalogo()
      if (is.null(md)) return(NULL)
      ids <- md$mdata_id
      if (isTRUE(input$visiveis)) ids <- ids[md$visivel]
      if (length(input$classe))
        ids <- ids[md$data_class_id %in% as.integer(input$classe)]
      ids <- unique(ids)
      if (!length(ids)) return(NULL)
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      origens <- setNames(md$orig_name, as.character(md$mdata_id))
      shiny::withProgress(
        message = "Carregando séries do recorte...", value = 0, {
          series <- list()
          for (i in ids) {
            shiny::incProgress(1 / length(ids),
                               detail = origens[as.character(i)])
            s <- tryCatch(painel_valores_por_ano(con, i),
                          error = function(e) NULL)
            if (!is.null(s) && NROW(s)) series[[as.character(i)]] <- s
          }
        })
      if (!length(series)) return(NULL)
      explorar_wide(series)
    }, ignoreInit = TRUE)
    wide_limpo <- shiny::reactive({
      w <- wide()
      if (is.null(w)) return(NULL)
      if (isTRUE(input$sem_degeneradas)) {
        deg <- explorar_degeneradas(w)
        mantem <- names(deg)[!deg]
        if (!length(mantem)) return(NULL)
        w <- w[, c("local_id", "ano", mantem), drop = FALSE]
      }
      w
    })
    mat <- shiny::reactive(explorar_matriz(wide_limpo(), input$leitura))
    id_orig <- shiny::reactive({
      md <- catalogo()
      if (is.null(md)) return(NULL)
      setNames(md$mdata_id, md$orig_name)
    })
    output$resumo <- shiny::renderText({
      m <- mat()
      if (is.null(m) || !length(m$r))
        return("Ajuste o recorte e clique em Calcular.")
      sprintf("%d séries | leitura: %s | células em branco: n < %d",
              ncol(m$r), input$leitura, as.integer(input$n_min))
    })
    output$heatmap <- plotly::renderPlotly({
      m <- mat()
      if (is.null(m) || !length(m$r)) return(NULL)
      r <- m$r
      r[m$n < as.integer(input$n_min)] <- NA
      mapa <- id_orig()
      rot <- colnames(r)
      if (!is.null(mapa)) rot <- ifelse(is.na(mapa[rot]), rot,
                                        unname(mapa[rot]))
      plotly::plot_ly(
        x = rot, y = rot, z = r, type = "heatmap", customdata = m$n,
        colorscale = list(c(0, "#1351B4"), c(0.5, "#F8F8F8"),
                          c(1, "#C5392B")),
        zmin = -1, zmax = 1,
        hovertemplate = paste0("%{y} × %{x}<br>r = %{z:.3f}",
                               "<br>n = %{customdata}<extra></extra>"),
        source = session$ns("heatmap")) |>
        plotly::layout(xaxis = list(tickangle = 45),
                       yaxis = list(autorange = "reversed")) |>
        plotly::config(displayModeBar = FALSE)
    })
    par <- shiny::reactiveVal(NULL)
    shiny::observe({
      ev <- plotly::event_data("plotly_click", source = session$ns("heatmap"))
      mapa <- id_orig()
      if (is.null(ev) || is.null(mapa) || !NROW(ev)) return(NULL)
      ix <- unname(mapa[ev$y]); iy <- unname(mapa[ev$x])
      if (length(ix) != 1L || length(iy) != 1L || is.na(ix) || is.na(iy))
        return(NULL)
      par(list(x = as.integer(iy), y = as.integer(ix)))
    })
    par
  })
}
