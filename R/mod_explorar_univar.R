#' UI da ficha do indicador (aba "Análise Univariada")
#' @keywords internal
mod_explorar_univar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, title = "Ficha do indicador",
      shiny::selectizeInput(
        ns("indicador"), "Indicador", choices = NULL,
        options = list(placeholder = "Selecione um indicador",
                       maxOptions = 200)),
      shiny::verbatimTextOutput(ns("resumo")),
      DT::DTOutput(ns("por_ano"))))
}

#' Server da ficha: cobertura e dispersao por ano (numeros da heuristica
#' de banda/lolly do roadmap) lidas da serie achatada
#' @keywords internal
mod_explorar_univar_server <- function(id) {
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
      rotulos <- setNames(as.character(md$mdata_id), md$rotulo)
      shiny::updateSelectizeInput(session, "indicador", choices = rotulos)
    })
    serie <- shiny::reactive({
      if (!length(input$indicador) || !nzchar(input$indicador))
        return(NULL)
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(painel_valores_por_ano(con, as.integer(input$indicador)),
               error = function(e) NULL)
    })
    ficha <- shiny::reactive({
      s <- serie()
      if (is.null(s)) return(NULL)
      explorar_ficha(s)
    })
    output$resumo <- shiny::renderText({
      f <- ficha()
      if (is.null(f)) return("Sem dados (banco fora do ar ou indicador sem série).")
      u <- f$por_ano[nrow(f$por_ano), ]
      sprintf(paste0("anos: %d-%d | localidades com dado: %d\n",
                     "último ano (%d): n = %d, média = %.4g, dp = %.4g, ",
                     "CV = %.4g, mediana = %.4g, IQR = %.4g\n%s"),
              f$anos[1], f$anos[2], f$n_locais, u$ano, u$n,
              u$media, u$dp, u$cv, u$mediana, u$iqr,
              if (f$degenerada)
                "série degenerada (dp \u2248 0): sem banda/contexto seccional"
              else if (f$cauda_pesada)
                "cauda pesada (|CV| > 1,5): banda mín-máx degenera; prefira percentis, log ou lolly"
              else "dispersão compatível com banda mín-máx")
    })
    output$por_ano <- DT::renderDT({
      f <- ficha()
      if (is.null(f)) return(DT::datatable(
        data.frame(Aviso = "Selecione um indicador."), rownames = FALSE))
      d <- f$por_ano
      nums <- c("min", "max", "media", "dp", "mediana", "iqr", "cv")
      d[nums] <- lapply(d[nums], function(v) signif(v, 5))
      DT::datatable(
        d, selection = "none", rownames = FALSE,
        options = list(pageLength = 12, language = list(url =
          "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")))
    })
  })
}
