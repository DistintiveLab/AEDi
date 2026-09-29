#' UI do dicionario de dados do catalogo (aba "Dicionário de Dados")
#' @keywords internal
mod_explorar_dicionario_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(shinydashboard::box(
    width = 12, title = "Dicionário de dados",
    shiny::selectInput(
      ns("classe"), "Classe",
      c("Todas", "Dado bruto / insumo", "Indicador direto",
        "Indicador composto")),
    DT::DTOutput(ns("tabela")),
    shiny::downloadButton(ns("csv"), "Baixar CSV")))
}

#' Server do dicionario: catalogo completo com filtros e export
#' @keywords internal
mod_explorar_dicionario_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    dic <- shiny::reactive({
      con <- explorar_con()
      if (is.null(con)) return(NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      tryCatch(explorar_dicionario(con), error = function(e) NULL)
    })
    vista <- shiny::reactive({
      d <- dic()
      if (is.null(d)) return(NULL)
      if (!identical(input$classe, "Todas"))
        d <- d[!is.na(d$classe) & d$classe == input$classe, , drop = FALSE]
      d
    })
    output$tabela <- DT::renderDT({
      d <- vista()
      if (is.null(d))
        return(DT::datatable(
          data.frame(Aviso = "Sem acesso ao banco de dados."),
          rownames = FALSE))
      DT::datatable(
        d, selection = "none", rownames = FALSE,
        options = list(pageLength = 15, language = list(url =
          "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")))
    })
    output$csv <- shiny::downloadHandler(
      filename = function()
        sprintf("dicionario_indicadores_%s.csv", format(Sys.Date())),
      content = function(arq) {
        d <- vista()
        if (is.null(d))
          d <- data.frame(aviso = "sem acesso ao banco no momento do download")
        readr::write_csv(d, arq)
      })
  })
}
