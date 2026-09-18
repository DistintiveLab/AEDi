# Painel de indicadores do DW (app Shiny autonoma) --------------------------

#' App do painel de indicadores (objeto shinyApp)
#'
#' Constroi o objeto `shinyApp` do painel de indicadores do DW (aedidb):
#' mapa coropletico municipal e serie temporal por localidade de qualquer
#' indicador gravado (mdata). E o motor por tras de [run_panel()] e da app
#' gerada por [deploy_panel()] — como ultima expressao de um `app.R`
#' hospedavel, basta `AEDi::panel_app()`.
#'
#' Conexao via variaveis de ambiente `user`/`password`/`host`/`dbname`
#' (mesmo padrao de [controle_con()]), com defaults locais.
#'
#' @param titulo titulo do header (default "Painel de Indicadores")
#'
#' @export
#' @importFrom shiny shinyApp
panel_app <- function(titulo = "Painel de Indicadores") {
  ui <- shiny::fluidPage(
    shinyGovBRstyle::use_govbr(),
    shinyGovBRstyle::br_header(
      titulo, "AEDi — DW de indicadores",
      logo = resolver_logo_src()),
    shinyGovBRstyle::br_layout(
      shinyGovBRstyle::br_tabs(
        "abas", c("Mapa", "S\u00e9ries"),
        shiny::tabPanel("Mapa", mod_panel_map_ui("panel_map_1")),
        shiny::tabPanel("S\u00e9ries", mod_panel_series_ui("panel_series_1"))),
      shinyGovBRstyle::br_footer()
    )
  )
  server <- function(input, output, session) {
    mod_panel_map_server("panel_map_1")
    mod_panel_series_server("panel_series_1")
  }
  shiny::shinyApp(ui, server)
}

#' Painel de indicadores do DW (app Shiny autonoma)
#'
#' Lanca um painel leve de consulta ao DW de indicadores (aedidb): mapa
#' coropletico municipal e serie temporal por localidade de qualquer
#' indicador gravado (mdata). Nao abre o AEDi completo — util para
#' conferir rapidamente o que foi carregado, inclusive em servidor.
#'
#' Para materializar uma app deployavel dentro de um projeto, use
#' [deploy_panel()]; para obter so o objeto da app (hospedavel),
#' use [panel_app()].
#'
#' Conexao via variaveis de ambiente `user`/`password`/`host`/`dbname`
#' (mesmo padrao de [controle_con()]), com defaults locais.
#'
#' @param port porta HTTP; default escolhe uma porta livre
#' @param titulo titulo do header (default "Painel de Indicadores")
#'
#' @export
run_panel <- function(port = NULL, titulo = "Painel de Indicadores") {
  app <- panel_app(titulo)
  if (!is.null(port)) app$options$port <- as.integer(port)
  app
}
