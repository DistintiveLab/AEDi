# Painel de indicadores do DW (app Shiny autonoma) --------------------------
#
# Estrutura adaptada do labourvaluesdatapanel: topbar com marca e acoes,
# navbarPage com abas realcadas, rodape fixo e paleta trocavel (gov.br ou
# preto e branco com o roxo da Distintive) persistida no navegador.

#' Resolve o src do logo para o painel (funciona fora do app completo)
#'
#' Igual a [resolver_logo_src()], mas o fallback aponta para o arquivo
#' embutido no pacote (inst/app/www) via resource path, para que o logo
#' exista tambem na app standalone gerada por [deploy_panel()].
#'
#' @keywords internal
painel_logo_src <- function() {
  logo <- Sys.getenv("aedi_logo", "www/aedi-Wide.png")
  if (grepl("^https?://", logo)) return(logo)
  if (file.exists(logo)) {
    shiny::addResourcePath("aedi_marca", dirname(normalizePath(logo)))
    return(file.path("aedi_marca", basename(logo)))
  }
  www <- system.file("app", "www", package = "AEDi")
  shiny::addResourcePath("aedi_marca", www)
  file.path("aedi_marca", basename(logo))
}

#' App do painel de indicadores (objeto shinyApp)
#'
#' Constroi o objeto `shinyApp` do painel de indicadores do DW (aedidb):
#' aba "Regi\u00e3o" com s\u00e9rie temporal por n\u00edvel territorial e
#' localidade (regi\u00e3o, UF, divis\u00f5es do IBGE ou munic\u00edpio), aba
#' "Mapa" coropl\u00e9tico municipal e aba "Sobre" com informa\u00e7\u00f5es do
#' autor e apoio da Distintive. Estrutura visual adaptada do
#' labourvaluesdatapanel: topbar com marca, abas real\u00e7adas e rodap\u00e9.
#' E o motor por tras de [run_panel()] e da app gerada por [deploy_panel()]
#' — como ultima expressao de um `app.R` hospedavel, basta
#' `AEDi::panel_app()`.
#'
#' Conexao via variaveis de ambiente `user`/`password`/`host`/`dbname`
#' (mesmo padrao de [controle_con()]), com defaults locais.
#'
#' @param titulo titulo do topbar (default "Painel de Indicadores")
#' @param paleta paleta inicial: `"govbr"` (padrao, azul Gov.br) ou `"pb"`
#'   (preto e branco com toques do roxo da Distintive). O visitante pode
#'   trocar no botao do topo; a escolha fica salva no navegador
#'
#' @export
#' @importFrom shiny shinyApp
panel_app <- function(titulo = "Painel de Indicadores",
                      paleta = c("govbr", "pb")) {
  paleta <- match.arg(paleta)
  painel_css <- system.file("painel", "painel.css", package = "AEDi")
  painel_js <- system.file("painel", "painel.js", package = "AEDi")
  logo <- painel_logo_src()

  ui <- shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport",
                       content = "width=device-width, initial-scale = 1")),
    htmltools::includeCSS(painel_css),
    htmltools::includeScript(painel_js),
    shinyGovBRstyle::use_govbr(),
    tags$div(id = "painel_raiz", `data-paleta` = paleta, class = "hidden"),
    tags$header(class = "painel-topbar",
      tags$a(class = "painel-brand", href = "#",
             `aria-label` = paste(titulo, "— in\u00edcio"),
             tags$img(src = logo, alt = "Logotipo AEDi"),
             tags$span(tags$span(class = "painel-brand-name", titulo),
                       tags$small("AEDi \u2014 DW de indicadores"))),
      tags$div(class = "painel-topbar-acoes",
        tags$button(id = "painel_paleta_btn", type = "button",
                    class = "painel-paleta-btn", `aria-pressed` = "true",
                    "Preto e branco"))),
    shiny::navbarPage(
      title = tags$span(class = "painel-marca-mobile", titulo),
      id = "painel_nav",
      selected = "regiao",
      windowTitle = titulo,
      collapsible = TRUE,
      lang = "pt-BR",
      shiny::tabPanel("Regi\u00e3o", value = "regiao",
                      mod_panel_regiao_ui("panel_regiao_1")),
      shiny::tabPanel("Mapa", value = "mapa",
                      mod_panel_map_ui("panel_map_1")),
      shiny::tabPanel("Sobre", value = "sobre",
                      panel_sobre_ui())),
    tags$div(class = "painel-busy", role = "status", `aria-live` = "polite",
             "Carregando\u2026"),
    tags$footer(class = "painel-rodape",
      tags$span("Dados: DW de indicadores do AEDi (aedidb)."),
      tags$span("Desenvolvido por Rodrigo E. S. Borges \u00b7 ",
        tags$a(href = "https://www.distintive.com.br", target = "_blank",
               rel = "noopener", "Distintive")))
  )

  server <- function(input, output, session) {
    paleta_ativa <- shiny::reactive({
      if (identical(input$painel_paleta_ativa, "pb")) "pb" else "govbr"
    })
    mod_panel_regiao_server("panel_regiao_1", paleta = paleta_ativa)
    mod_panel_map_server("panel_map_1")
    panel_sobre_server(input, session)
  }
  shiny::shinyApp(ui, server)
}

#' Painel de indicadores do DW (app Shiny autonoma)
#'
#' Lanca um painel leve de consulta ao DW de indicadores (aedidb): serie
#' temporal por nivel territorial e localidade, mapa coropletico municipal
#' e pagina Sobre. Nao abre o AEDi completo — util para conferir
#' rapidamente o que foi carregado, inclusive em servidor.
#'
#' Para materializar uma app deployavel dentro de um projeto, use
#' [deploy_panel()]; para obter so o objeto da app (hospedavel),
#' use [panel_app()].
#'
#' Conexao via variaveis de ambiente `user`/`password`/`host`/`dbname`
#' (mesmo padrao de [controle_con()]), com defaults locais.
#'
#' @param port porta HTTP; default escolhe uma porta livre
#' @param titulo titulo do topbar (default "Painel de Indicadores")
#' @param paleta paleta inicial, `"govbr"` (padrao) ou `"pb"` — ver
#'   [panel_app()]
#'
#' @export
run_panel <- function(port = NULL, titulo = "Painel de Indicadores",
                      paleta = c("govbr", "pb")) {
  app <- panel_app(titulo, paleta)
  if (!is.null(port)) app$options$port <- as.integer(port)
  app
}
