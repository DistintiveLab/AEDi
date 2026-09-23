# Painel de indicadores do DW (app Shiny autonoma) --------------------------
#
# Estrutura adaptada do labourvaluesdatapanel: topbar com marca e acoes,
# navbarPage com abas realcadas, rodape fixo e paleta trocavel (gov.br ou
# preto e branco com o roxo da Distintive) persistida no navegador. A UI e
# composta pelos blocos de R/painel_ui.R e a marca vem de
# R/painel_branding.R — os mesmos arquivos copiados pelo
# deploy_panel(esqueleto = TRUE), garantindo uma unica fonte entre a app
# launcher e os esqueletos de projeto.

#' App do painel de indicadores (objeto shinyApp)
#'
#' Constroi o objeto `shinyApp` do painel de indicadores do DW (aedidb):
#' aba "Região" com série temporal por nível territorial e
#' localidade (região, UF, divisões do IBGE ou município) e globo
#' interativo de UFs, aba "Mapa" coroplético municipal com slider de
#' ano animado, paleta divergente centrada em 0 (invertível), ajuda por
#' indicador e atualização incremental (apenas cores e tooltips
#' atravessam a conexão após a primeira carga da geometria) e aba
#' "Sobre" com informações do autor e apoio da Distintive. Estrutura
#' visual adaptada do labourvaluesdatapanel: topbar com marca, abas
#' realçadas e rodapé.
#' E o motor por tras de [run_panel()] e da app launcher gerada por
#' [deploy_panel()] — como ultima expressao de um `app.R` hospedavel,
#' basta `AEDi::panel_app()`.
#'
#' Marca configuravel por variaveis de ambiente (sobrepoem os argumentos;
#' ver R/painel_branding.R): `painel_titulo`, `painel_subtitulo`,
#' `painel_paleta` e `painel_contato`.
#'
#' Conexao via variaveis de ambiente `user`/`password`/`host`/`dbname`
#' (mesmo padrao de [controle_con()]), com defaults locais.
#'
#' @param titulo titulo do topbar (default "Painel de Indicadores")
#' @param paleta paleta inicial: `"govbr"` (padrao, azul Gov.br) ou `"pb"`
#'   (preto e branco com toques do roxo da Distintive). O visitante pode
#'   trocar no botao do topo; a escolha fica salva no navegador
#' @param assets_dir diretorio com os assets do painel (CSS/JS); default
#'   o embutido no pacote (inst/painel) — o esqueleto gerado por
#'   [deploy_panel()] passa o proprio `www/` local
#'
#' @export
#' @importFrom shiny shinyApp
panel_app <- function(titulo = "Painel de Indicadores",
                      paleta = c("govbr", "pb"),
                      assets_dir = NULL) {
  paleta <- painel_brand_paleta(match.arg(paleta))
  if (is.null(assets_dir))
    assets_dir <- system.file("painel", package = "AEDi")
  titulo <- painel_brand_titulo(titulo)
  subtitulo <- painel_brand_subtitulo()
  ui <- shiny::tagList(
    painel_recursos(assets_dir, paleta),
    painel_topbar(titulo, subtitulo),
    do.call(shiny::navbarPage, c(
      list(title = shiny::tags$span(class = "painel-marca-mobile", titulo),
           id = "painel_nav", selected = "regiao", windowTitle = titulo,
           collapsible = TRUE, lang = "pt-BR"),
      painel_abas())),
    painel_aviso_carregando(),
    painel_rodape(painel_brand_contato()))
  shiny::shinyApp(ui, panel_server)
}

#' Server do painel (um mod_*_server por aba)
#' @keywords internal
panel_server <- function(input, output, session) {
  paleta_ativa <- shiny::reactive({
    if (identical(input$painel_paleta_ativa, "pb")) "pb" else "govbr"
  })
  mod_panel_regiao_server("panel_regiao_1", paleta = paleta_ativa)
  mod_panel_map_server("panel_map_1")
  mod_panel_baixar_server("panel_baixar_1")
  panel_sobre_server(input, session)
}

#' Painel de indicadores do DW (app Shiny autonoma)
#'
#' Lanca um painel leve de consulta ao DW de indicadores (aedidb): serie
#' temporal por nivel territorial e localidade com globo de UFs, mapa
#' coropletico municipal com slider de ano animado e atualizacao
#' incremental, e pagina Sobre. Nao abre o AEDi completo — util para
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
