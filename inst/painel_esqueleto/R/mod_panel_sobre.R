# Aba "Sobre" do painel de indicadores — pagina editorial minimalista,
# adaptada do padrao about do labourvaluesdatapanel: hero com acoes de
# navegacao, cartoes da equipe (painel_equipe) e boxes de apoio
# (painel_apoios), ambos multiplos via variaveis de ambiente. UI estatica
# (sem server); os botoes usam ids globais tratados no server do panel_app().

#' UI da aba Sobre do painel
#' @keywords internal
panel_sobre_ui <- function() {
  tagList(
    tags$div(id = "painel_raiz_sobre"),
    tags$section(class = "painel-sobre-hero", `aria-labelledby` = "painel_sobre_titulo",
      tags$p(class = "painel-kicker", "Sobre o painel"),
      tags$h1(id = "painel_sobre_titulo", "Indicadores do banco de dados do painel"),
      tags$p(class = "painel-lead",
        "Consulta pública às séries de indicadores gravadas no banco de",
        "dados do painel: escolha um recorte territorial — região, unidade",
        "da Federação, divisões regionais do IBGE ou município — e",
        "acompanhe a evolução de qualquer indicador, ou veja o mapa municipal",
        "do último ano disponível."),
      tags$div(class = "painel-acoes",
        shiny::actionButton("sobre_ver_regiao", "Ver séries regionais",
                            class = "btn-primary",
                            icon = shiny::icon("chart-line")),
        shiny::actionButton("sobre_ver_mapa", "Ver o mapa municipal",
                            icon = shiny::icon("map-location-dot")))),
    tags$section(class = "painel-sobre-secao", `aria-labelledby` = "painel_quem_titulo",
      tags$h2(id = "painel_quem_titulo", "Quem faz"),
      tags$div(class = "painel-pessoa", painel_brand_equipe())),
    tags$section(class = "painel-sobre-secao", `aria-labelledby` = "painel_apoio_titulo",
      tags$h2(id = "painel_apoio_titulo", "Apoio"),
      tags$div(class = "painel-apoios", painel_brand_apoios()))
  )
}

#' Acoes da aba Sobre (navegacao para as demais abas) — chamada no server
#' do panel_app()
#' @keywords internal
panel_sobre_server <- function(input, session) {
  shiny::observeEvent(input$sobre_ver_regiao, {
    shiny::updateNavbarPage(session, "painel_nav", selected = "regiao")
  })
  shiny::observeEvent(input$sobre_ver_mapa, {
    shiny::updateNavbarPage(session, "painel_nav", selected = "mapa")
  })
}
