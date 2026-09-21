# Server do painel: um mod_*_server por aba (espelha R/mod_*.R). Para uma
# aba nova, crie R/mod_nova.R e chame mod_nova_server("nova_1") aqui.

app_server <- function(input, output, session) {
  paleta_ativa <- shiny::reactive({
    if (identical(input$painel_paleta_ativa, "pb")) "pb" else "govbr"
  })
  mod_panel_regiao_server("panel_regiao_1", paleta = paleta_ativa)
  mod_panel_map_server("panel_map_1")
  panel_sobre_server(input, session)
}
