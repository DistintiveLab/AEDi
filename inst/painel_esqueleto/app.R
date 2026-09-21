# Painel de indicadores do DW — esqueleto gerado por AEDi::deploy_panel()
# AEDi %%VERSAO%% · %%DATA%%. Este codigo pertence ao projeto: edite
# livremente (abas em R/mod_*.R e R/painel_ui.R, composicao em
# R/app_ui.R, marca em R/branding.R, dados em R/painel_dw.R).
# Credenciais do DW (variaveis de ambiente): user, password, host, dbname.

library(shiny)  # modulos usam NS(), tagList(), reactive() etc. sem prefixo

for (f in c(
  "R/branding.R",
  "R/painel_cache.R",
  "R/painel_dw.R",
  "R/painel_ui.R",
  "R/mod_panel_globe.R",
  "R/mod_panel_map.R",
  "R/mod_panel_regiao.R",
  "R/mod_panel_sobre.R",
  "R/app_ui.R",
  "R/app_server.R"
)) source(f, encoding = "UTF-8")

shiny::shinyApp(ui = app_ui(), server = app_server)
