# Painel de indicadores do banco de dados do painel — esqueleto gerado por AEDi::deploy_panel()
# AEDi %%VERSAO%% · %%DATA%%. Este codigo pertence ao projeto: edite
# livremente (abas em R/mod_*.R e R/painel_ui.R, composicao em
# R/app_ui.R, marca em R/branding.R, dados em R/painel_dw.R).
# Credenciais do banco de dados (variaveis de ambiente): user, password, host, dbname.
# Basemap: CARTO_API_KEY habilita tiles Carto; sem ela, fundo neutro
# vetorial (padrao do labourvaluesdatapanel + contorno de UFs do IBGE).
# PAINEL_BASEMAP=carto|neutro forca a escolha.

library(shiny)  # modulos usam NS(), tagList(), reactive() etc. sem prefixo

# carrega todos os blocos da propria app (qualquer R/*.R do esqueleto —
# inclusive abas adicionadas pelo projeto depois do deploy)
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE))
  source(f, encoding = "UTF-8")

shiny::shinyApp(ui = app_ui(), server = app_server)
