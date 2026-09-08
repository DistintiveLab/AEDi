# IDEB + Censo Escolar: atualizacao das series-base (append do ultimo refdate).
# Fonte IDEB 2025: download.inep.gov.br/ideb/resultados/divulgacao_*_2025.zip
#   (CDN exige TLS relaxado + HTTP/1.1; parser espelha educabR::le_ideb:
#    cabecalho nas linhas 8:10, dados a partir da 11; rede "Publica";
#    coluna vl_observado_<ano>)
# Fonte Censo Escolar 2025: dados_abertos/microdados_censo_escolar_2025_.zip
#   (nome com sublinhada final!), tabela Tabela_Escola_2025_V2.csv (dialecto
#   sep=';' dec=',' Latin-1; chaves NU_ANO_CENSO/CO_MUNICIPIO(7d)/
#   IN_ESGOTO_REDE_PUBLICA/IN_INTERNET)
# Derivados: recalcular ideb_derivados_recalc.R depois deste script.
