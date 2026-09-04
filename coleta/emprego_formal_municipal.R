# emprego_formal_municipal (mdata 92): vinculos ativos em 31/12 por municipio
# Fonte direta: mte_rais. Padrao A5b (anos dinamicos + recarga completa no DW).
# Base para desprod2/tx_crescimento/familia emprego agricola (via DW).

if (!exists("rais")) rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  dbname=Sys.getenv("mte_rais"), user="mte_rais",
  password=Sys.getenv("pwdrais"), host=Sys.getenv("hostraispsql"))

emprego_formal <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1 GROUP BY municipio"))
  a$ano <- ano
  a
}

efm <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), emprego_formal))
readr::write_csv(efm, "coleta/cache/emprego_formal_municipal/emprego_formal_municipal.csv")

AEDi:::gravar_serie_dw("emprego_formal_municipal",
  data.frame(local = efm$local,
             periodo = as.Date(paste0(efm$ano, "-12-31")),
             valor = efm$qtd_vinculos))
DBI::dbDisconnect(rais)
