# empregoformal_agricola_municipal (mdata 98): vinculos ativos na agricultura
# (CNAE classe < 4000) por municipio. Fonte direta mte_rais. Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

pega_agricola <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1 AND cnae_2_0_classe < 4000",
           " GROUP BY municipio"))
  a$ano <- ano
  a
}

ag <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pega_agricola))
readr::write_csv(ag, "coleta/cache/empregoformal_agricola_municipal/empregoformal_agricola_municipal.csv")

AEDi:::gravar_serie_dw("empregoformal_agricola_municipal",
  data.frame(local = ag$local,
             periodo = as.Date(paste0(ag$ano, "-12-31")),
             valor = ag$qtd_vinculos))
DBI::dbDisconnect(rais)
