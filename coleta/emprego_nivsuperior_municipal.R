# emprego_nivsuperior_municipal (mdata 94): vinculos ativos com escolaridade
# superior por municipio. Fonte direta mte_rais. Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

pega_nivsup <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1 AND escolaridade_apos_2005 > 8",
           " GROUP BY municipio"))
  a$ano <- ano
  a
}

ns <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pega_nivsup))
readr::write_csv(ns, "coleta/cache/emprego_nivsuperior_municipal/emprego_nivsuperior_municipal.csv")

AEDi:::gravar_serie_dw("emprego_nivsuperior_municipal",
  data.frame(local = ns$local,
             periodo = as.Date(paste0(ns$ano, "-12-31")),
             valor = ns$qtd_vinculos))
DBI::dbDisconnect(rais)
