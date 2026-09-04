#emprego_industrial_municipal

# autocontencao (padrao A5b): conexoes e objetos de sessao
if (!exists("rais") || !inherits(rais, "DBIConnection")) rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  dbname=Sys.getenv("mte_rais"), user="mte_rais",
  password=Sys.getenv("pwdrais"), host=Sys.getenv("hostraispsql"))
if (!exists("con") || !inherits(con, "DBIConnection")) con <- DBI::dbConnect(RPostgres::Postgres(),
  user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
  host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))
if (!exists("mdr") || !inherits(mdr, "DBIConnection")) mdr <- con
if (!exists("locgeoloc") || !is.data.frame(locgeoloc)) locgeoloc <- DBI::dbGetQuery(con,
  "select local_id, local_name, geoloc_id from local")



pegaindustria <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe BETWEEN 10000 AND 33999 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

emprego_industria_municipal <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegaindustria))

empregoformalmun <- dbGetQuery(con,
                               "SELECT trunc(geoloc_id/10) local,value, extract('year' from refdate) ano
                                 from data_values a left join mdata b on a.mdata_id = b.mdata_id
                                 left join local c on a.local_id = c.local_id where orig_name like 'emprego_formal_mun%' and extract('year' from refdate) > 2012")

readr::write_csv(emprego_industria_municipal,"coleta/cache/emprego_industria_mun/empregoformal_industria_mun1324.csv")

desprod2_aedi <-
  empregoformalmun|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(emprego_industria_municipal)|>
  dplyr::mutate(refdate=as.Date(paste0(ano,"-12-31")))


desprod2_aedi$desprod2_aedi <- desprod2_aedi$qtd_vinculos_agr/desprod2_aedi$value

desprod2_aedi[is.na(desprod2_aedi)] <- 0

# serie no DW (recalculo completo, padrao A5b; dedup local-periodo contra
# fan-out do join com locgeoloc)
.serie_desprod2 <- data.frame(local = desprod2_aedi$local,
                              periodo = desprod2_aedi$refdate,
                              valor = desprod2_aedi$desprod2_aedi)
.serie_desprod2 <- .serie_desprod2[!duplicated(.serie_desprod2[, c("local", "periodo")]), ]
AEDi:::gravar_serie_dw("desprod2", .serie_desprod2)
DBI::dbDisconnect(rais)

#Conferência
# desprod2_comp <- emprego_industria_municipal|>
#   dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
#                    geoloc_id=local,
#                    desprod2_aedi)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

desprod2_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'desprod2%'")

desprod2_compara <- desprod2_orig|>dplyr::filter(refdate > '2012-12-31')|>
  dplyr::left_join(desprod2_aedi|>dplyr::rename(empfmun=value))|>
  dplyr::mutate(across(desprod2_aedi,\(x){ifelse(is.na(x),0,100*x)}))

cor(desprod2_compara$value,desprod2_compara$desprod2_aedi,use='complete.obs')
#0.9980446

