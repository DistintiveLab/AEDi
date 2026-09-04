#salmedio_semadmpub_municipal

# autocontencao (padrao A5b)
if (!exists("rais") || !inherits(rais, "DBIConnection")) rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  dbname=Sys.getenv("mte_rais"), user="mte_rais",
  password=Sys.getenv("pwdrais"), host=Sys.getenv("hostraispsql"))
if (!exists("con") || !inherits(con, "DBIConnection")) con <- DBI::dbConnect(RPostgres::Postgres(),
  user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
  host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))
if (!exists("mdr") || !inherits(mdr, "DBIConnection")) mdr <- con
if (!exists("locgeoloc") || !is.data.frame(locgeoloc)) locgeoloc <- DBI::dbGetQuery(con,
  "select local_id, local_name, geoloc_id from local")



pegasalmed_semadmpub <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND (cnae_2_0_classe <84000 OR cnae_2_0_classe > 84999)   GROUP BY municipio")
  )
  a$ano <- ano
  a
}

salmedio_semadmpub_municipal <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegasalmed_semadmpub))

salmedio_semadmpub_municipal <-salmedio_semadmpub_municipal|>
  dplyr::mutate(salario_medio_formal_sadmpub = massa_salarial/qtd_vinculos_agr)


salmedio_semadmpub_municipal$desprod3_aedi <- salmedio_semadmpub_municipal$salario_medio_formal_sadmpub

salmedio_semadmpub_municipal[is.na(salmedio_semadmpub_municipal)] <- 0

readr::write_csv(salmedio_semadmpub_municipal,"coleta/cache/salmedio_semadmpub_municipal/desprod3_aedi.csv")
#Conferência
desprod3_aedi <- salmedio_semadmpub_municipal|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   desprod3_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("desprod3",
  data.frame(local = desprod3_aedi$geoloc_id,
             periodo = desprod3_aedi$refdate,
             valor = desprod3_aedi$desprod3_aedi))
DBI::dbDisconnect(rais)

# readr::write_csv(desprod3_aedi|>
#   dplyr::rename(local=geoloc_id)|>
#   dplyr::left_join(locgeoloc|>
#                      dplyr::mutate(local=trunc(geoloc_id/10)))|>
#   dplyr::filter(!is.na(local_id))|>
#   dplyr::transmute(refdate,local_id,desprod3_aedi),
# 'coleta/cache/desprod3_aedi/desprod_aedi1323.csv')


# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

desprod3_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'desprod3%'")

desprod3_compara <- desprod3_orig|>dplyr::filter(refdate > '2012-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(desprod3_aedi)

cor(desprod3_compara$value,desprod3_compara$desprod3_aedi,use='complete.obs')
#0.9970939

