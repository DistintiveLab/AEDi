#salmedio_semadmpub_municipal_genero - pega dif genero

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


pegadifsal_genero <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT sexo_trabalhador sexo,municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND (cnae_2_0_classe <38000 OR cnae_2_0_classe > 38999)   GROUP BY municipio, sexo_trabalhador")
  )
  a$ano <- ano
  a
}

salmedio_semadmpub_municipal_genero <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegadifsal_genero))

#salmedio_semadmpub_municipal_genero[is.na(salmedio_semadmpub_municipal_genero)] <- 1

salmedio_semadmpub_municipal_genero <-salmedio_semadmpub_municipal_genero|>
  tidyr::pivot_wider(names_from=sexo,values_from=c(massa_salarial,qtd_vinculos_agr))|>
  dplyr::mutate(dessoc4=(massa_salarial_2/qtd_vinculos_agr_2)/(massa_salarial_1/qtd_vinculos_agr_1))





#Conferência
dessoc4_aedi <- salmedio_semadmpub_municipal_genero|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   dessoc4_aedi=dessoc4)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("dessoc4",
  data.frame(local = dessoc4_aedi$geoloc_id,
             periodo = dessoc4_aedi$refdate,
             valor = dessoc4_aedi$dessoc4_aedi))
DBI::dbDisconnect(rais)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

dessoc4_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'dessoc4%'")

dessoc4_compara <- dessoc4_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(dessoc4_aedi)

cor(dessoc4_compara$value,dessoc4_compara$dessoc4_aedi,use='complete.obs')
#0.839431

#0.9999807
summary(dessoc4_compara|>dplyr::transmute(refdate,local_id,dessoc4_base=value,
                                          dessoc4_aedi))
