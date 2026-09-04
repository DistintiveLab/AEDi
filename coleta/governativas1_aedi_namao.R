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



pegadirigente_municipal <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND trunc(cbo_ocupacao_2002/100) IN (1112,1114) GROUP BY municipio")
  )
  a$ano <- ano
  a
}

pegadirigente_municipal_cursosuperior <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND trunc(cbo_ocupacao_2002/100) IN (1112,1114) AND escolaridade_apos_2005> 8 GROUP BY municipio")

  )
  a$ano <- ano
  a
}

dirigentesmunicipais <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegadirigente_municipal))

saveRDS(dirigentesmunicipais,"coleta/cache/rais_dirigentesmunicipais/2013_2024_dirigentesmunicipais_rais.rds")

csuperior_dirigentesmunicipais <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegadirigente_municipal_cursosuperior))

saveRDS(csuperior_dirigentesmunicipais,"coleta/cache/rais_dirigentesmunicipais/2013_2024_comcursosuperior_dirigentesmunicipais_rais.rds")


governativas1_aedi <- dirigentesmunicipais|>
  dplyr::left_join(csuperior_dirigentesmunicipais|>dplyr::rename(qtd_vinculos_agr_csuperior=qtd_vinculos_agr,
                                                                 massa_salarial_sup=massa_salarial))

governativas1_aedi[is.na(governativas1_aedi)] <- 0

governativas1_aedi$governativas1_aedi <- 100*governativas1_aedi$qtd_vinculos_agr_csuperior/
  governativas1_aedi$qtd_vinculos_agr


#Conferência
governativas1_aedic <- governativas1_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   gov1_aedi=governativas1_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("governativas1",
  data.frame(local = governativas1_aedic$geoloc_id,
             periodo = governativas1_aedic$refdate,
             valor = governativas1_aedic$gov1_aedi))

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas1_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas1%'")

gov1_compara <- governativas1_orig|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(governativas1_aedic)

cor(gov1_compara$value,gov1_compara$gov1_aedi,use='complete.obs')
#0.9936931




pegadirigente_municipalcnae <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe = 84116 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

pegadirigente_municipal_cursosuperiorcnae <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe = 84116 AND escolaridade_apos_2005> 8 GROUP BY municipio")

  )
  a$ano <- ano
  a
}


dirigentesmunicipaiscnae <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegadirigente_municipalcnae))

csuperior_dirigentesmunicipaiscnae <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegadirigente_municipal_cursosuperiorcnae))


governativas2_aedi <- dirigentesmunicipaiscnae|>
  dplyr::left_join(csuperior_dirigentesmunicipaiscnae|>dplyr::rename(qtd_vinculos_agr_csuperior=qtd_vinculos_agr,
                                                                     massa_salarial_sup=massa_salarial))

governativas2_aedi[is.na(governativas2_aedi)] <- 0

governativas2_aedi$governativas2_aedi <- 100*governativas2_aedi$qtd_vinculos_agr_csuperior/
  governativas2_aedi$qtd_vinculos_agr


#Conferência
governaivas2_aedic <- governativas2_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   gov2_aedi=governativas2_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("governativas2",
  data.frame(local = governaivas2_aedic$geoloc_id,
             periodo = governaivas2_aedic$refdate,
             valor = governaivas2_aedic$gov2_aedi))

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas2_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas2%'")

gov2_compara <- governativas2_orig|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(governaivas2_aedic)

cor(gov2_compara$value,gov2_compara$gov2_aedi,use='complete.obs')
#0.91539
summary(gov2_compara|>dplyr::transmute(refdate,local_id,gov2_base=value,gov2_aedi))

governativas3_aedi <- governativas2_aedi|>
  dplyr::transmute(local,ano,governativas3_aedi=massa_salarial/qtd_vinculos_agr)


#Conferência gov3
gov3aedic <- governativas3_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   gov3_aedi=governativas3_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("governativas3",
  data.frame(local = gov3aedic$geoloc_id,
             periodo = gov3aedic$refdate,
             valor = gov3aedic$gov3_aedi))
DBI::dbDisconnect(rais)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas3_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas3%'")

gov3_compara <- governativas3_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(gov3aedic)

cor(gov3_compara$value,gov3_compara$gov3_aedi,use='complete.obs')
#
#0.9855483


