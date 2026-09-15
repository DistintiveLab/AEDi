#salmedio_semadmpub_municipal_genero - pega dif genero

#
# rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                        dbname=Sys.getenv("mte_rais"),
#                        user="mte_rais",
#                        password=Sys.getenv("pwdrais"),
#                        host=Sys.getenv("hostraispsql"))



pegaservidor_municipal <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND (cnae_2_0_classe = 84116)   GROUP BY municipio")
  )
  a$ano <- ano
  a
}

pegaservidor_municipal_cursosuperior <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr_csuperior,SUM(vl_remun_dezembro_nom) massa_salarial_sup FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND (cnae_2_0_classe = 84116)  AND escolaridade_apos_2005> 8 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

servidoresmunicipais <-
  data.table::rbindlist(lapply(2013:2024,pegaservidor_municipal))

saveRDS(servidoresmunicipais,"coleta/cache/rais_servidoresmunicipais/2013_2024_servidoresmunicipais_rais.rds")


csuperior_servidoresmunicipais <-
  data.table::rbindlist(lapply(2013:2024,pegaservidor_municipal_cursosuperior))

saveRDS(servidoresmunicipais,"coleta/cache/rais_servidoresmunicipais/2013_2024_servidoresmunicipais_rais.rds")

governativas2_aedi <- servidoresmunicipais|>
  dplyr::left_join(csuperior_servidoresmunicipais)

governativas2_aedi[is.na(governativas2_aedi)] <- 0

governativas2_aedi$governativas2_aedi <- 100*governativas2_aedi$qtd_vinculos_agr_csuperior/
  governativas2_aedi$qtd_vinculos_agr

governativas3_aedi <- governativas2_aedi|>
  dplyr::transmute(local,ano,governativas3_aedi=massa_salarial/qtd_vinculos_agr)

#Conferência
teste_aedi <- governativas2_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   gov2_aedi=governativas2_aedi)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas2_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas2%'")

gov2_compara <- governativas2_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(teste_aedi)

cor(gov2_compara$value,gov2_compara$gov2_aedi,use='complete.obs')
#0.91539


#Conferência gov3
teste_aedi <- governativas3_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   gov3_aedi=governativas3_aedi)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas3_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas3%'")

gov3_compara <- governativas3_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(teste_aedi)

cor(gov3_compara$value,gov3_compara$gov3_aedi,use='complete.obs')
#0.9855483


