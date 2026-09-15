#DESPROD4 - cf relatorio monitoramento

salmedio_semadmpub_municipal <-
  salmedio_semadmpub_municipal|>
  dplyr::mutate(tx_cresc_sal=100*salario_medio_formal_sadmpub/lag(salario_medio_formal_sadmpub)-100)


###Escala produtiva complemento
pega_num_estabel_semadmp <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_estabelecimentos FROM rais_estabelecimento_",
                              ano," WHERE  (cnae_2_0_classe <84000 OR cnae_2_0_classe > 84999)   GROUP BY municipio")
  )
  a$ano <- ano
  a
}

estabelec_sem_admpub <- data.table::rbindlist(lapply(2013:2023,pega_num_estabel_semadmp))

readr::write_csv(estabelec_sem_admpub,"coleta/cache/estabelec_sem_admpu_mun/estabelec_sem_admpu_mun.csv")

escala_prod <- estabelec_sem_admpub|>
  dplyr::left_join(salmedio_semadmpub_municipal)

escala_prod <- escala_prod|>
  dplyr::mutate(desprod4_aedi = massa_salarial/qtd_estabelecimentos)

#Conferência
desprod4_aedi <- escala_prod|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   desprod4_aedi)

# readr::write_csv(desprod4_aedi|>
#   dplyr::rename(local=geoloc_id)|>
#   dplyr::left_join(locgeoloc|>
#                      dplyr::mutate(local=trunc(geoloc_id/10)))|>
#   dplyr::filter(!is.na(local_id))|>
#   dplyr::transmute(refdate,local_id,desprod4_aedi),
# 'coleta/cache/desprod4_aedi/desprod4_aedi1323.csv')


# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

desprod4_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'desprod4%'")

desprod4_compara <- desprod4_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(desprod4_aedi)

cor(desprod4_compara$value,desprod4_compara$desprod4_aedi,use='complete.obs')
#0.9968285
