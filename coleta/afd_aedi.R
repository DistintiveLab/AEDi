#afd_aedi

pega_aedi <- \(ano) {
  educabR::le_afd(ano)|>
    dplyr::filter(indicador_afd=='grupo_1')|>
    dplyr::transmute(refdate=as.Date(paste0(ano,"-12-31")),
                     geoloc_id=codigo_municipio,
                     afd_aedi=valor)
}

afd_aedi <- data.table::rbindlist(
  lapply(2013:2024,pegaedi)
)

saveRDS(afd_aedi,'coleta/cache/afd/afd_aedi_2013_2024.rds')




educ3_orig <-DBI::dbGetQuery(mdr,"select
                                refdate,geoloc_id,value from data_values a left join local b ON a.local_id = b.local_id left join mdata c on a.mdata_id = c.mdata_id where orig_name LIKE 'educ3%'")


educ3_compara <-
  educ3_orig|>
  dplyr::left_join(afd_aedi)

cor(educ3_compara$value,educ3_compara$afd_aedi,use='complete.obs')
#1
