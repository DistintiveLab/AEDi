

dessoc2 <- data.table::fread("coleta/cache/dessoc2_aedi/visdata3-download-02-05-2025 003336.csv")

#popmun - dbGetQuery(mdr,'select refdate,local_id,geoloc_id codigo_ibge, value from data_values aleft join mdata b on a.mdata_id = b.mdata_id left join local c on a.local_id = c.local_id where orig_name LIKE 'datasus_pop%')
# apos mutate refdate para dezembro
loc_geoloc <- popmun|>dplyr::distinct(local_id,codigo_ibge)

dessoc2_aedi <- dessoc2|>
  dplyr::filter(grepl("^12",Referência))|>
  dplyr::transmute(refdate = as.Date(paste0("31/",Referência),tryFormats= "%d/%m/%Y"),
            codigo_ibge = Código,
            pessoas_fam_ate_1sm = `Quantidade de pessoas cadastradas em famílias com renda total mensal até 1 salário mínimo`
  )|>
  dplyr::left_join(popmunicipal|>dplyr::transmute(refdate= as.Date(paste0(lubridate::year(refdate),'-12-31')) ,codigo_ibge=local,populacao))

dessoc2_aedi <-dessoc2_aedi|>
  dplyr::transmute(refdate,codigo_ibge,dessoc2_aedi = pessoas_fam_ate_1sm/populacao)

dessoc2_orig <- DBI::dbGetQuery(mdr,"select refdate,trunc(geoloc_id/10) codigo_ibge,value from data_values a left join mdata b on a.mdata_id = b.mdata_id left join local c on a.local_id = c.local_id where orig_name LIKE 'dessoc2%'")

dessoc2_compara <- dessoc2_orig|>
  dplyr::left_join(dessoc2_aedi)

cor(dessoc2_compara$value,dessoc2_compara$dessoc2_aedi,use='complete.obs')
#0.9980992
