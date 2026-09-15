###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('emprego_agricola_sobre_total_mun','empregoagricola_prop_nacional')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
dbdbase <- objetivo4_1_via_aedi <- dbdbase |>
                     dplyr::rename(setNames(c('emprego_agricola_sobre_total_mun','empregoagricola_prop_nacional'), c('a','b'))) |>
                dplyr::transmute(objetivo4_1_via_aedi = a / b,refdate,local_id)

###Conferência
objetivo4_1_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo4_1%'")


objetivo4_1_aedi <- dbGetQuery(con,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo4_1_via_aedi%'")

objetivo4_1_compara <-
  objetivo4_1_orig|>
  dplyr::left_join(objetivo4_1_aedi|>dplyr::rename(aedi=value))
cor(objetivo4_1_compara$value,objetivo4_1_compara$aedi,use='complete.obs')
# [1] 0.5322782
# 0.9267027
