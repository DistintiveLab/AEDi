###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('rais_remdezs38','rais_vinculos_s38')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- rais_remdezs38_medias <- dbdbase |>
                     dplyr::rename(setNames(c('rais_remdezs38','rais_vinculos_s38'), c('a','b'))) |>
                dplyr::transmute(rais_remdezs38_medias = a / b,refdate,local_id)
