###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('rais_remdezs38_media')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- rais_mediana_remmed_s38s <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('rais_remdezs38_media'), c('a'))) |>
                dplyr::transmute(rais_mediana_remmed_s38s =  median(a,na.rm=T),refdate,local_id)|> dplyr::ungroup()
