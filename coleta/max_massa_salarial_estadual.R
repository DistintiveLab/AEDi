###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
dbdbase <- max_massa_salarial_estadual <- dbdbase|>
                            dplyr::group_by(estado,refdate)|> dplyr::mutate(rais_vlr_rem_dez_s38= maxsna(rais_vlr_rem_dez_s38))|>dplyr::ungroup() |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38'), c('a'))) |>
                dplyr::transmute(max_massa_salarial_estadual = a,refdate,local_id)|> dplyr::ungroup()
