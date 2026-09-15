###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('rais_vinculos_s38')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
dbdbase <- objetivo_3_1_via_aedi <- dbdbase|>
                            dplyr::group_by(município,refdate)|> dplyr::mutate(rais_vinculos_s38= percentvar_sna(rais_vinculos_s38))|>dplyr::ungroup() |>
                     dplyr::rename(setNames(c('rais_vinculos_s38'), c('a'))) |>
                dplyr::transmute(objetivo_3_1_via_aedi = a,refdate,local_id)|> dplyr::ungroup()
