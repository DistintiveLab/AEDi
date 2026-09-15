###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('emprego_formal_municipal')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
dbdbase <- nome_indicador <- dbdbase|>
                            dplyr::group_by(município,refdate)|> dplyr::mutate(emprego_formal_municipal= percentvar_sna(emprego_formal_municipal))|>dplyr::ungroup() |>
                     dplyr::rename(setNames(c('emprego_formal_municipal'), c('a'))) |>
                dplyr::transmute(nome_indicador = a,refdate,local_id)|> dplyr::ungroup()
