###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('empregoformal_agricola_municipal','emprego_formal_municipal')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
dbdbase <- especializacao_emprego_agricola_nacional <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('empregoformal_agricola_municipal','emprego_formal_municipal'), c('a','b'))) |>
                dplyr::transmute(especializacao_emprego_agricola_nacional = a / b,refdate,local_id)|> dplyr::ungroup()
