###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('datasus_popmun','maxpopestadual')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- primazia_populacional_estadual <- dbdbase |>
                     dplyr::rename(setNames(c('datasus_popmun','maxpopestadual'), c('a','b'))) |>
                dplyr::transmute(primazia_populacional_estadual = a / b,refdate,local_id)
