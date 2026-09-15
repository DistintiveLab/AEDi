###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('datasus_popmun')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- populacao_estadual_por_municipio <- dbdbase|>
                            dplyr::group_by(refdate,estado) |>
                     dplyr::rename(setNames(c('datasus_popmun'), c('a'))) |>
                dplyr::transmute(populacao_estadual_por_municipio = a,refdate,local_id)|> dplyr::ungroup()
