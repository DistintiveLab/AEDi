###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- mediana_remdez_s38rais <- dbdbase|> dplyr::group_by(refdate,local_id)|>dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38'), c('a'))) |>
                dplyr::transmute(mediana_remdez_s38rais =  median(a),refdate,local_id)
###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- mediana_remdez_s38rais <- dbdbase|> dplyr::group_by(refdate,local_id)|>dplyr::group_by(local_id) |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38'), c('a'))) |>
                dplyr::transmute(mediana_remdez_s38rais =  median(a),refdate,local_id)
