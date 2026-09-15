###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- rais_remdez_s38_mediana <- dbdbase|> dplyr::group_by(refdate)|>dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38'), c('a'))) |>
                dplyr::transmute(rais_remdez_s38_mediana =  median(a),refdate,local_id)|> dplyr::ungroup()
