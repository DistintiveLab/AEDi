###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_remdezs38_media')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- rais_mediana_remed_s38 <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('rais_remdezs38_media'), c('a'))) |>
                dplyr::transmute(rais_mediana_remed_s38 =  median(a),refdate,local_id)|> dplyr::ungroup()
