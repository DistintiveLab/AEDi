###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_mediaremdez_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- rais_mediana_remedez_s38 <- dbdbase |>
                     dplyr::rename(setNames(c('rais_mediaremdez_s38'), c('a'))) |>
                dplyr::transmute(rais_mediana_remedez_s38 =  median(a),refdate,local_id)
