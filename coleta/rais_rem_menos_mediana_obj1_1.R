###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_remdezs38_media')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- rais_rem_menos_mediana_obj1_1 <- dbdbase |>
                     dplyr::rename(setNames(c('rais_remdezs38_media'), c('a'))) |>
                dplyr::transmute(rais_rem_menos_mediana_obj1_1 = - a + a,refdate,local_id)
