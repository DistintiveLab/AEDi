###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38','rais_vinculos_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- rais_mediaremdez_s38 <- dbdbase |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38','rais_vinculos_s38'), c('a','b'))) |>
                dplyr::transmute(rais_mediaremdez_s38 = a / b,refdate,local_id)
