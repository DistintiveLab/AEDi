###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_remdezs38_media','rais_mediana_remmed_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- objetivo1_1_via_aedi_de_uma_vez_s <- dbdbase |>
                     dplyr::rename(setNames(c('rais_remdezs38_media','rais_mediana_remmed_s38'), c('a','b'))) |>
                dplyr::transmute(objetivo1_1_via_aedi_de_uma_vez_s = a - b,refdate,local_id)
