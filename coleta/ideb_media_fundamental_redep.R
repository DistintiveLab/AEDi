###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_anos_iniciais_redep','ideb_anos_finais_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- ideb_media_fundamental_redep <- dbdbase |>
                     dplyr::rename(setNames(c('ideb_anos_iniciais_redep','ideb_anos_finais_redep'), c('a','b'))) |>
                dplyr::transmute(ideb_media_fundamental_redep = ((a + b)) / 2,refdate,local_id)
