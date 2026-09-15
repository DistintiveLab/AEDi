###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_anos_finais_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- media_ideb_basico <- dbdbase |>
                     dplyr::rename(setNames(c('ideb_anos_finais_redep'), c('a'))) |>
                dplyr::transmute(media_ideb_basico = mu * a,refdate,local_id)
