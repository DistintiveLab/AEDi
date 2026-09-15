###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_anos_iniciais_redep','ideb_anos_finais_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- ideb_media_basico_redep <- dbdbase |>
                     dplyr::rename(setNames(c('ideb_anos_iniciais_redep','ideb_anos_finais_redep'), c('a1','a2'))) |>
                dplyr::transmute(ideb_media_basico_redep = rowMeans(dplyr::pick(dplyr::matches('^a[[:digit:]]*$')),na.rm=T),refdate,local_id)
