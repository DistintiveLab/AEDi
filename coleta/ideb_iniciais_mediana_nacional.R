###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_media_basico_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- ideb_iniciais_mediana_nacional <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('ideb_media_basico_redep'), c('a'))) |>
                dplyr::transmute(ideb_iniciais_mediana_nacional =  median(a,na.rm=T),refdate,local_id)|> dplyr::ungroup()
