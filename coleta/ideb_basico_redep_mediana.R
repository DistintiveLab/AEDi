###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_media_basico_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- ideb_basico_redep_mediana <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('ideb_media_basico_redep'), c('a'))) |>
                dplyr::transmute(ideb_basico_redep_mediana =  median(a,na.rm=T),refdate,local_id)|> dplyr::ungroup()
