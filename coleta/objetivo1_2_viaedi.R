###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_media_basico_redep','ideb_basico_redep_mediana')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- objetivo1_2_viaedi <- dbdbase |>
                     dplyr::rename(setNames(c('ideb_media_basico_redep','ideb_basico_redep_mediana'), c('a','b'))) |>
                dplyr::transmute(objetivo1_2_viaedi = a - b,refdate,local_id)
