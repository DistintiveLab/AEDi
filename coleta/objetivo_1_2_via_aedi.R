###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_media_fundamental_redep')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- objetivo_1_2_via_aedi <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('ideb_media_fundamental_redep'), c('a'))) |>
                dplyr::transmute(objetivo_1_2_via_aedi = -  median(a,na.rm=T) + a,refdate,local_id)|> dplyr::ungroup()
