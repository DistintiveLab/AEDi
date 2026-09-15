###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('profissionais_de_saude_pc')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- objetivo1_3_viaedi <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('profissionais_de_saude_pc'), c('a'))) |>
                dplyr::transmute(objetivo1_3_viaedi = -  median(a,na.rm=T) + a,refdate,local_id)|> dplyr::ungroup()
