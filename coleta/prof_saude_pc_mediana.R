###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('profissionais_de_saude_pc')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- prof_saude_pc_mediana <- dbdbase|>
                            dplyr::group_by(refdate) |>
                     dplyr::rename(setNames(c('profissionais_de_saude_pc'), c('a'))) |>
                dplyr::transmute(prof_saude_pc_mediana =  median(a,na.rm=T),refdate,local_id)|> dplyr::ungroup()
