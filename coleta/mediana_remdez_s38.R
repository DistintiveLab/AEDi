###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,SELECT * from named_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38')
dbdbase|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value')
###Cria indicador
dbdbase <- mediana_remdez_s38 <- dbdbase|> group_by(refdate)|>
                        summarize(across(where(is.numeric),somasna),across(where(is.character),first))|>
                        group_by(refdate) |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38'), c('a'))) |>
                dplyr::transmute(mediana_remdez_s38 =  median(a),refdate,local_id)
