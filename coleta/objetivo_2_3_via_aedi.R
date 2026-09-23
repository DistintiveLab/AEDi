###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('rais_vlr_rem_dez_s38','max_massa_salarial_estadual')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),unused_fn=dplyr::first)
###Cria indicador
# sem values_fill: entrada faltante deve virar NA, nunca 0 (0 no denominador
# geraria Inf/0 silencioso)
dbdbase <- objetivo_2_3_via_aedi <- dbdbase |>
                     dplyr::rename(setNames(c('rais_vlr_rem_dez_s38','max_massa_salarial_estadual'), c('a','b'))) |>
                dplyr::mutate(b = ifelse(is.finite(b) & b != 0, b, NA_real_)) |>
                dplyr::transmute(objetivo_2_3_via_aedi = a / b,refdate,local_id)
