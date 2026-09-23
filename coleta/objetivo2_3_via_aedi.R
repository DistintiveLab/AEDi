###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('massa_salarial_municipal','max_massa_salarial_na_uf')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),unused_fn=dplyr::first)
###Cria indicador
# sem values_fill: entrada faltante deve virar NA (e NA no numerador ou no
# denominador da NA), nunca 0 -- 0 aqui geraria 0 ou Inf silencioso.
dbdbase <- objetivo2_3_via_aedi <- dbdbase |>
                     dplyr::rename(setNames(c('massa_salarial_municipal','max_massa_salarial_na_uf'), c('a','b'))) |>
                dplyr::mutate(b = ifelse(is.finite(b) & b != 0, b, NA_real_)) |>
                dplyr::transmute(objetivo2_3_via_aedi = a / b,refdate,local_id)

#Conferência
objetivo2_3_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo2_3%'")

obj2_3_compara <- objetivo2_3_orig|>
  dplyr::left_join(dbdbase)|>
  dplyr::transmute(refdate,local_id,obj2_3_base=value,obj2_3_via_aedi=objetivo2_3_via_aedi)

cor(obj2_3_compara$obj2_3_base,obj2_3_compara$obj2_3_via_aedi,use='complete.obs')
#0.9293505

summary(obj2_3_compara)
