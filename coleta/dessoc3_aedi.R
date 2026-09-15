##dessoc3 - idadeserie

idade_serie_total_fundamental <- \(ano){
  educabR::le_idadeserie(ano)|>
    dplyr::filter(rede == 'Total',detalhe %in%
                    c('Total_Ensino Fundamental_Total',
                      "Total_Taxa de Distorção Idade-Série - Ensino Fundamental_Total Fundamental" ))|>
    dplyr::select(ano,codigo_municipio,valor)
}

dessoc3_aedi <- data.table::rbindlist(lapply(2013:2024,idade_serie_total_fundamental))

dessoc3_aedi1314 <- data.table::rbindlist(lapply(2013:2014,idade_serie_total_fundamental))



a <- dessoc3_aedi1314|>dplyr::bind_rows(dessoc3_aedi|>dplyr::mutate(ano=lubridate::year(ano)))|>
  dplyr::bind_rows(dessoc3_aedi24)|>
#  dplyr::filter(rede == 'Total')|>dplyr::filter(grepl("Total.*Fundamental_Total",detalhe))|>
  dplyr::select(ano,codigo_municipio,valor)

a$ano <- as.Date(paste0(a$ano,"-12-31"))


dessoc3_orig <- dbGetQuery(mdr,
                           "select refdate ano,geoloc_id codigo_municipio,value from
                           data_values a left join mdata b on a.mdata_id = b.mdata_id
                           left join local c on a.local_id = c.local_id where orig_name LIKE 'dessoc3%'")

dessoc3_compara <- dessoc3_orig|>
  dplyr::left_join(a)

cor(dessoc3_compara$value,dessoc3_compara$valor,use='complete.obs')
#1

data.table::fwrite(a,'coleta/cache/dessoc3_aedi/dessoc_3_aedi_comp1323.csv')
