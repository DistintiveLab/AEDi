###Filtra e prepara dados base
#  dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('pequenas_empresas_biotecsaude_mun','datasus_popmun')")
# dbdbase <- dbdbase|>dplyr::filter(lubridate::year(refdate)>2014)|>
#     dplyr::mutate(data_freq_id=max(data_freq_id),refdate=as.Date(paste0(lubridate::year(refdate),"-12-31")))|>
#       tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
# ###Cria indicador
# dbdbase <- citec1_aedi <- dbdbase |>
#                      dplyr::rename(setNames(c('pequenas_empresas_biotecsaude_mun','datasus_popmun'), c('a','b'))) |>
#                 dplyr::transmute(citec1_aedi = 1e+06 * a / b,refdate,local_id)|>
#   transmute(local=local_id,valor=citec1_aedi,periodo=refdate)


peqemp_biotecsaude_mun <- data.table::rbindlist(
  lapply(2013:2024,
         \(x){
           peq <- dbGetQuery(rais,
                             paste0("select municipio, COUNT(*) estabelecimento FROM
                                    rais_estabelecimento_",
                             x," WHERE tamanho_estabelecimento < 6 AND
                             TRUNC(CNAE_2_0_CLASSE/100) IN (211,212,266,325)
                             GROUP BY municipio"))
           peq$ano <- x
           peq
         })
)

readr::write_csv(peqemp_biotecsaude_mun,"coleta/cache/pequenas_empresas_biotecsaude_mun/pequenas_empresas_biotecsaude_mun20132023.csv")
#datasuspop_mun <- massalmun|>dplyr::select(ano,local,datasus_popmun)

citec1_aedi <-
  datasuspop_mun|>dplyr::transmute(
    ano,
    municipio=local,
    pop=datasus_popmun)|> dplyr::ungroup()|>
  dplyr::left_join(peqemp_biotecsaude_mun)|>
    dplyr::transmute(
      refdate=as.Date(paste0(ano,"-12-31")),
      local=municipio,
      citec1_aedi=ifelse(is.na(estabelecimento),0,estabelecimento)*1e6/pop
      )

citec1_aedi[is.na(citec1_aedi)] <- 0

readr::write_csv(citec1_aedi|>dplyr::filter(refdate>'2012-12-31')|>dplyr::left_join(locgeoloc|>dplyr::transmute(local=trunc(geoloc_id/10),local_id))|>
                   dplyr::transmute(refdate,local_id,citec1_aedi),'coleta/cache/citec1_aedi/citec1_aedi.csv')


# mdatarais <- data.frame(
#   orig_name='citec1_aedi',
#   data_name= 'Micro e pequenas empresas relacionadas ao setor de biotecnologia e saúde humana',
#   data_desc="auto import rais - check source code"
# )
#
# mdata_extrais <- data.frame(
#   data_class_id=1,
#   data_freq_id=mmfreq,
#   data_type_id=1,
#   datasource_id=1,
#   data_url='raispsql estabelecimento tamanho_estabelecimento < 6 AND TRUNC(CNAE_2_0_CLASSE/100) IN (211,212,266,325)'
# )
#
# db_datawrite(
#   list(mdatarais,mdata_extrais),dbdbase,
#   construct = 'raispsql estabelecimento tamanho_estabelecimento < 6 AND TRUNC(CNAE_2_0_CLASSE/100) IN (211,212,266,325) *1e6/datasus_popmun',
#   sanitize = F)


#Conferência
citec1_orig <- dbGetQuery(mdr,
                          "select refdate,local_id,value citec1_base from data_values a
                          left join mdata b on a.mdata_id = b.mdata_id where
                          orig_name like 'citec1%'")

citec1_compara <-
  citec1_orig|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(citec1_aedi)|>
  dplyr::transmute(refdate,local_id,citec1_base,citec1_aedi=
                     ifelse(is.na(citec1_aedi),0,citec1_aedi))

cor(citec1_compara$citec1_base,citec1_compara$citec1_aedi,use='complete.obs')
#0.9907606
summary(citec1_compara)
