###Filtra e prepara dados base
#DBI::dbGetQuery(conrais,\"SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_2023 WHERE vinculo_ativo_31_12 = 1  AND TRUNC(cnae_2_0_classe/1000) = 72 GROUP BY municipio\")

empregos_cnae2citecm2013 <-
  dbGetQuery(rais,
             "SELECT municipio local, COUNT(*) qtd_vinculos_agr
             FROM rais_vinculo_2013 WHERE vinculo_ativo_31_12 = 1
             AND TRUNC(cnae_2_0_classe/1000) = 72 GROUP BY municipio")

empregos_cnae2citec_mun <- readr::read_csv("coleta/cache/empregos_cnae2citec_mun/empregos_cnae2citec_mun.csv")

empregos_cnae2citec_mun <-
  rbind(empregos_cnae2citecm2013|>dplyr::mutate(periodo=as.Date("2013-12-31")),
               empregos_cnae2citec_mun)

readr::write_csv(empregos_cnae2citec_mun,"coleta/cache/empregos_cnae2citec_mun/empregos_cnae2citec_mun20132023.csv")
 # dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name = 'datasus_popmun'")
 #
 # dbdbase <- dbdbase|>select(refdate,local_id,codigo_ibge,value)|>dplyr::mutate(refdate=as.Date(paste0(lubridate::year(refdate),"-12-31")))|>
 #   dplyr::filter(lubridate::year(refdate)>2013)

citec3_aedi <- datasus_popmun|>dplyr::mutate(ano=lubridate::year(refdate))|>
  dplyr::ungroup()|>dplyr::transmute(periodo=data.table::as.IDate(as.Date(paste0(ano,"-12-31"))),
                                     local,populacao)|>
  dplyr::left_join(empregos_cnae2citec_mun)

citec3_aedi[is.na(citec3_aedi$qtd_vinculos_agr),]$qtd_vinculos_agr <- 0

citec3_aedi <-
  citec3_aedi|>
  dplyr::mutate(
    citec3 = qtd_vinculos_agr*1e6/populacao
  )

citec3_aedi <-
  citec3_aedi|>dplyr::left_join(locgeoloc)
citec3_aedi <- citec3_aedi[!is.na(citec3_aedi$local),c('periodo','local_id','citec3')]

data.table::fwrite(citec3_aedi|>dplyr::transmute(refdate=periodo,local_id,citec3),'coleta/cache/citec3_aedi/citec3_aedi.csv')
#names(citec2_aedi) <- c('refdate','local_id','citec2_aedi')
citec3_orig <- dbGetQuery(mdr,
                          "select refdate,local_id,value citec3_base from data_values a
                          left join mdata b on a.mdata_id = b.mdata_id where
                          orig_name like 'citec3%'")
citec3_compara <-
  citec3_orig|>dplyr::mutate(refdate=data.table::as.IDate(refdate))|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(citec3_aedi|>dplyr::rename(refdate=periodo))|>
  dplyr::transmute(refdate,local_id,citec3_base,
                   citec3_aedi=ifelse(is.na(citec3),0,citec3))

cor(citec3_compara$citec3_base,citec3_compara$citec3_aedi,use="complete.obs")
#0.9971692
summary(citec3_compara)


#       tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
# ###Cria indicador
# dbdbase <- citec2_aedi <- dbdbase |>
#                      dplyr::rename(setNames(c('pequenas_empresas_biotecsaude_mun','datasus_popmun'), c('a','b'))) |>
#                 dplyr::transmute(citec2_aedi = 1e+06 * a / b,refdate,local_id)
