###Filtra e prepara dados base
#   DBI::dbGetQuery(conrais,\"SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM
#   rais_vinculo_2014 WHERE vinculo_ativo_31_12 = 1  AND cbo_ocupacao_2002 IS NOT NULL
#   AND TRUNC(cbo_ocupacao_2002/1000) IN (203, 234,395)  GROUP BY municipio\")"

empregocitecmun2013 <- dbGetQuery(
  rais,
  "SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM
     rais_vinculo_2013 WHERE vinculo_ativo_31_12 = 1  AND cbo_ocupacao_2002 IS NOT NULL
     AND TRUNC(cbo_ocupacao_2002/1000) IN (203, 234,395)  GROUP BY municipio"
     )

empregos_citec_mun <-
  readr::read_csv("coleta/cache/emprego_citec_mun/emprego_citec_mun.csv")

empregos_citec_mun <-
  rbind(empregocitecmun2013|>dplyr::mutate(periodo=as.Date("2013-12-31")),
        empregos_citec_mun)

readr::write_csv(empregos_citec_mun,"coleta/cache/emprego_citec_mun/emprego_citec_mun20132023.csv")

#popmun <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name = 'datasus_popmun'")

# popmun <- popmun|>select(refdate,local_id,codigo_ibge,value)|>
#   dplyr::mutate(refdate=as.Date(paste0(lubridate::year(refdate),"-12-31")))|>
#   dplyr::filter(lubridate::year(refdate)>2013)

citec2_aedi <- datasus_popmun|>dplyr::mutate(ano=lubridate::year(refdate))|>
  dplyr::ungroup()|>dplyr::transmute(periodo=data.table::as.IDate(as.Date(paste0(ano,"-12-31"))),
                              local,populacao)|>
  dplyr::left_join(empregos_citec_mun)

citec2_aedi[is.na(citec2_aedi$qtd_vinculos_agr),]$qtd_vinculos_agr <- 0

citec2_aedi <- citec2_aedi[periodo>'2012-12-31',]

citec2_aedi <-
  citec2_aedi|>
  dplyr::mutate(
    citec2 = qtd_vinculos_agr*1e6/populacao
  )


citec2_aedi <- citec2_aedi[!is.na(citec2_aedi$local),c('periodo','local','citec2')]
readr::write_csv(citec2_aedi,"coleta/cache/citec2_aedi/citec2_aedi.csv")

#names(citec2_aedi) <- c('refdate','local_id','citec2_aedi')
citec2_orig <- dbGetQuery(mdr,
                          "select refdate,local_id,value citec2_base from data_values a
                          left join mdata b on a.mdata_id = b.mdata_id where
                          orig_name like 'citec2%'")
citec2_compara <-
  citec2_orig|>dplyr::mutate(refdate=data.table::as.IDate(refdate))|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(citec2_aedi|>dplyr::rename(refdate=periodo))|>
  dplyr::transmute(refdate,local_id,citec2_base,
                   citec2_aedi=ifelse(is.na(citec2),0,citec2))

cor(citec2_compara$citec2_base,citec2_compara$citec2_aedi,use="complete.obs")
#0,98142
#      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
# dbdbase <- citec2_aedi <- dbdbase |>
#                      dplyr::rename(setNames(c('pequenas_empresas_biotecsaude_mun','datasus_popmun'), c('a','b'))) |>
#                 dplyr::transmute(refdate,local_id,citec2_aedi = 1e+06 * a / b)
