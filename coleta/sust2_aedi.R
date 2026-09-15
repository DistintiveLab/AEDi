

map_mun <- geobr::read_municipality(year=2020)

map_mun <- map_mun |>dplyr::mutate(area_mun=sf::st_area(geom))|>sf::st_drop_geometry()

desmatamento_com_acumulado <- list.files("coleta/cache/sust2_aedi/desmatamento_ano_acumulado/",pattern="*.csv",full.names = TRUE)


todos_desmatamento <- data.table::rbindlist(
  lapply(desmatamento_com_acumulado,\(x){data.table::fread(x)})
)


desmat_ano <- todos_desmatamento|>
  dplyr::group_by(year,geocode_ibge)|>
  dplyr::summarise(area_desmatamento=sum(areakm,na.rm = TRUE)
  )|>
  dplyr::ungroup()|>
  dplyr::left_join(map_mun,by=c("geocode_ibge"="code_muni"))|>
  dplyr::mutate(prop_desmatamento = 100*area_desmatamento*1e6/as.vector(area_mun))


desmat_ano <- desmat_ano|>
  dplyr::mutate(prop_desmatamento=ifelse(prop_desmatamento>100,100,prop_desmatamento))|>
  dplyr::group_by(geocode_ibge)|>
  dplyr::mutate(
    desmatamento_acumulado = ifelse(cumsum(prop_desmatamento)>100,100,cumsum(prop_desmatamento)))




readr::write_csv(desmat_ano,'coleta/cache/sust2_aedi/sust2_aedi_max100.csv')

sust2_orig <- dbGetQuery(mdr,"select refdate,geoloc_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id left join local c on a.local_id=c.local_id where orig_name like 'sust2%'")


sust2_compara <- sust2_orig|>
  dplyr::mutate(year=lubridate::year(refdate))|>
  dplyr::left_join(desmat_ano|>dplyr::rename(geoloc_id=geocode_ibge))

cor(sust2_compara$value,sust2_compara$desmatamento_acumulado,use='complete.obs')
#0.992024
#0.992039 <- max 100

summary(sust2_compara|>dplyr::transmute(refdate,geoloc_id,sust2_base=value,sust2_aedi=desmatamento_acumulado))

sust3_orig <- dbGetQuery(mdr,"select refdate,geoloc_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id left join local c on a.local_id=c.local_id where orig_name like 'sust3%'")

sust3_compara <- sust3_orig|>
  dplyr::mutate(year=lubridate::year(refdate))|>
  dplyr::left_join(desmat_ano|>dplyr::rename(geoloc_id=geocode_ibge))

cor(sust3_compara$value,sust3_compara$area_desmatamento,use='complete.obs')

#0.9966295

