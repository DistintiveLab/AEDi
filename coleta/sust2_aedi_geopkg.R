#https://terrabrasilis.dpi.inpe.br/downloads/
prodes_ult <- "https://terrabrasilis.dpi.inpe.br/download/dataset/cerrado-prodes/vector/prodes_cerrado_nb.gpkg.zip"
download.file(prodes_ult,'coleta/cache/sust2_aedi/prodes_brasil_2023.zip')


arqsgeopackages <- list.files('/home/wlvdbaj/pRojetos/AEDi/coleta/cache/sust2_aedi',pattern="*.gpkg$",full.names = TRUE)
arqsgeopackages <- rev(arqsgeopackages)

camadas <- sf::st_layers(arqsgeopackages[1])
camadas_pantanal <- sf::st_layers(arqsgeopackages[1])

def2000 <-
  mapply(arqsgeopackages,
         \(x,y) {sf::st_read(x,layer = y)},
         arqsgeopackages,paste0('accumulated_deforestation_200',c(rep(0,5),7),
                                c(rep("",5),"_biome")))

def2000 <- rbind(def2000[[1]],def2000[[2]])

desmatanual <- mapply(\(x,y) {
  sf::st_read(x,layer = y)},
  arqsgeopackages,paste0('yearly_deforestation',c(rep("",5),"_biome")))

desmatanual <- rbind(desmatanual[[1]],
                     desmatanual[[2]],desmatanual[[3]],desmatanual[[4]],desmatanual[[5]])


map_mun <- geobr::read_municipality(year=2022,simplified=FALSE)

map_mun <- map_mun|>dplyr::select(code_muni)

map_mun$area_municipio <- sf::st_area(map_mun)

arqmapmup <- 'coleta/cache/sust2_aedi/shapemuns_e_area_total.rds'
saveRDS(map_mun,arqmapmup)
desmatanual <- sf::st_make_valid(desmatanual)



junta_um_bioma <- \(x) {
  arqmapmup <- 'coleta/cache/sust2_aedi/shapemuns_e_area_total.rds'
  map_mun <- readRDS(arqmapmup)
  map_mun <- sf::st_make_valid(map_mun)
  map_mun <- map_mun|>sf::st_buffer(100)
  arquivo <- arqsgeopackages[x]
  print(x)
  camadas <- sf::st_layers(arquivo)
  camada_obj <- camadas$name[grepl("yearly_deforestation($|_b)",camadas$name)]
  bioma <- sf::st_read(arquivo,camada_obj)
  bioma <- sf::st_make_valid(bioma)
  sf::st_join(
    bioma,
    map_mun,join = sf::st_within)|>
    sf::st_drop_geometry()|>
    dplyr::group_by(year,code_muni)|>
    dplyr::summarise(area_desmatada_km2 = sum(area_km))
}

desmatanual_pampa <- junta_um_bioma(2)

desmatanual_mata_atlantica <- junta_um_bioma(3)

desmatanual_cerrado <- junta_um_bioma(4)

desmatanual_caatinga <- junta_um_bioma(5)

desmatanual_pantanal <- junta_um_bioma(1)

desmatanual_amazonia <- junta_um_bioma(6)

library(doParallel)
cl <- makeCluster(3,'FORK')
registerDoParallel(cl)

desmata_j <-
  data.table::rbindlist(parLapply(cl,
    2:4,
    junta_um_bioma)
  )



sust2_aedi <- desmatanual|>sf::st_drop_geometry(desmatanual)|>
  dplyr::mutate(area_municipio=as.numeric(area_municipio)/1e6)|>
  dplyr::group_by(year,code_muni)|>
  dplyr::summarise(area_desmatada_km2 = sum(area_km),area_municipio=first(area_municipio))|>
  dplyr::ungroup()|>
  dplyr::group_by(code_muni)|>
  dplyr::mutate(area_desmatada_km2=cumsum(area_desmatada_km2))|>
  dplyr::mutate(sust2_aedi=area_desmatada_km2/area_municipio)|>
  dplyr::ungroup()



