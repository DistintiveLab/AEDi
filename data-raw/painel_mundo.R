# Contorno mundial simplificado para o globo do painel de indicadores.
#
# Gera inst/painel/painel-mundo.geojson a partir do banco "world" do pacote
# maps (distribuido com o R, sem download externo): 253 poligonos preenchidos
# simplificados em dTolerance 1 grau e coordenadas com 1 casa decimal (~134
# KB). Nesta escala (o globo inteiro em ~320 px), 1 grau ~ 1 px: nada de
# detalhe se perde visualmente e o asset cabe no pacote e nas apps geradas
# por deploy_panel(). O globo busca o arquivo por URL
# (painel_recursos/painel-mundo.geojson) e funciona mesmo sem ele.

sf::sf_use_s2(FALSE)
mundo <- maps::map("world", plot = FALSE, fill = TRUE)
sfobj <- sf::st_as_sf(mundo)
sf::st_crs(sfobj) <- sf::st_crs(4326)
geom <- sf::st_make_valid(sf::st_geometry(sfobj))
geom <- suppressWarnings(
  sf::st_simplify(geom, dTolerance = 1, preserveTopology = TRUE))

destino <- file.path("inst", "painel", "painel-mundo.geojson")
sf::st_write(sf::st_sf(geometry = geom), destino, delete_dsn = TRUE,
             quiet = TRUE, layer_options = "COORDINATE_PRECISION=1")
cat(destino, "-", round(file.size(destino) / 1024), "KB,", length(geom),
    "features\n")
