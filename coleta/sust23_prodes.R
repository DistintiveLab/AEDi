# sust2 (27) e sust3 (28): desmatamento PRODES/TerraBrasilis.
# Le os CSVs pre-processados (7 biomas, 2000-2024, 5570 municipios) em
# cache/sust2_aedi/desmatamento_ano_acumulado/. O TerraBrasilis fornece
# area anual por municipio; sust3 = km2/ano, sust2 = % acumulado (cap 100).
# Areas municipais do geobr 2022 em cache (shapemuns_e_area_total.rds).
# Padrao A5b (replace completo).

suppressMessages({library(data.table); library(dplyr); library(sf)})

# dados brutos do TerraBrasilis
arqs <- list.files("coleta/cache/sust2_aedi/desmatamento_ano_acumulado",
                   pattern = "\\.csv$", full.names = TRUE)
desmat <- rbindlist(lapply(arqs, fread))
cat("biomas:", length(arqs), "| linhas:", nrow(desmat),
    "| anos:", paste(range(desmat$year), collapse="-"), "\n")

# areas municipais (geobr 2022, do shapefile em cache)
map_mun <- readRDS("coleta/cache/sust2_aedi/shapemuns_e_area_total.rds")
areas <- map_mun |>
  st_drop_geometry() |>
  dplyr::transmute(geocode_ibge = code_muni,
                   area_mun_km2 = as.numeric(area_municipio) / 1e6)

# agregar por municipio-ano (soma dos biomas — municipios de transicao)
desmat_ano <- desmat |>
  dplyr::group_by(year, geocode_ibge) |>
  dplyr::summarise(area_km = sum(areakm, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(areas, by = "geocode_ibge") |>
  dplyr::filter(!is.na(area_mun_km2), area_mun_km2 > 0)

# sust3: taxa anual (km2/ano)
s3 <- desmat_ano |>
  dplyr::transmute(local = geocode_ibge,
                   periodo = as.Date(paste0(year, "-12-31")),
                   valor = area_km)

# sust2: % acumulado da area municipal (cap 100)
s2 <- desmat_ano |>
  dplyr::arrange(geocode_ibge, year) |>
  dplyr::group_by(geocode_ibge) |>
  dplyr::mutate(acum_km2 = cumsum(area_km)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local = geocode_ibge,
                   periodo = as.Date(paste0(year, "-12-31")),
                   valor = pmin(100, 100 * acum_km2 / area_mun_km2))

cat("sust2:", nrow(s2), "pontos | sust3:", nrow(s3), "pontos | anos:",
    paste(range(as.numeric(format(unique(s2$periodo), "%Y"))), collapse="-"), "\n")

AEDi:::gravar_serie_dw("sust2",
  data.frame(local = s2$local, periodo = s2$periodo, valor = s2$valor))
AEDi:::gravar_serie_dw("sust3",
  data.frame(local = s3$local, periodo = s3$periodo, valor = s3$valor))
