# Basemap do mapa municipal. O Carto passou a exigir chave de API nos
# tiles (anexada como ?key= nas chamadas), entao o fundo depende de duas
# variaveis de ambiente: CARTO_API_KEY (chave do Carto) e PAINEL_BASEMAP
# ("carto" ou "neutro", para forcar uma das opcoes). Com chave, o mapa usa
# os rastertiles voyager do Carto; sem chave, vale o padrao do
# labourvaluesdatapanel: fundo neutro vetorial sem tiles, complementado
# pelo contorno das UFs da malha do IBGE (via DW).

#' Chave de API do Carto (variavel de ambiente CARTO_API_KEY); NULL sem chave
#' @keywords internal
painel_carto_chave <- function() {
  chave <- trimws(Sys.getenv("CARTO_API_KEY"))
  if (nzchar(chave)) chave else NULL
}

#' Basemap vigente: "carto" quando ha chave, senao "neutro" (sem tiles,
#' padrao do labourvaluesdatapanel). PAINEL_BASEMAP=carto sem chave cai no
#' neutro com aviso.
#' @keywords internal
painel_basemap_tipo <- function() {
  forca <- tolower(trimws(Sys.getenv("PAINEL_BASEMAP")))
  chave <- painel_carto_chave()
  if (forca == "neutro") return("neutro")
  if (is.null(chave)) {
    if (forca == "carto")
      warning("PAINEL_BASEMAP=carto sem CARTO_API_KEY definida: usando ",
              "o fundo neutro (sem tiles).")
    return("neutro")
  }
  "carto"
}

#' Adiciona o basemap ao mapa leaflet: tiles voyager do Carto com a chave
#' em ?key= quando disponivel, ou fundo neutro sem tiles (padrao do
#' labourvaluesdatapanel) com o contorno das UFs em um pane acima da
#' camada municipal. `contorno_uf` e uma funcao sem argumentos que devolve
#' as geometrias das UFs (por exemplo `painel_geo_uf_cache`): so e
#' chamada no modo neutro, para nao consultar o DW a toa quando ha tiles.
#' @keywords internal
painel_basemap_adicionar <- function(mapa, contorno_uf = NULL) {
  if (identical(painel_basemap_tipo(), "carto")) {
    return(leaflet::addTiles(
      mapa,
      urlTemplate = paste0(
        "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/",
        "{z}/{x}/{y}.png?key=", painel_carto_chave()),
      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attributions">CARTO</a>',
      options = leaflet::providerTileOptions(subdomains = "abcd",
                                             maxZoom = 20)))
  }
  limites <- if (is.function(contorno_uf))
    tryCatch(contorno_uf(), error = function(e) NULL) else contorno_uf
  if (is.null(limites) || NROW(limites) == 0L) return(mapa)
  mapa |>
    leaflet::addMapPane("painel-contorno-uf", zIndex = 450) |>
    leaflet::addPolygons(
      data = limites, fill = FALSE, weight = 1.2, color = "#6f7883",
      opacity = 0.9,
      options = leaflet::pathOptions(pane = "painel-contorno-uf",
                                     interactive = FALSE))
}
