# Basemap do painel: tiles Carto com chave ou fundo neutro vetorial ------

ufs_exemplo <- function() {
  sf::st_sf(
    local_id = 1L, local_name = "X",
    geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
      crs = 4326))
}

test_that("painel_basemap_tipo segue CARTO_API_KEY e PAINEL_BASEMAP", {
  antigas <- Sys.getenv(c("CARTO_API_KEY", "PAINEL_BASEMAP"))
  on.exit(do.call(Sys.setenv, as.list(antigas)), add = TRUE)
  Sys.setenv(CARTO_API_KEY = "")
  Sys.unsetenv("PAINEL_BASEMAP")
  expect_identical(AEDi:::painel_basemap_tipo(), "neutro")
  Sys.setenv(CARTO_API_KEY = "chave-teste")
  expect_identical(AEDi:::painel_basemap_tipo(), "carto")
  Sys.setenv(PAINEL_BASEMAP = "neutro")
  expect_identical(AEDi:::painel_basemap_tipo(), "neutro")
  Sys.setenv(PAINEL_BASEMAP = "carto", CARTO_API_KEY = "")
  expect_warning(AEDi:::painel_basemap_tipo(), "sem CARTO_API_KEY")
})

test_that("modo carto anexa a chave como ?key= nos tiles voyager", {
  antigas <- Sys.getenv(c("CARTO_API_KEY", "PAINEL_BASEMAP"))
  on.exit(do.call(Sys.setenv, as.list(antigas)), add = TRUE)
  Sys.setenv(CARTO_API_KEY = "chave-teste", PAINEL_BASEMAP = "carto")
  mapa <- AEDi:::painel_basemap_adicionar(leaflet::leaflet())
  metodos <- vapply(mapa$x$calls, function(chamada) chamada$method,
                    character(1L))
  expect_identical(metodos, "addTiles")
  plano <- paste(unlist(mapa$x$calls[[1]]$args), collapse = " ")
  expect_match(plano, fixed("?key=chave-teste"))
  expect_match(plano, "basemaps\\.cartocdn\\.com.*voyager.*\\{z\\}/\\{x\\}/\\{y\\}\\.png")
  expect_match(plano, "abcd")
  expect_match(plano, "openstreetmap.org")
})

test_that("modo carto nao consulta o contorno de UFs", {
  antigas <- Sys.getenv(c("CARTO_API_KEY", "PAINEL_BASEMAP"))
  on.exit(do.call(Sys.setenv, as.list(antigas)), add = TRUE)
  Sys.setenv(CARTO_API_KEY = "chave-teste", PAINEL_BASEMAP = "carto")
  chamado <- FALSE
  mapa <- AEDi:::painel_basemap_adicionar(
    leaflet::leaflet(),
    contorno_uf = function() { chamado <<- TRUE; ufs_exemplo() })
  expect_false(chamado)
})

test_that("modo neutro fica sem tiles e contorna as UFs quando dao", {
  antigas <- Sys.getenv(c("CARTO_API_KEY", "PAINEL_BASEMAP"))
  on.exit(do.call(Sys.setenv, as.list(antigas)), add = TRUE)
  Sys.setenv(CARTO_API_KEY = "", PAINEL_BASEMAP = "neutro")

  chamado <- FALSE
  mapa <- AEDi:::painel_basemap_adicionar(
    leaflet::leaflet(),
    contorno_uf = function() { chamado <<- TRUE; NULL })
  expect_true(chamado)
  expect_identical(length(mapa$x$calls), 0L)

  mapa <- AEDi:::painel_basemap_adicionar(
    leaflet::leaflet(), contorno_uf = function() ufs_exemplo())
  metodos <- vapply(mapa$x$calls, function(chamada) chamada$method,
                    character(1L))
  expect_identical(metodos, c("addMapPane", "addPolygons"))
  expect_false(any(metodos %in% c("addTiles", "addProviderTiles")))
  expect_identical(mapa$x$calls[[1]]$args[[1]], "painel-contorno-uf")
  expect_true(450 %in% unlist(mapa$x$calls[[1]]$args))
})
