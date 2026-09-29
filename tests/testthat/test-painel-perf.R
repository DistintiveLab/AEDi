# Performance do painel: cache achatado, TTLs longos e banda de contexto
# (aquecimento pos-ETL; ver pndr_coord) ------------------------------------

test_that("TTL de valores sobe para 30 dias", {
  expect_identical(AEDi:::painel_cache_ttl[["valores"]], 24 * 30)
  expect_identical(AEDi:::painel_cache_ttl[["valores"]], 720)
  # geo e catalogo seguem nos patamares historicos
  expect_identical(AEDi:::painel_cache_ttl[["geo"]], 24 * 30)
  expect_identical(AEDi:::painel_cache_ttl[["catalogo"]], 24 * 7)
})

test_that("painel_valores_por_ano guarda NA sem tocar no banco", {
  v <- AEDi:::painel_valores_por_ano(NULL, NA_integer_)
  expect_identical(names(v), c("local_id", "refdate", "value", "ano"))
  expect_identical(nrow(v), 0L)
  expect_type(v$ano, "integer")
})

test_that("painel_filtrar_ano devolve o contrato historico", {
  # df como sai do SQL: 1 linha por localidade x ano (ultimo refdate)
  d <- data.frame(
    local_id = c(1L, 1L, 2L),
    refdate = as.Date(c("2020-06-01", "2021-06-01", "2021-06-01")),
    value = c(2, 4, 3),
    ano = c(2020L, 2021L, 2021L))
  f <- AEDi:::painel_filtrar_ano(d, 2020)
  expect_identical(f$local_id, 1L)
  expect_equal(f$value, 2)
  expect_identical(f$refdate, as.Date("2020-06-01"))
  expect_identical(names(f), c("local_id", "refdate", "value"))
  # filtro puro: passa todas as linhas do ano (dedupe e papel do SQL)
  f21 <- AEDi:::painel_filtrar_ano(d, 2021)
  expect_identical(nrow(f21), 2L)
  # ano sem dados: vazio tipado
  f2 <- AEDi:::painel_filtrar_ano(d, 1999)
  expect_identical(nrow(f2), 0L)
  expect_identical(names(f2), c("local_id", "refdate", "value"))
  expect_s3_class(f2$refdate, "Date")
  # entradas degeneradas nao quebram
  expect_identical(nrow(AEDi:::painel_filtrar_ano(NULL, 2020)), 0L)
  expect_identical(nrow(AEDi:::painel_filtrar_ano(d, NA)), 0L)
  sem_ano <- d; sem_ano$ano <- NULL
  expect_identical(nrow(AEDi:::painel_filtrar_ano(sem_ano, 2020)), 0L)
})

test_that("painel_plot_banda desenha banda, mediana e destaque", {
  d <- data.frame(
    local_id = rep(c(1L, 2L), each = 3),
    refdate = rep(as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")), 2),
    value = c(1, 2, 3, 5, 6, 7),
    ano = rep(c(2019L, 2020L, 2021L), 2))
  p <- AEDi:::painel_plot_banda(d, local_id = 1)
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(geoms, c("GeomRibbon", "GeomLine", "GeomLine", "GeomPoint"))
  # contexto agrega os dois locais por ano: min = menor serie
  ctx <- p$layers[[1]]$data
  expect_identical(ctx$Ano, c(2019L, 2020L, 2021L))
  expect_equal(ctx[["Mínimo"]], c(1, 2, 3))
  expect_equal(ctx[["Máximo"]], c(5, 6, 7))
  expect_equal(ctx[["Mediana"]], c(3, 4, 5))
  # destaque e a serie da localidade 1, com rotulo amigavel
  destaque <- p$layers[[4]]$data
  expect_identical(destaque$Ano, c(2019L, 2020L, 2021L))
  expect_equal(destaque$valor, c(1, 2, 3))
})

test_that("painel_plot_banda mantem datas e sobrevive a contexto vazio", {
  d <- data.frame(
    local_id = c(1L, 2L),
    refdate = as.Date(c("2020-01-01", "2020-07-01")),
    value = c(1, NA),
    ano = c(2020L, 2020L))
  # rotulo de tempo mensal: eixo x continua Date (sem agregacao por ano)
  p <- AEDi:::painel_plot_banda(d, local_id = 1, rotulo_x = "Mês")
  expect_s3_class(p$layers[[1]]$data$Mês, "Date")
  # NA nao entra no contexto: banda so com o valor finito
  expect_equal(p$layers[[1]]$data[["Mínimo"]], 1)
  # tudo NA: banda vazia sem erro
  dna <- d; dna$value <- NA_real_
  pna <- AEDi:::painel_plot_banda(dna, local_id = 1)
  expect_s3_class(pna, "ggplot")
  expect_identical(nrow(pna$layers[[1]]$data), 0L)
  # entrada NULL tambem e valida (cache ainda vazio)
  expect_s3_class(AEDi:::painel_plot_banda(NULL, local_id = 1), "ggplot")
})
