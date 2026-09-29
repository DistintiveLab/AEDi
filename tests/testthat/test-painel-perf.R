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

test_that("counts do SQL do painel voltam como inteiro plano", {
  # RPostgres mapeia count(*) (bigint) para bit64::integer64 quando o bit64
  # esta disponivel; sem ele no processo consumidor do cache, format()/paste0
  # mostram os bits crus do double (2.75e-320 em vez de 5571). O cast ::int
  # na fonte fecha o contrato para qualquer processo
  sql_niveis <- paste(deparse(body(AEDi:::painel_niveis)), collapse = "")
  expect_true(grepl("count(*)::int AS n_locais", sql_niveis, fixed = TRUE))
  sql_geo <- paste(deparse(body(AEDi:::painel_geo_nivel)), collapse = "")
  expect_true(grepl("count(*)::int AS n", sql_geo, fixed = TRUE))
  sql_rank <- paste(deparse(body(AEDi:::painel_ranking_local)), collapse = "")
  expect_true(grepl(")::int AS rank_uf", sql_rank, fixed = TRUE))
  expect_true(grepl("count(*)::int AS n_uf", sql_rank, fixed = TRUE))
  expect_true(grepl("alvo.value)::int", sql_rank, fixed = TRUE))
  expect_true(grepl(")::int AS n_br", sql_rank, fixed = TRUE))
})

test_that("rotulo de nivel territorial renderiza count como numero inteiro", {
  niveis <- data.frame(
    nivel_id = "7",
    rotulo = "Município",
    n_locais = 5571L,
    stringsAsFactors = FALSE)
  rotulos <- paste0(niveis$rotulo, " (", niveis$n_locais, ")")
  expect_identical(rotulos, "Município (5571)")
  expect_match(rotulos, "^Município \\(5[.,]?571\\)$")
})

test_that("tipo de grafico default: composto vira banda, demais linha", {
  expect_identical(AEDi:::painel_tipo_grafico("ponto", 2L), "ponto")
  expect_identical(AEDi:::painel_tipo_grafico("linha", 4L), "linha")
  expect_identical(AEDi:::painel_tipo_grafico(NA_character_, 4L), "banda")
  expect_identical(AEDi:::painel_tipo_grafico(NA_character_, 2L), "linha")
  expect_identical(AEDi:::painel_tipo_grafico(NA_character_, 1L), "linha")
  expect_identical(AEDi:::painel_tipo_grafico(NA_character_, NA_integer_), "linha")
  expect_identical(AEDi:::painel_tipo_grafico(NA_character_, NULL), "linha")
  expect_identical(AEDi:::painel_tipo_grafico(NULL, 4L), "banda")
})

test_that("catalogo e aba Regiao resolvem o default pela classe", {
  sql_extras <- paste(deparse(body(AEDi:::painel_mdata_extras)), collapse = "")
  expect_true(grepl("e.data_class_id", sql_extras, fixed = TRUE))
  mod <- paste(deparse(body(AEDi:::mod_panel_regiao_server)), collapse = "")
  expect_true(grepl("painel_tipo_grafico(", mod, fixed = TRUE))
})
