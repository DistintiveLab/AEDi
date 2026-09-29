# Exploracao de indicadores: pares alinhados, correlacao e ficha por ano
# (backend puro, sem banco; ver pndr_coord/roadmap_exploracao_indicadores.md) ---

test_that("explorar_pares faz inner join por (local_id, ano) só com pares finitos", {
  sx <- data.frame(
    local_id = c(1L, 1L, 2L),
    ano = c(2019L, 2020L, 2019L),
    value = c(1, 2, NA_real_))
  sy <- data.frame(
    local_id = c(1L, 1L, 3L),
    ano = c(2019L, 2020L, 2019L),
    value = c(10, 20, 30))
  p <- AEDi:::explorar_pares(sx, sy)
  expect_identical(nrow(p), 2L)
  expect_identical(p$ano, c(2019L, 2020L))
  expect_identical(p$local_id, c(1L, 1L))
  expect_equal(p$x, c(1, 2))
  expect_equal(p$y, c(10, 20))
})

test_that("explorar_pares devolve vazio tipado", {
  sx <- data.frame(local_id = 1L, ano = 2019L, value = 1)
  v <- AEDi:::explorar_pares(sx[0, ], sx)
  expect_identical(names(v), c("local_id", "ano", "x", "y"))
  expect_identical(nrow(v), 0L)
  expect_type(v$local_id, "integer")
  expect_type(v$x, "double")
})

test_that("explorar_cor calcula Pearson e Spearman e mantém aviso NA", {
  p <- data.frame(local_id = 1L, ano = 2019L, x = 1:10, y = 2 * (1:10))
  r <- AEDi:::explorar_cor(p)
  expect_equal(r$n, 10L)
  expect_equal(r$pearson, 1)
  expect_equal(r$spearman, 1)
  expect_true(is.na(r$aviso))
})

test_that("explorar_cor sinaliza série degenerada", {
  p <- data.frame(local_id = 1L, ano = 2019L, x = 1:10, y = rep(5, 10))
  r <- AEDi:::explorar_cor(p)
  expect_true(is.na(r$pearson))
  expect_true(is.na(r$spearman))
  expect_true(grepl("degenerada", r$aviso))
})

test_that("explorar_cor exige no mínimo 3 pares completos", {
  p <- data.frame(local_id = 1L, ano = 2019L, x = c(1, 2), y = c(3, 4))
  r <- AEDi:::explorar_cor(p)
  expect_equal(r$n, 2L)
  expect_true(is.na(r$pearson))
  expect_true(grepl("3 pares", r$aviso))
})

test_that("explorar_ficha resume por ano ignorando NA e sem bandeiras", {
  s <- data.frame(
    local_id = c(1L, 1L, 2L, 2L),
    ano = c(2019L, 2020L, 2019L, 2020L),
    value = c(1, 2, NA_real_, 4))
  f <- AEDi:::explorar_ficha(s)
  expect_identical(nrow(f$por_ano), 2L)
  expect_equal(f$por_ano$n, c(1L, 2L))
  expect_identical(f$ultimo_ano, 2020L)
  expect_equal(f$n_locais, 2L)
  expect_false(f$degenerada)
  expect_false(f$cauda_pesada)
})

test_that("explorar_ficha marca série constante como degenerada", {
  s <- data.frame(
    local_id = c(1L, 1L, 2L),
    ano = c(2019L, 2020L, 2020L),
    value = c(5, 5, 5))
  f <- AEDi:::explorar_ficha(s)
  expect_true(f$degenerada)
  expect_true(is.na(f$por_ano$cv[1]))
})

test_that("explorar_ficha devolve NULL para série vazia", {
  expect_null(AEDi:::explorar_ficha(data.frame()))
  s <- data.frame(local_id = 1L, ano = 2019L, value = NA_real_)
  expect_null(AEDi:::explorar_ficha(s))
})

test_that("explorar_dicionario cruza o catálogo com classe e fonte", {
  corpo <- deparse(body(AEDi:::explorar_dicionario))
  expect_true(any(grepl("LEFT JOIN datasource", corpo, fixed = TRUE)))
  expect_true(any(grepl("LEFT JOIN data_class", corpo, fixed = TRUE)))
})
