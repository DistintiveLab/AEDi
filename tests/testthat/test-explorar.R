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

test_that("explorar_leituras projeta between e within por localidade", {
  p <- data.frame(
    local_id = c(1L, 1L, 2L, 3L),
    ano = c(2019L, 2020L, 2019L, 2020L),
    x = c(1, 3, 10, 7),
    y = c(2, 6, 20, 14))
  expect_identical(AEDi:::explorar_leituras(p, "pooled"), p)
  expect_identical(AEDi:::explorar_leituras(p, "outra"), p)
  b <- AEDi:::explorar_leituras(p, "between")
  expect_identical(nrow(b), 3L)
  expect_equal(b$x[b$local_id == 1L], 2)
  expect_equal(b$y[b$local_id == 2L], 20)
  expect_true(all(is.na(b$ano)))
  w <- AEDi:::explorar_leituras(p, "within")
  expect_identical(nrow(w), 2L)
  expect_equal(w$x[w$local_id == 1L & w$ano == 2019L], -1)
  expect_equal(w$y[w$local_id == 1L & w$ano == 2020L], 2)
})

test_that("explorar_wide empilha por full outer join (nunca listwise)", {
  s1 <- data.frame(local_id = 1L, ano = c(2019L, 2020L), value = c(1, 2))
  s2 <- data.frame(local_id = c(1L, 2L), ano = c(2020L, 2020L),
                   value = c(3, 4))
  w <- AEDi:::explorar_wide(list("11" = s1, "22" = s2))
  expect_identical(nrow(w), 3L)
  expect_setequal(names(w), c("local_id", "ano", "11", "22"))
  expect_true(is.na(w[["11"]][w$local_id == 2L]))
  expect_null(AEDi:::explorar_wide(list()))
})

test_that("explorar_degeneradas marca colunas de dispersão nula", {
  w <- data.frame(local_id = 1L, ano = 2019L,
                  a = c(1, 1), b = c(1, 2))
  d <- AEDi:::explorar_degeneradas(w)
  expect_true(d[["a"]])
  expect_false(d[["b"]])
})

test_that("explorar_matriz calcula r e n pairwise por leitura", {
  w <- data.frame(
    local_id = c(1L, 1L, 2L, 2L),
    ano = c(2019L, 2020L, 2019L, 2020L),
    a = c(1, 2, 3, 4),
    b = c(2, 4, 6, 8),
    c = c(10, 10, 10, 10))
  m <- AEDi:::explorar_matriz(w)
  expect_equal(m$r["a", "b"], 1)
  expect_equal(m$n["a", "b"], 4)
  expect_true(is.na(m$r["a", "c"]))
  mb <- AEDi:::explorar_matriz(w, "between")
  expect_equal(mb$n["a", "b"], 2)
  expect_equal(mb$r["a", "b"], 1)
  mw <- AEDi:::explorar_matriz(w, "within")
  expect_equal(mw$n["a", "b"], 4)
  expect_equal(mw$r["a", "b"], 1)
})

test_that("explorar_matriz conta pares completos com NA no wide", {
  w <- data.frame(local_id = c(1L, 1L, 2L),
                  ano = c(2019L, 2020L, 2019L),
                  a = c(1, 2, NA), b = c(2, NA, 5))
  m <- AEDi:::explorar_matriz(w)
  expect_equal(m$n["a", "b"], 1)
  expect_true(is.na(m$r["a", "b"]))
})

test_that("explorar_preditores ranqueia por |r| decrescente", {
  ids <- c("7", "8", "9")
  mat <- list(
    r = matrix(c(1, .2, -.8, .2, 1, .5, -.8, .5, 1), 3, 3,
               dimnames = list(ids, ids)),
    n = matrix(4, 3, 3, dimnames = list(ids, ids)))
  rk <- AEDi:::explorar_preditores(mat, "7")
  expect_identical(nrow(rk), 2L)
  expect_identical(rk$mdata_id, c("9", "8"))
  expect_equal(rk$r[1], -0.8)
  expect_identical(nrow(AEDi:::explorar_preditores(mat, "404")), 0L)
})

test_that("explorar_badge_n classifica o n de pares", {
  expect_true(grepl("3 pares", AEDi:::explorar_badge_n(2)))
  expect_true(grepl("insuficiente", AEDi:::explorar_badge_n(10)))
  expect_true(is.na(AEDi:::explorar_badge_n(30)))
  expect_true(is.na(AEDi:::explorar_badge_n(NA)))
})
