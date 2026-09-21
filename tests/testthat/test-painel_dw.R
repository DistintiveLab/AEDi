# Guards de id invalido e nivel default da aba Regiao ---------------

test_that("painel_valores_local_todos_cache com id invalido retorna vazio sem tocar no cache", {
  vazio <- AEDi:::painel_valores_local_todos_cache(NA)
  expect_identical(nrow(vazio), 0L)
  expect_identical(names(vazio), c("mdata_id", "refdate", "value"))
  expect_s3_class(vazio$refdate, "Date")
  expect_identical(nrow(AEDi:::painel_valores_local_todos_cache("abc")), 0L)
})

test_that("painel_nivel_default prefere municipio e cai na UF", {
  com_mun <- data.frame(nivel_id = c(1, 7), n_locais = c(5L, 5570L),
                        rotulo = c("Região", "Município"))
  expect_identical(AEDi:::painel_nivel_default(com_mun), "7")
  so_uf <- data.frame(nivel_id = 2, n_locais = 27L,
                      rotulo = "Unidade da Federação")
  expect_identical(AEDi:::painel_nivel_default(so_uf), "2")
  expect_identical(AEDi:::painel_nivel_default(NULL), "2")
  vazio <- data.frame(nivel_id = integer(0), n_locais = integer(0),
                      rotulo = character(0))
  expect_identical(AEDi:::painel_nivel_default(vazio), "2")
})
