# Malha municipal por UF do globo (painel_geo_mun_uf) -------------------

test_that("painel_geo_mun_uf valida o prefixo da UF antes de consultar", {
  geo <- AEDi:::painel_geo_mun_uf
  for (uf in list(NA, NULL, "3", "310", "ab", "")) {
    vazio <- geo(NULL, uf)
    expect_identical(nrow(vazio), 0L, info = paste("uf:", uf))
    expect_identical(names(vazio), c("code", "label", "geometry"))
  }
})

test_that("guarda do cache da malha devolve vazio sem tocar no banco", {
  expect_identical(nrow(AEDi:::painel_geo_mun_uf_cache(NA)), 0L)
  expect_identical(nrow(AEDi:::painel_geo_mun_uf_cache("abc")), 0L)
  expect_identical(names(AEDi:::painel_geo_mun_uf_cache(NA)),
                   c("code", "label", "geometry"))
})

test_that("painel_geo_mun_uf traz os municipios da UF (MG, prefixo 31)", {
  todos <- AEDi:::painel_com_con(function(con) AEDi:::painel_geo_mun_uf(con, "31"))
  expect_identical(nrow(todos), 853L)
  expect_identical(names(todos), c("code", "label", "geometry"))
  expect_type(todos$code, "character")
  expect_true("2245" %in% todos$code)  # Abadia dos Dourados (MG)
})

test_that("o cache da malha exclui o selecionado e e compartilhado por UF", {
  malha <- AEDi:::painel_geo_mun_uf_cache(2245L)
  expect_identical(nrow(malha), 852L)
  expect_false("2245" %in% malha$code)
  vizinho <- as.integer(malha$code[1])
  outra <- AEDi:::painel_geo_mun_uf_cache(vizinho)
  expect_identical(nrow(outra), 852L)
  expect_true("2245" %in% outra$code)
  expect_false(as.character(vizinho) %in% outra$code)
})

test_that("localidade inexistente devolve malha vazia", {
  expect_identical(nrow(AEDi:::painel_geo_mun_uf_cache(999999L)), 0L)
})
