# Chave de nivel territorial: municipios x regioes PNAD na largura 7 ----

test_that("painel_nivel_parse decodifica larguras e o subnivel PNAD", {
  p2 <- AEDi:::painel_nivel_parse("2")
  expect_identical(p2$nivel, 2L)
  expect_false(p2$pnad)
  expect_identical(p2$chave, "2")
  expect_identical(p2$filtro, "length(g.geoloc_id::text) = 2")

  p7 <- AEDi:::painel_nivel_parse(7L)
  expect_identical(p7$nivel, 7L)
  expect_false(p7$pnad)
  expect_identical(p7$chave, "7")
  expect_identical(
    p7$filtro,
    sprintf("length(g.geoloc_id::text) = 7 AND (l.local_id < %d OR l.local_id > %d)",
            AEDi:::painel_municipio_limite_id, AEDi:::painel_pnad_bloco_fim))

  pp <- AEDi:::painel_nivel_parse("7p")
  expect_identical(pp$nivel, 7L)
  expect_true(pp$pnad)
  expect_identical(pp$chave, "7p")
  expect_identical(
    pp$filtro,
    sprintf("length(g.geoloc_id::text) = 7 AND l.local_id >= %d AND l.local_id <= %d",
            AEDi:::painel_municipio_limite_id, AEDi:::painel_pnad_bloco_fim))
})

test_that("painel_nivel_parse aceita chave numerica e inteira", {
  expect_identical(AEDi:::painel_nivel_parse(2)$chave,
                   AEDi:::painel_nivel_parse("2")$chave)
  expect_identical(AEDi:::painel_nivel_parse(7L)$filtro,
                   AEDi:::painel_nivel_parse("7")$filtro)
})

test_that("painel_nivel_parse rejeita chave invalida sem gerar filtro", {
  for (chave in list(NA, NULL, "", "abc", "7x", "0")) {
    p <- AEDi:::painel_nivel_parse(chave)
    expect_identical(p$nivel, NA_integer_, info = paste(chave, collapse = ","))
    expect_identical(p$chave, NA_character_)
    expect_identical(p$filtro, "1 = 0")
  }
})

test_that("rotulo e fronteira do subnivel PNAD estao no lugar", {
  expect_identical(unname(AEDi:::painel_niveis_rotulo["7p"]),
                   "Região de interesse PNAD")
  expect_identical(AEDi:::painel_municipio_limite_id, 5571L)
})

test_that("guards dos caches com a nova chave retornam vazio sem tocar no banco", {
  expect_identical(AEDi:::painel_locais_nivel_cache(NA), integer(0))
  expect_identical(AEDi:::painel_locais_nivel_cache("abc"), integer(0))
  expect_identical(AEDi:::painel_locais_com_dados_cache(NA, "7p"),
                   character(0))
  expect_null(AEDi:::painel_local_top_cache(NA, "7p"))
  expect_identical(AEDi:::painel_ufs_com_dados_cache(NA, "7p"), character(0))
  expect_identical(nrow(AEDi:::painel_geo_nivel_cache(NA)), 0L)
})

test_that("painel_nivel_default continua no municipio com o nivel PNAD presente", {
  niveis <- data.frame(
    nivel_id = c("2", "7", "7p"),
    n_locais = c(27L, 5570L, 146L),
    rotulo = c("Unidade da Federação", "Município",
               "Região de interesse PNAD"))
  expect_identical(AEDi:::painel_nivel_default(niveis), "7")
})
