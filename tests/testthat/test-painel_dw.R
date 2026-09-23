# Guards de id invalido e nivel default da aba Regiao ---------------

test_that("painel_valores_local_todos_cache com id invalido retorna vazio sem tocar no cache", {
  vazio <- AEDi:::painel_valores_local_todos_cache(NA)
  expect_identical(nrow(vazio), 0L)
  expect_identical(names(vazio), c("mdata_id", "refdate", "value"))
  expect_s3_class(vazio$refdate, "Date")
  expect_identical(nrow(AEDi:::painel_valores_local_todos_cache("abc")), 0L)
})

test_that("guards dos novos caches de geometria do globo retornam vazio sem tocar no cache", {
  geo <- AEDi:::painel_geo_nivel_cache(NA)
  expect_identical(nrow(geo), 0L)
  expect_identical(names(geo), c("code", "label", "geometry"))
  expect_identical(nrow(AEDi:::painel_geo_local_cache("abc")), 0L)
  expect_identical(nrow(AEDi:::painel_geo_pai_uf_cache(NA)), 0L)
  expect_identical(AEDi:::painel_ufs_com_dados_cache(NA, 7L), character(0))
  expect_null(AEDi:::painel_local_top_uf_cache(NA, 7L, 31L))
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

# Catalogo por convencao de orig_name (aba Regiao) --------------------

test_that("painel_grupo_indicador mapeia os 7 eixos, os 4 objetivos e os estratos PNAD", {
  g <- AEDi:::painel_grupo_indicador
  expect_identical(g("educ1"), "Eixo 1")
  expect_identical(g("educ4"), "Eixo 1")
  expect_identical(g("comp_educ"), "Eixo 1")
  expect_identical(g("citec3"), "Eixo 2")
  expect_identical(g("governativas4"), "Eixo 7")
  expect_identical(g("comp_governativas"), "Eixo 7")
  expect_identical(g("infra2"), "Eixo 4")
  expect_identical(g("objetivo3_2"), "Objetivo 3")
  expect_identical(g("comp_objetivo4"), "Objetivo 4")
  expect_identical(g("pnadc5"), "Estratos PNAD")
  expect_identical(g("comp_pnadc9"), "Estratos PNAD")
  expect_identical(g(c("educ1", "objetivo1_1")), c("Eixo 1", "Objetivo 1"))
})

test_that("painel_grupo_indicador descarta variacoes de trabalho e series de apoio", {
  g <- AEDi:::painel_grupo_indicador
  expect_true(is.na(g("objetivo1_2_via_aedi")))
  expect_true(is.na(g("comp_desprod_v0")))
  expect_true(is.na(g("comp_objetivo1_v0")))
  expect_true(is.na(g("citec1_aedi")))
  expect_true(is.na(g("datasus_popmun")))
  expect_true(is.na(g("ideb_anos_iniciais")))
  expect_true(is.na(g("massa_salarial_municipal")))
})

test_that("painel_grupo_raiz agrupa eixos, objetivos e mantem o resto", {
  r <- AEDi:::painel_grupo_raiz
  expect_identical(r("Eixo 1"), "Eixos")
  expect_identical(r("Eixo 7"), "Eixos")
  expect_identical(r("Objetivo 4"), "Objetivos")
  expect_identical(r("Estratos PNAD"), "Estratos PNAD")
})

test_that("painel_resumo_grupos vazio devolve tabela tipada", {
  vazio <- AEDi:::painel_resumo_grupos(NULL, NULL, NULL)
  expect_identical(nrow(vazio), 0L)
  expect_identical(names(vazio),
                   c("grupo", "raiz", "mdata_id", "rotulo", "valor", "refdate"))
  expect_s3_class(vazio$refdate, "Date")
  expect_identical(nrow(AEDi:::painel_resumo_grupos(
    data.frame(), data.frame(), data.frame())), 0L)
})

test_that("painel_resumo_grupos usa so compostos e o refdate mais recente", {
  hierarquia <- data.frame(
    raiz_id = c(13L, 13L, 14L),
    raiz_nome = c("Eixos", "Eixos", "Objetivos"),
    datagroup_id = c(1L, 1L, 9L),
    datagroup_name = c("Eixo 1", "Eixo 1", "Objetivo 1"),
    mdata_id = c(10L, 11L, 20L),
    data_class_id = c(2L, 4L, 4L),
    orig_name = c("educ1", "comp_educ", "comp_objetivo1"),
    data_name = c("Indicador bruto", "Composto do Eixo 1", NA_character_))
  compostos <- data.frame(mdata_id = c(11L, 20L),
                          orig_name = c("comp_educ", "comp_objetivo1"),
                          data_name = c("Composto do Eixo 1", NA_character_))
  valores <- data.frame(
    mdata_id = c(10L, 11L, 11L, 20L),
    refdate = as.Date(c("2020-01-01", "2019-01-01", "2021-01-01", "2018-01-01")),
    value = c(1, 2, 3, 4))
  r <- AEDi:::painel_resumo_grupos(hierarquia, compostos, valores)
  expect_identical(nrow(r), 2L)
  expect_identical(r$mdata_id, c(11L, 20L))
  expect_identical(r$grupo, c("Eixo 1", "Objetivo 1"))
  expect_identical(r$raiz, c("Eixos", "Objetivos"))
  expect_equal(r$valor, c(3, 4))
  expect_identical(r$refdate, as.Date(c("2021-01-01", "2018-01-01")))
  expect_identical(r$rotulo[1], "Composto do Eixo 1 — Eixo 1")
  expect_identical(r$rotulo[2], "comp_objetivo1 — Objetivo 1")
})

test_that("painel_resumo_grupos ignora composto sem valor finito", {
  hierarquia <- data.frame(
    raiz_id = 13L, raiz_nome = "Eixos", datagroup_id = 1L,
    datagroup_name = "Eixo 1", mdata_id = 11L, data_class_id = 4L,
    orig_name = "comp_educ", data_name = "Composto do Eixo 1")
  compostos <- data.frame(mdata_id = 11L, orig_name = "comp_educ",
                          data_name = "Composto do Eixo 1")
  valores <- data.frame(mdata_id = 11L, refdate = as.Date("2021-01-01"),
                        value = NA_real_)
  expect_identical(nrow(AEDi:::painel_resumo_grupos(
    hierarquia, compostos, valores)), 0L)
})

# Opcoes do selectize (aba Regiao) ------------------------------------

test_that("painel_opcoes_select eleva o maxOptions do selectize", {
  o <- AEDi:::painel_opcoes_select("Digite parte do nome")
  expect_identical(o$placeholder, "Digite parte do nome")
  expect_true(o$maxOptions > 1000L)
  expect_identical(AEDi:::painel_opcoes_select("x", 100L)$maxOptions, 100L)
})
