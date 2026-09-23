# Opcoes do seletor de indicadores agrupadas por eixo/objetivo -------

md_opcoes <- function() {
  data.frame(
    mdata_id = c(1L, 2L, 3L, 4L, 10L, 11L, 12L, 20L, 21L, 30L, 40L, 41L, 50L, 60L),
    orig_name = c("educ2", "educ10", "educ1", "comp_educ",
                  "objetivo2_2", "objetivo2_1", "comp_objetivo2",
                  "objetivo4_1", "comp_objetivo4",
                  "comp_pnadc10", "pnadc2", "pnadc10",
                  "ideb_basico", "comp_objetivo2_v0"),
    data_name = c("Distorção", "Dez", "Cadastro Único", "Composto Educação",
                  "Salário médio", "Emprego formal", "Composto Objetivo 2",
                  "Renda", "Composto Objetivo 4",
                  "PNAD Eixo 3", "Estrato 2", "Estrato 10",
                  NA_character_, "v0"))
}

test_that("painel_opcoes_indicador agrupa na ordem eixo, objetivo, PNAD e demais", {
  op <- AEDi:::painel_opcoes_indicador(md_opcoes())
  expect_identical(names(op), c("Eixo 1", "Objetivo 2", "Objetivo 4",
                                "Estratos PNAD", "Demais indicadores"))
  # grupo multi-membro vira vetor nomeado de ids: o selectize trata como
  # optgroup quando length > 1
  expect_type(op[[1]], "character")
  expect_identical(unlist(op, use.names = FALSE),
                   as.character(c(4L, 3L, 1L, 2L, 12L, 11L, 10L, 21L, 20L,
                                  40L, 30L, 41L, 60L, 50L)))
})

test_that("composto abre o grupo e os componentes seguem numerados", {
  op <- AEDi:::painel_opcoes_indicador(md_opcoes())
  expect_identical(names(op[["Eixo 1"]])[1], "Composto Educação (comp_educ)")
  expect_identical(names(op[["Eixo 1"]]),
    c("Composto Educação (comp_educ)",
      "Indicador 1 - Cadastro Único (educ1)",
      "Indicador 2 - Distorção (educ2)",
      "Indicador 10 - Dez (educ10)"))
  expect_identical(names(op[["Objetivo 2"]]),
    c("Composto Objetivo 2 (comp_objetivo2)",
      "Indicador 1 - Emprego formal (objetivo2_1)",
      "Indicador 2 - Salário médio (objetivo2_2)"))
})

test_that("Estratos PNAD ordenam pelo numero do estrato", {
  op <- AEDi:::painel_opcoes_indicador(md_opcoes())
  expect_identical(names(op[["Estratos PNAD"]]),
    c("Indicador 2 - Estrato 2 (pnadc2)",
      "PNAD Eixo 3 (comp_pnadc10)",
      "Indicador 10 - Estrato 10 (pnadc10)"))
})

test_that("demais indicadores ficam alfabeticos e sem prefixo", {
  op <- AEDi:::painel_opcoes_indicador(md_opcoes())
  expect_identical(names(op[["Demais indicadores"]]),
    c("v0 (comp_objetivo2_v0)", "ideb_basico (ideb_basico)"))
})

test_that("grupo com um indicador so continua como optgroup", {
  md <- data.frame(mdata_id = 99L, orig_name = "educ1", data_name = "Único")
  op <- AEDi:::painel_opcoes_indicador(md)
  expect_identical(names(op), "Eixo 1")
  expect_type(op[[1]], "list")
  expect_identical(names(op[[1]]), "Indicador 1 - Único (educ1)")
  expect_identical(op[[1]][[1]], "99")
})

test_that("catalogo vazio devolve lista vazia e data_name ausente nao quebra", {
  expect_identical(AEDi:::painel_opcoes_indicador(NULL), list())
  expect_identical(AEDi:::painel_opcoes_indicador(data.frame()), list())
  md <- data.frame(mdata_id = 1L, orig_name = "educ1")
  op <- AEDi:::painel_opcoes_indicador(md)
  expect_identical(names(op[["Eixo 1"]]), "Indicador 1 - educ1 (educ1)")
})

test_that("valores sao ids como caracter e unicos, rotulos unicos", {
  op <- AEDi:::painel_opcoes_indicador(md_opcoes())
  valores <- unlist(op, use.names = FALSE)
  expect_identical(anyDuplicated(valores), 0L)
  rotulos <- unlist(lapply(op, names))
  expect_identical(anyDuplicated(rotulos), 0L)
  expect_true(all(grepl("\\([a-z0-9_]+\\)$", rotulos)))
})
