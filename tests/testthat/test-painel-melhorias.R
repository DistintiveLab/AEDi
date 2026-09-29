# Melhorias do painel: rotulos de tempo, visibilidade, ano do resumo,
# tipos de grafico (roadmap_painel_melhorias.md, P1-P4) ---------------

test_that("painel_rotulo_tempo mapeia periodicidades e cai no Ano", {
  r <- AEDi:::painel_rotulo_tempo
  expect_identical(r("anual"), "Ano")
  expect_identical(r("bienal"), "Ano")
  expect_identical(r("mensal"), "Mês")
  expect_identical(r("trimestral"), "Trimestre")
  expect_identical(r("semestral"), "Semestre")
  expect_identical(r("diària"), "Data")
  expect_identical(r(NA_character_), "Ano")
  expect_identical(r(NULL), "Ano")
  expect_identical(r("quinzenal"), "Quinzena")
})

test_that("painel_opcoes_indicador esconde indicadores invisiveis", {
  md <- data.frame(
    mdata_id = c(1L, 2L),
    orig_name = c("educ1", "educ2"),
    data_name = c("Cadastro Único", "Distorção"),
    visivel = c(NA, FALSE))
  op <- AEDi:::painel_opcoes_indicador(md)
  expect_identical(unlist(op, use.names = FALSE), "1")
  # catalogo sem a coluna visivel segue mostrando tudo
  md2 <- md; md2$visivel <- NULL
  expect_length(unlist(AEDi:::painel_opcoes_indicador(md2),
                       use.names = FALSE), 2L)
})

test_that("painel_indicador_default pula indicador invisivel", {
  md <- data.frame(mdata_id = c(1L, 2L), orig_name = c("educ1", "educ2"),
                   visivel = c(FALSE, TRUE))
  expect_identical(AEDi:::painel_indicador_default(md), 2L)
  # env pinando um invisivel tambem cai no primeiro visivel
  withr::with_envvar(list(aedi_indicador = "educ1"),
    expect_identical(AEDi:::painel_indicador_default(md), 2L))
})

hierarquia_resumo <- function() {
  data.frame(
    raiz_id = c(13L, 13L), raiz_nome = c("Eixos", "Eixos"),
    datagroup_id = c(1L, 1L), datagroup_name = c("Eixo 1", "Eixo 1"),
    mdata_id = c(10L, 11L), data_class_id = c(2L, 4L),
    orig_name = c("educ1", "comp_educ"),
    data_name = c("Indicador bruto", "Composto do Eixo 1"))
}

test_that("painel_resumo_grupos respeita o ano e traz o par anterior", {
  compostos <- data.frame(mdata_id = 11L, orig_name = "comp_educ",
                          data_name = "Composto do Eixo 1")
  valores <- data.frame(
    mdata_id = c(11L, 11L, 11L, 11L),
    refdate = as.Date(c("2019-01-01", "2020-01-01",
                        "2021-01-01", "2022-01-01")),
    value = c(2, 3, 5, 7))
  # sem ano: ultima observacao + anterior imediata
  r <- AEDi:::painel_resumo_grupos(hierarquia_resumo(), compostos, valores)
  expect_identical(r$refdate, as.Date("2022-01-01"))
  expect_equal(r$valor, 7)
  expect_equal(r$valor_ant, 5)
  expect_identical(r$refdate_ant, as.Date("2021-01-01"))
  # ano de referencia no meio da serie
  r20 <- AEDi:::painel_resumo_grupos(hierarquia_resumo(), compostos,
                                     valores, ano = 2020)
  expect_identical(r20$refdate, as.Date("2020-01-01"))
  expect_equal(r20$valor, 3)
  expect_equal(r20$valor_ant, 2)
  # ano atras da primeira observacao: composto sai do resumo
  r10 <- AEDi:::painel_resumo_grupos(hierarquia_resumo(), compostos,
                                     valores, ano = 2010)
  expect_identical(nrow(r10), 0L)
  # observacao unica: sem anterior
  unico <- valores[valores$refdate <= as.Date("2019-01-01"), ]
  r1 <- AEDi:::painel_resumo_grupos(hierarquia_resumo(), compostos, unico)
  expect_true(is.na(r1$valor_ant) && is.na(r1$refdate_ant))
})

test_that("painel_plot_indicador renomeia eixos e despacha o tipo", {
  v <- data.frame(refdate = as.Date(c("2013-01-01", "2014-01-01")),
                  value = c(0.2, 0.4))
  p <- AEDi:::painel_plot_indicador(v, "Título", cor = "#1351B4",
                                    tipo = "linha", rotulo_x = "Ano")
  expect_s3_class(p, "ggplot")
  expect_identical(names(p$data), c("Ano", "valor"))
  expect_type(p$data$Ano, "integer")
  expect_s3_class(p$layers[[1]]$geom, "GeomLine")
  pb <- AEDi:::painel_plot_indicador(v, tipo = "barras")
  expect_s3_class(pb$layers[[1]]$geom, "GeomCol")
  pl <- AEDi:::painel_plot_indicador(v, tipo = "lollipop")
  expect_s3_class(pl$layers[[1]]$geom, "GeomSegment")
  expect_s3_class(pl$layers[[2]]$geom, "GeomPoint")
  # tipo desconhecido/ausente cai em linha; rotulo mensal mantem a data
  px <- AEDi:::painel_plot_indicador(v, tipo = NA_character_,
                                     rotulo_x = "Mês")
  expect_s3_class(px$layers[[1]]$geom, "GeomLine")
  expect_s3_class(px$data$Mês, "Date")
})
