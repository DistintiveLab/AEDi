# Aba "Baixar": construtores puros de painel_xlsx.R + UI do modulo ----------

# dados sinteticos (sem banco): dois indicadores, uma localidade, anos 2022-2023
valores_regiao <- data.frame(
  mdata_id = c(1L, 1L, 1L, 2L),
  refdate = as.Date(c("2022-03-15", "2022-11-20", "2023-06-01", "2022-08-01")),
  value = c(10, 11, 21, 5))

mdata_regiao <- data.frame(
  mdata_id = c(1L, 2L, 3L),
  orig_name = c("ind_b", "ind_a", "ind_fora"),
  data_name = c("Indicador B", "", "Nunca entra"),
  data_desc = c("desc B", "desc A", "desc fora"),
  stringsAsFactors = FALSE)

test_that("painel_slug gera ascii com hifens", {
  expect_identical(AEDi:::painel_slug(" A B "), "a-b")
  expect_identical(AEDi:::painel_slug("x/y_9"), "x-y-9")
  expect_identical(AEDi:::painel_slug(""), "dados")
  expect_identical(AEDi:::painel_slug(NA_character_), "dados")
  aceito <- AEDi:::painel_slug("São Paulo")
  expect_true(grepl("^[a-z0-9-]+$", aceito),
              info = paste("slug fora do padrao:", aceito))
})

test_that("painel_ultimo_por_ano guarda a ultima observacao finita do ano", {
  ultimo <- AEDi:::painel_ultimo_por_ano(valores_regiao, "mdata_id")
  expect_setequal(names(ultimo), c("mdata_id", "ano", "valor"))
  # ind 1 em 2022: refdate 2022-11-20 ganha de 2022-03-15
  esperado <- data.frame(mdata_id = c(1L, 1L, 2L),
                         ano = c(2022L, 2023L, 2022L),
                         valor = c(11, 21, 5))
  esperado <- esperado[order(esperado$mdata_id, esperado$ano), ]
  obtido <- ultimo[order(ultimo$mdata_id, ultimo$ano),
                   match(c("mdata_id", "ano", "valor"), names(ultimo))]
  rownames(obtido) <- NULL
  expect_equal(obtido, esperado)
})

test_that("painel_ultimo_por_ano descarta NaN/NA/Inf e aceita entrada vazia", {
  com_nan <- data.frame(
    local_id = c(1L, 1L, 1L, 1L),
    refdate = as.Date(c("2020-01-01", "2020-06-01", "2020-09-01",
                        "2021-01-01")),
    value = c(NaN, Inf, NA, 7))
  ultimo <- AEDi:::painel_ultimo_por_ano(com_nan, "local_id")
  # 2020 so tem nao-finitos: o ano inteiro fica de fora; sobra 2021
  expect_equal(nrow(ultimo), 1L)
  expect_equal(ultimo$valor, 7)
  vazio <- AEDi:::painel_ultimo_por_ano(
    data.frame(local_id = integer(0), refdate = as.Date(character(0)),
               value = numeric(0)), "local_id")
  expect_equal(nrow(vazio), 0L)
  expect_setequal(names(vazio), c("local_id", "ano", "valor"))
  expect_equal(nrow(AEDi:::painel_ultimo_por_ano(data.frame(), "x")), 0L)
})

test_that("painel_xlsx_regiao gera abas dados + metadados", {
  arq <- tempfile(fileext = ".xlsx")
  on.exit(unlink(arq), add = TRUE)
  AEDi:::painel_xlsx_regiao(arq, valores_regiao, mdata_regiao,
                            "Espírito Santo", "UF", "Painel Teste")
  expect_identical(openxlsx::getSheetNames(arq), c("dados", "metadados"))
  # cabecalho de 4 pares: tabela com header na linha 6
  bloco <- openxlsx::read.xlsx(arq, sheet = "dados", colNames = FALSE,
                               rows = 1:5)
  expect_equal(bloco[[1]][1:4],
               c("Painel", "Localidade", "Nível territorial", "Gerado em"))
  expect_equal(bloco[[2]][1], "Painel Teste")
  expect_equal(bloco[[2]][2], "Espírito Santo")
  tab <- openxlsx::read.xlsx(arq, sheet = "dados", startRow = 6)
  expect_identical(names(tab), c("Código", "Indicador", "2022", "2023"))
  # ordenacao por orig_name: ind_a (mdata 2, nome fallback p/ orig_name)
  # vem antes de ind_b (mdata 1)
  expect_equal(tab$Código, c("ind_a", "ind_b"))
  expect_equal(tab$Indicador, c("ind_a", "Indicador B"))
  expect_equal(tab[["2022"]], c(5, 11))
  expect_equal(tab[["2023"]], c(NA_real_, 21))
  # metadados: id fora do recorte nao entra
  md <- openxlsx::read.xlsx(arq, sheet = "metadados")
  expect_identical(names(md), c("id", "Código", "Nome", "Descrição"))
  expect_equal(md$id, c(2, 1))
})

test_that("painel_xlsx_regiao sem dados gera aba de aviso", {
  arq <- tempfile(fileext = ".xlsx")
  on.exit(unlink(arq), add = TRUE)
  AEDi:::painel_xlsx_regiao(
    arq, data.frame(mdata_id = integer(0), refdate = as.Date(character(0)),
                    value = numeric(0)),
    mdata_regiao, "X", "UF")
  expect_identical(openxlsx::getSheetNames(arq), "dados")
  aviso <- openxlsx::read.xlsx(arq, sheet = "dados", colNames = FALSE)
  expect_true(grepl("Sem observacoes", aviso[[1]][1]))
})

test_that("painel_xlsx_indicador gera uma aba por ano ordenada", {
  por_ano <- list(
    "2022" = data.frame(
      local_id = c(12L, 11L, 99L),
      refdate = as.Date(rep("2022-12-31", 3)),
      value = c(2.5, 1.5, 9)),
    "2021" = data.frame(
      local_id = 11L, refdate = as.Date("2021-12-31"), value = 1),
    "2020" = data.frame(local_id = integer(0),
                        refdate = as.Date(character(0)), value = numeric(0)))
  locais <- c("Alfa" = 11, "Beta" = 12)
  arq <- tempfile(fileext = ".xlsx")
  on.exit(unlink(arq), add = TRUE)
  AEDi:::painel_xlsx_indicador(arq, por_ano, locais, "Indicador X", "Município",
                               "Painel Teste")
  # 2020 sem dados fica fora; abas em ordem numerica (2021 < 2022)
  expect_identical(openxlsx::getSheetNames(arq), c("metadados", "2021", "2022"))
  # contexto na aba metadados
  ctx <- openxlsx::read.xlsx(arq, sheet = "metadados", colNames = FALSE,
                             rows = 1:5)
  expect_equal(ctx[[1]][1:5], c("Painel", "Indicador", "Nível territorial",
                                "Anos", "Gerado em"))
  expect_equal(ctx[[2]][2], "Indicador X")
  # aba de 2022: ordenada por local_id, rotulos do vetor nomeado,
  # fallback "local N" para id fora do nivel
  tab22 <- openxlsx::read.xlsx(arq, sheet = "2022")
  expect_identical(names(tab22), c("Código", "Localidade", "Valor"))
  expect_equal(tab22$Código, c(11, 12, 99))
  expect_equal(tab22$Localidade, c("Alfa", "Beta", "local 99"))
  expect_equal(tab22$Valor, c(1.5, 2.5, 9))
})

test_that("painel_xlsx_indicador sem dados gera aba de aviso", {
  vazio <- data.frame(local_id = integer(0),
                      refdate = as.Date(character(0)), value = numeric(0))
  arq <- tempfile(fileext = ".xlsx")
  on.exit(unlink(arq), add = TRUE)
  AEDi:::painel_xlsx_indicador(arq, list("2022" = vazio),
                               c("Alfa" = 11), "Indicador X", "UF")
  expect_identical(openxlsx::getSheetNames(arq), "dados")
  aviso <- openxlsx::read.xlsx(arq, sheet = "dados", colNames = FALSE)
  expect_true(grepl("Sem observacoes", aviso[[1]][1]))
})

test_that("painel_xlsx_indicador traduz o codigo para o IBGE quando mapeado", {
  por_ano <- list("2022" = data.frame(
    local_id = c(11L, 12L, 99L),
    refdate = as.Date(rep("2022-12-31", 3)),
    value = c(1.5, 2.5, 9)))
  codigos <- c("11" = "1100023", "12" = "1100031")
  arq <- tempfile(fileext = ".xlsx")
  on.exit(unlink(arq), add = TRUE)
  AEDi:::painel_xlsx_indicador(arq, por_ano, c("Alfa" = 11, "Beta" = 12),
                               "Indicador X", "Município",
                               codigos = codigos)
  tab <- openxlsx::read.xlsx(arq, sheet = "2022")
  # mapeados viram o codigo IBGE; fora do vetor cai no local_id como texto
  expect_identical(tab$Código, c("1100023", "1100031", "99"))
  expect_equal(tab$Localidade, c("Alfa", "Beta", "local 99"))
  expect_equal(tab$Valor, c(1.5, 2.5, 9))
})

test_that("painel_codigo_mun_cache traz codigo IBGE de 7 digitos por local_id", {
  codigos <- AEDi:::painel_codigo_mun_cache()
  expect_length(codigos, 5570L)
  expect_true(all(grepl("^[0-9]{7}$", codigos)))
  expect_identical(sum(startsWith(codigos, "31")), 853L)  # MG
})

test_that("mod_panel_baixar_ui monta os dois cards de download", {
  ui <- AEDi:::mod_panel_baixar_ui("baixar_teste")
  expect_s3_class(ui, c("shiny.tag.list", "list"))
  html <- paste(as.character(ui), collapse = "")
  for (id in c("baixar_teste-nivel_regiao", "baixar_teste-regiao",
               "baixar_teste-baixar_regiao", "baixar_teste-indicador",
               "baixar_teste-nivel_ano", "baixar_teste-baixar_indicador"))
    expect_true(grepl(id, html, fixed = TRUE), info = id)
})
