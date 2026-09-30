test_that("admin_app constroi a app sem tocar o banco (server e lazy)", {
  app <- AEDi::admin_app(raiz = tempdir(), titulo = "Admin teste")
  expect_s3_class(app, "shiny.appobj")
})

test_that("deploy_admin grava launcher fina e idempotente com sobrescrever", {
  tmp <- tempfile()
  dir.create(file.path(tmp, "coleta"), recursive = TRUE)
  writeLines("script,dep_orig_name,regua,acao,serie_propria\nfake,citec4,proprio,pular,",
             file.path(tmp, "coleta", "dependencias.csv"))

  dir_app <- file.path(tmp, "admin")
  expect_error(AEDi::deploy_admin(diretorio = dir_app), NA)
  expect_true(file.exists(file.path(dir_app, "app.R")))
  expect_true(file.exists(file.path(dir_app, "README.md")))
  linhas <- readLines(file.path(dir_app, "app.R"))
  expect_true(any(grepl('AEDi::admin_app\\(raiz = "\\.\\.", tema = "govbr"\\)',
                        linhas)))
  expect_error(parse(file.path(dir_app, "app.R")), NA)

  # sem sobrescrever, recusa; com sobrescrever, regenera
  expect_error(AEDi::deploy_admin(diretorio = dir_app), "app.R ja existe")
  expect_error(AEDi::deploy_admin(diretorio = dir_app, sobrescrever = TRUE),
               NA)
})

test_that(".tabela_status_lote devolve colunas em portugues mesmo sem lote", {
  tmp <- tempfile()
  dir.create(file.path(tmp, "coleta"), recursive = TRUE)
  d <- AEDi:::.tabela_status_lote(tmp, projeto = "projeto_inexistente")
  expect_s3_class(d, "data.frame")
  expect_identical(colnames(d), c("Script", "Etapa", "Última execução",
                                  "Metadados (BD)", "Máx. refdate",
                                  "Situação", "Detalhe"))
})

test_that("listar_scripts_coleta sem coleta/ devolve character(0), nunca NA", {
  tmp <- tempfile()  # sem coleta/ algum
  expect_identical(AEDi:::listar_scripts_coleta(tmp), character(0))
  # regressao 0.8.1: paste0(character(0), ".ignore") devolve
  # ".ignore" e arqs[TRUE] no vetor vazio virava NA — linha em branco com
  # botao Atualizar na aba Atualizacao (script 'NA' nao encontrado)
  dir.create(file.path(tmp, "coleta"), recursive = TRUE)
  expect_identical(AEDi:::listar_scripts_coleta(tmp), character(0))
  writeLines("", file.path(tmp, "coleta", "um.R"))
  writeLines("", file.path(tmp, "coleta", "dois.R.ignore"))
  writeLines("", file.path(tmp, "coleta", "dois.R"))
  expect_identical(AEDi:::listar_scripts_coleta(tmp), "um.R")
})

test_that(".tabela_status_lote sem lote devolve 0 linhas e sem NA", {
  sem_coleta <- tempfile()
  com_coleta_vazio <- tempfile()
  dir.create(file.path(com_coleta_vazio, "coleta"), recursive = TRUE)
  for (raiz in c(sem_coleta, com_coleta_vazio)) {
    d <- AEDi:::.tabela_status_lote(raiz, projeto = "projeto_inexistente",
                                    com_acao = TRUE, id_acao = "x")
    expect_identical(nrow(d), 0L)
    expect_false(any(is.na(d$Script)))
    expect_identical(colnames(d), c("Script", "Etapa", "Última execução",
                                    "Metadados (BD)", "Máx. refdate",
                                    "Situação", "Detalhe", "Ação"))
  }
})

test_that(".dados_dependencias_lote usa o CSV quando o DW nao tem grafo", {
  tmp <- tempfile()
  dir.create(file.path(tmp, "coleta"), recursive = TRUE)
  writeLines(c("script,dep_orig_name,regua,acao,serie_propria",
               "fake,citec4,proprio,pular,comp_educ;comp_citec"),
             file.path(tmp, "coleta", "dependencias.csv"))
  d <- AEDi:::.dados_dependencias_lote("projeto_inexistente", tmp)
  expect_gte(nrow(d), 1)
  expect_true(all(c("script", "serie", "regua", "acao",
                    "series_proprias") %in% colnames(d)))
  linha <- d[d$script == "fake" & d$serie == "citec4", ]
  expect_identical(linha$acao, "pular")
  expect_identical(linha$series_proprias, "comp_educ; comp_citec")
})
