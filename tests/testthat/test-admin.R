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
  expect_true(any(grepl('AEDi::admin_app\\(raiz = "\\.\\."\\)', linhas)))
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
