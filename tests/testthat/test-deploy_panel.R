test_that("deploy_panel (launcher) cria app executavel no projeto onde e chamada", {
  tmp <- file.path(tempdir(), "proj_painel")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  op <- setwd(tmp); on.exit(setwd(op), add = TRUE)

  out <- deploy_panel(diretorio = "painel", titulo = "Painel Teste",
                      esqueleto = FALSE)
  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "app.R")))
  expect_true(file.exists(file.path(out, "README.md")))

  # app.R devolve um objeto shinyApp pronto para hospedar
  app <- source(file.path(out, "app.R"), local = TRUE)$value
  expect_s3_class(app, "shiny.appobj")

  # segunda chamada sem sobrescrever falha; com sobrescrever, recria
  expect_error(deploy_panel(diretorio = "painel", esqueleto = FALSE),
               "sobrescrever")
  expect_length(deploy_panel(diretorio = "painel", esqueleto = FALSE,
                             sobrescrever = TRUE), 1)
})

test_that("deploy_panel (launcher) registra a paleta escolhida na app gerada", {
  tmp <- file.path(tempdir(), "proj_painel_pb")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  op <- setwd(tmp); on.exit(setwd(op), add = TRUE)

  out <- deploy_panel(diretorio = "painel", paleta = "pb", esqueleto = FALSE)
  linhas <- readLines(file.path(out, "app.R"))
  expect_true(any(grepl("paleta = \"pb\"", linhas, fixed = TRUE)))
  expect_error(deploy_panel(diretorio = "p2", paleta = "x"), "govbr")

  app <- source(file.path(out, "app.R"), local = TRUE)$value
  expect_s3_class(app, "shiny.appobj")
})
