# Esqueleto do painel: sincronia dos templates e deploy_panel() ----------

compartilhados <- c(
  "R/branding.R"          = "painel_branding.R",
  "R/painel_basemap.R"    = "painel_basemap.R",
  "R/painel_ui.R"         = "painel_ui.R",
  "R/painel_cache.R"      = "painel_cache.R",
  "R/painel_dw.R"         = "painel_dw.R",
  "R/mod_panel_globe.R"   = "mod_panel_globe.R",
  "R/mod_panel_map.R"     = "mod_panel_map.R",
  "R/mod_panel_regiao.R"  = "mod_panel_regiao.R",
  "R/mod_panel_sobre.R"   = "mod_panel_sobre.R")

test_that("templates do esqueleto sao copias exatas dos fontes do pacote", {
  skip_if_not(dir.exists(test_path("../../R")), "sem arvore-fonte (check)")
  tpl <- system.file("painel_esqueleto", package = "AEDi")
  skip_if_not(nzchar(tpl), "templates ausentes")
  for (tpl_rel in names(compartilhados)) {
    esperado <- readLines(test_path("../../R", compartilhados[[tpl_rel]]),
                          encoding = "UTF-8", warn = FALSE)
    obtido <- readLines(file.path(tpl, tpl_rel), encoding = "UTF-8",
                        warn = FALSE)
    expect_identical(obtido, esperado,
      info = paste0(tpl_rel, " divergiu de R/", compartilhados[[tpl_rel]],
                    " — ressincronize com file.copy()"))
  }
})

test_that("deploy_panel(esqueleto=TRUE) materializa copia autonoma", {
  d <- file.path(tempdir(), "painel_esq")
  unlink(d, recursive = TRUE)
  p <- deploy_panel(d, titulo = "Painel Teste", paleta = "pb")
  expect_true(file.exists(file.path(p, "app.R")))
  expect_true(all(file.exists(file.path(p, names(compartilhados)))))
  expect_true(all(file.exists(file.path(p, c(
    "R/app_ui.R", "R/app_server.R", "www/painel.css", "www/painel.js",
    "www/painel-map.js", "www/aedi-Wide.png", "README.md",
    "esqueleto_manifest.json")))))
  # substituicoes: sem placeholders remanescentes, com titulo e paleta
  app_ui <- readLines(file.path(p, "R/app_ui.R"), warn = FALSE)
  expect_false(any(grepl("%%", app_ui)))
  expect_true(any(grepl('"Painel Teste"', app_ui, fixed = TRUE)))
  expect_true(any(grepl('"pb"', app_ui, fixed = TRUE)))
  for (f in c("app.R", "README.md"))
    expect_false(any(grepl("%%", readLines(file.path(p, f), warn = FALSE))))
  # todo .R gerado parseia
  r_files <- c("app.R", file.path("R", list.files(file.path(p, "R"),
                                                  pattern = "[.]R$")))
  for (f in r_files) expect_type(parse(file.path(p, f)), "expression")
  # manifest valido com hash de cada arquivo gerado (lista nomeada)
  m <- jsonlite::fromJSON(file.path(p, "esqueleto_manifest.json"))
  expect_match(m$gerador, "deploy_panel")
  expect_gt(length(m$arquivos), 10)
  hashes <- vapply(m$arquivos, \(x) x$sha256, character(1))
  expect_true(all(nzchar(hashes)))
  expect_identical(names(m$arquivos)[[1]], "app.R")
  # a app gerada constroi de fato: source(app.R) devolve shiny.appobj
  # (paths relativos "R/..." e "www/..." exigem cwd = diretorio da app)
  op <- setwd(p); on.exit(setwd(op), add = TRUE)
  app <- source("app.R", local = TRUE)$value
  expect_s3_class(app, "shiny.appobj")
})

test_that("deploy_panel(esqueleto=TRUE) respeita arquivos existentes", {
  d <- file.path(tempdir(), "painel_esq_guard")
  unlink(d, recursive = TRUE)
  deploy_panel(d)
  expect_error(deploy_panel(d),
               regexp = "sobrescrever")
  expect_error(deploy_panel(d, sobrescrever = TRUE), NA)
})

test_that("deploy_panel(esqueleto=FALSE) gera launcher fina", {
  d <- file.path(tempdir(), "painel_lau")
  unlink(d, recursive = TRUE)
  deploy_panel(d, titulo = "Painel X", esqueleto = FALSE)
  app_r <- readLines(file.path(d, "app.R"), warn = FALSE)
  expect_true(any(grepl("AEDi::panel_app", app_r, fixed = TRUE)))
  expect_false(dir.exists(file.path(d, "R")))
})

test_that("atualizar_painel preserva edicoes locais e regenera manifest", {
  d <- file.path(tempdir(), "painel_upd")
  unlink(d, recursive = TRUE)
  deploy_panel(d, titulo = "T Atualiza", paleta = "pb")
  app_ui_p <- file.path(d, "R/app_ui.R")
  writeLines(c("# edicao local do projeto",
               readLines(app_ui_p, warn = FALSE)), app_ui_p)

  # sem manifest (dir launcher) a atualizacao nega servico
  lau <- file.path(tempdir(), "painel_lau2")
  unlink(lau, recursive = TRUE)
  deploy_panel(lau, esqueleto = FALSE)
  expect_error(atualizar_painel(lau), "manifest")

  # nada a fazer quando upstream nao mudou (mesma versao e conteudo)
  expect_output(atualizar_painel(d), "mais recente")

  # simula versao nova do AEDi: versao do manifest envelhece
  m <- jsonlite::fromJSON(file.path(d, "esqueleto_manifest.json"))
  m$versao_aedi <- "0.0.0.9000"
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE),
             file.path(d, "esqueleto_manifest.json"))

  atualizar_painel(d)
  # edicao local preservada; demais arquivos atualizados
  expect_true(any(grepl("# edicao local do projeto",
                        readLines(app_ui_p, warn = FALSE), fixed = TRUE)))
  m2 <- jsonlite::fromJSON(file.path(d, "esqueleto_manifest.json"))
  hashes <- vapply(m2$arquivos, \(x) x$sha256, character(1))
  # hash do arquivo preservado segue sendo o upstream esperado, nao o local
  d_ref <- file.path(tempdir(), "painel_upd_ref")
  unlink(d_ref, recursive = TRUE)
  deploy_panel(d_ref, titulo = "T Atualiza", paleta = "pb")
  hash_upstream <- digest::digest(file.path(d_ref, "R/app_ui.R"),
                                  file = TRUE, algo = "sha256")
  expect_identical(unname(hashes[["R/app_ui.R"]]), hash_upstream)
  # app segue construindo apos a atualizacao
  op <- setwd(d); on.exit(setwd(op), add = TRUE)
  expect_s3_class(source("app.R", local = TRUE)$value, "shiny.appobj")

  # forcar = TRUE reponta o arquivo modificado pelo upstream
  setwd(op)
  atualizar_painel(d, forcar = TRUE)
  expect_false(any(grepl("# edicao local do projeto",
                         readLines(app_ui_p, warn = FALSE), fixed = TRUE)))
})
