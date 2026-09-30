# Tema no painel admin (T2): admin_app + deploy_admin + chrome ------------

test_that("css do admin existe e consome as variáveis do núcleo", {
  css_path <- system.file("tema", "aedi-admin.css", package = "AEDi")
  skip_if_not(nzchar(css_path))
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("var(--p-", css, fixed = TRUE))
  # chrome do admin, não do painel
  expect_false(grepl("painel-", css))
})

test_that("admin_app embute recursos do tema, botão e css do admin", {
  app <- AEDi::admin_app(tema = "pb")
  res <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                              url_scheme = "http"))
  skip_if(is.null(res), "httpHandler não renderizou a UI")
  ui <- if (is.raw(res$content)) rawToChar(res$content) else
    as.character(res$content)
  expect_true(grepl("aedi_tema_raiz", ui, fixed = TRUE))
  expect_true(grepl('data-paleta="pb"', ui, fixed = TRUE))
  expect_true(grepl("admin_tema_btn", ui, fixed = TRUE))
  expect_true(grepl("aedi-tema-btn", ui, fixed = TRUE))
  expect_true(grepl("body.aedi-pb", ui, fixed = TRUE))
  expect_true(grepl("aedi:paleta", ui, fixed = TRUE))
  # css do admin é inlineado pelo includeCSS
  expect_true(grepl("navbar-default", ui, fixed = TRUE))
  expect_error(AEDi::admin_app(tema = "outra"), "govbr|pb")
})

test_that("deploy_admin materializa o tema resolvido na launcher", {
  dir <- withr::local_tempfile(pattern = "admin")
  dir.create(dir, recursive = TRUE)
  withr::local_dir(dir)
  AEDi::deploy_admin("admin", tema = "pb", sobrescrever = TRUE)
  app_r <- paste(readLines(file.path("admin", "app.R"), warn = FALSE),
                 collapse = "\n")
  expect_true(grepl('AEDi::admin_app(raiz = "..", tema = "pb")',
                    app_r, fixed = TRUE))
  readme <- paste(readLines(file.path("admin", "README.md"), warn = FALSE),
                  collapse = "\n")
  expect_true(grepl("`pb`", readme, fixed = TRUE))
})

test_that("deploy_admin sem tema segue o default do núcleo", {
  dir <- withr::local_tempfile(pattern = "admin")
  dir.create(dir, recursive = TRUE)
  withr::local_dir(dir)
  AEDi::deploy_admin("admin", sobrescrever = TRUE)
  app_r <- paste(readLines(file.path("admin", "app.R"), warn = FALSE),
                 collapse = "\n")
  expect_true(grepl('tema = "govbr"', app_r, fixed = TRUE))
})
