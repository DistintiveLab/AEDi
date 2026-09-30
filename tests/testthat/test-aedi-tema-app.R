# Tema no app AEDi (T3): app_ui + rightbar + chrome AdminLTE ---------------

test_that("css do app existe e sobrepõe o chrome AdminLTE com --p-*", {
  css_path <- system.file("tema", "aedi-app.css", package = "AEDi")
  skip_if_not(nzchar(css_path))
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("var(--p-", css, fixed = TRUE))
  expect_true(grepl(".skin-black .main-header", css, fixed = TRUE))
  expect_true(grepl(".skin-black .main-sidebar", css, fixed = TRUE))
  expect_true(grepl(".box-header", css, fixed = TRUE))
  expect_false(grepl("painel-", css))
})

test_that("app_ui embute o núcleo do tema, o css do app e os recursos www", {
  ui <- app_ui(tema = "pb")
  h <- paste(as.character(ui), collapse = "")
  expect_true(grepl("aedi_tema_raiz", h, fixed = TRUE))
  expect_true(grepl('data-paleta="pb"', h, fixed = TRUE))
  expect_true(grepl("aedi-tema-btn", h, fixed = TRUE))
  expect_true(grepl("body.aedi-pb", h, fixed = TRUE))
  expect_true(grepl("aedi:paleta", h, fixed = TRUE))
  # css do app inlineado (seletor AdminLTE) e recursos www preservados
  expect_true(grepl("main-sidebar", h, fixed = TRUE))
  expect_true(grepl("www/styles.css", h, fixed = TRUE))
  # dashboardPage skin-black continua presente
  expect_true(grepl("skin-black", h, fixed = TRUE))
})

test_that("app_ui sem tema segue o default do núcleo", {
  h <- paste(as.character(app_ui()), collapse = "")
  expect_true(grepl('data-paleta="govbr"', h, fixed = TRUE))
  expect_error(app_ui(tema = "outra"), "govbr|pb")
})

test_that("rightbar ganha aba Tema com o botão de alternância", {
  rb <- right_sidebar_ui()
  h <- paste(as.character(rb), collapse = "")
  expect_true(grepl("app_tema_btn", h, fixed = TRUE))
  expect_true(grepl("aedi-tema-btn", h, fixed = TRUE))
  expect_true(grepl("Tema", h, fixed = TRUE))
  # aba Ajuda preservada
  expect_true(grepl("Ajuda", h, fixed = TRUE))
})
