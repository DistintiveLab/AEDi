# Núcleo do tema gov.br/pb (T1): variáveis, toggle e helpers ---------------

test_that("núcleo do tema existe e define as duas paletas", {
  tema_dir <- system.file("tema", package = "AEDi")
  skip_if_not(nzchar(tema_dir))
  css <- readLines(file.path(tema_dir, "aedi-tema.css"), warn = FALSE)
  css <- paste(css, collapse = "\n")
  # paleta gov.br (default) e pb (classe no body), mesmas cores do painel
  expect_true(grepl(":root", css, fixed = TRUE))
  expect_true(grepl("body.aedi-pb", css, fixed = TRUE))
  for (hex in c("#1351B4", "#0C3F91", "#FFCD07", "#1B1B1B", "#78529D",
                "#5E3F82", "#C9B9DE"))
    expect_true(grepl(hex, css, fixed = TRUE), info = paste("sem", hex))
  # sem regras específicas do painel no núcleo
  expect_false(grepl("painel-", css))
  js <- readLines(file.path(tema_dir, "aedi-tema.js"), warn = FALSE)
  js <- paste(js, collapse = "\n")
  expect_true(grepl("aedi-pb", js, fixed = TRUE))
  expect_true(grepl("'aedi_paleta'", js, fixed = TRUE))
  expect_true(grepl("aedi_tema_raiz", js, fixed = TRUE))
  expect_true(grepl("aedi:paleta", js, fixed = TRUE))
})

test_that("aedi_tema_paleta_default lê a env aedi_paleta com fallback", {
  expect_identical(AEDi:::aedi_tema_paleta_default(), "govbr")
  expect_identical(AEDi:::aedi_tema_paleta_default("pb"), "pb")
  expect_identical(
    withr::with_envvar(c(aedi_paleta = "pb"), AEDi:::aedi_tema_paleta_default()),
    "pb")
  expect_identical(
    withr::with_envvar(c(aedi_paleta = "govbr"), AEDi:::aedi_tema_paleta_default("pb")),
    "govbr")
  expect_error(AEDi:::aedi_tema_paleta_default("outra"), "govbr|pb")
  expect_error(
    withr::with_envvar(c(aedi_paleta = "x"), AEDi:::aedi_tema_paleta_default()),
    "govbr|pb")
})

test_that("aedi_tema_recursos devolve css+js+div raiz com data-paleta", {
  for (paleta in c("govbr", "pb")) {
    r <- AEDi:::aedi_tema_recursos(paleta)
    html <- paste(as.character(r), collapse = "")
    # includeCSS/includeScript inline o conteudo (nao linkam por nome)
    expect_true(grepl("body.aedi-pb", html, fixed = TRUE))
    expect_true(grepl("aedi:paleta", html, fixed = TRUE))
    expect_true(grepl("aedi_tema_raiz", html, fixed = TRUE))
    expect_true(grepl(paste0('data-paleta="', paleta, '"'), html,
                      fixed = TRUE))
  }
  expect_error(AEDi:::aedi_tema_recursos("outra"), "govbr|pb")
})

test_that("aedi_tema_botao marca classe e estado inicial acessível", {
  b <- AEDi:::aedi_tema_botao("meu_btn")
  expect_identical(b$name, "button")
  expect_identical(b$attribs$id, "meu_btn")
  expect_identical(b$attribs$class, "aedi-tema-btn")
  expect_identical(b$attribs$`aria-pressed`, "true")
  expect_identical(as.character(b$children[[1]]), "Preto e branco")
})
