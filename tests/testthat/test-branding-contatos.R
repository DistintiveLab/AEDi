# Contatos do header (aedi_contatos) com logo por box ---------

test_that("contatos_header default: Distintive com logo padrao, autor sem", {
  Sys.unsetenv("aedi_contatos")
  Sys.unsetenv("aedi_contato_logo")
  itens <- AEDi:::contatos_header()
  expect_length(itens, 2)
  html1 <- as.character(itens[[1]])
  html2 <- as.character(itens[[2]])
  expect_identical(grepl("<img", html1, fixed = TRUE), FALSE)
  expect_match(html2, "www/aedi-innovations-Square.png", fixed = TRUE)
  expect_match(html2, "apps@distintive.com.br", fixed = TRUE)
  expect_match(html1, "rodrigo@borges.net.br", fixed = TRUE)
})

test_that("5o campo define o logo do box e none desliga", {
  tf <- tempfile(fileext = ".png")
  file.create(tf)
  Sys.setenv(aedi_contatos = paste(
    paste0("Ana Silva|Pesquisadora|61-0000-0000|ana@exemplo.br|", tf),
    "Org X|Consultoria|61-1111-1111|x@exemplo.br|none",
    sep = ";"))
  on.exit(Sys.unsetenv("aedi_contatos"), add = TRUE)
  itens <- AEDi:::contatos_header()
  expect_length(itens, 2)
  html1 <- as.character(itens[[1]])
  html2 <- as.character(itens[[2]])
  expect_match(html1, "aedi_marca/", fixed = TRUE)
  expect_match(html1, basename(tf), fixed = TRUE)
  expect_identical(grepl("<img", html2, fixed = TRUE), FALSE)
})

test_that("aedi_contato_logo vale para entradas sem 5o campo", {
  Sys.setenv(aedi_contatos = "Org Y|Laboratorio|61-2222-2222|y@exemplo.br")
  Sys.setenv(aedi_contato_logo = "www/meu-logo.png")
  on.exit({
    Sys.unsetenv("aedi_contatos")
    Sys.unsetenv("aedi_contato_logo")
  }, add = TRUE)
  html <- as.character(AEDi:::contatos_header()[[1]])
  expect_match(html, "www/meu-logo.png", fixed = TRUE)
  expect_match(html, "<img", fixed = TRUE)
})

test_that("contact_item renderiza avatar redondo so quando ha logo", {
  html <- as.character(AEDi:::contact_item(
    name = "Ana", role = "Pesquisadora",
    phone = "61", email = "a@b.br", logo = "www/x.png"))
  expect_match(html, "<img", fixed = TRUE)
  expect_match(html, "www/x.png", fixed = TRUE)
  expect_match(html, "border-radius:50%", fixed = TRUE)
  expect_match(html, "mailto:a@b.br", fixed = TRUE)
  sem <- as.character(AEDi:::contact_item(
    name = "Ana", role = "Pesquisadora", phone = "61", email = "a@b.br"))
  expect_identical(grepl("<img", sem, fixed = TRUE), FALSE)
})
