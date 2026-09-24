# Equipe (painel_equipe) e apoios (painel_apoios) da aba Sobre ---------

test_that("painel_brand_equipe default mantem o cartao unico do autor", {
  Sys.unsetenv("painel_equipe")
  cards <- AEDi:::painel_brand_equipe()
  expect_length(cards, 1)
  html <- as.character(cards[[1]])
  expect_match(html, "painel-pessoa-card", fixed = TRUE)
  expect_match(html, "Rodrigo Emmanuel Santana Borges", fixed = TRUE)
  expect_match(html, "Desenvolvedor e cientista de dados", fixed = TRUE)
  expect_match(html, "mailto:rodrigo@borges.net.br", fixed = TRUE)
})

test_that("painel_brand_equipe aceita varias entradas separadas por ;", {
  Sys.setenv(painel_equipe = paste(
    "Ana Silva|Pesquisadora|ana@exemplo.br",
    "Bruno Souza|Cientista de dados",
    "Carla Dias|Designer|carla@exemplo.br", sep = ";"))
  on.exit(Sys.unsetenv("painel_equipe"), add = TRUE)
  cards <- AEDi:::painel_brand_equipe()
  expect_length(cards, 3)
  expect_match(as.character(cards[[1]]), "Ana Silva", fixed = TRUE)
  expect_match(as.character(cards[[1]]), "mailto:ana@exemplo.br", fixed = TRUE)
  html2 <- as.character(cards[[2]])
  expect_match(html2, "Bruno Souza", fixed = TRUE)
  expect_match(html2, "painel-pessoa-papel", fixed = TRUE)
  expect_identical(grepl("mailto", html2, fixed = TRUE), FALSE)
  expect_match(as.character(cards[[3]]), "Carla Dias", fixed = TRUE)
})

test_that("painel_brand_equipe aceita foto (4o campo) como avatar do cartao", {
  Sys.setenv(painel_equipe = paste(
    "Ana Silva|Pesquisadora|ana@exemplo.br|foto-ana.png",
    "Bruno Souza|Cientista de dados|", sep = ";"))
  on.exit(Sys.unsetenv("painel_equipe"), add = TRUE)
  cards <- AEDi:::painel_brand_equipe()
  expect_length(cards, 2)
  html1 <- as.character(cards[[1]])
  expect_match(html1, "painel-pessoa-foto", fixed = TRUE)
  expect_match(html1, "aedi_marca/foto-ana.png", fixed = TRUE)
  expect_match(html1, "Foto de Ana Silva", fixed = TRUE)
  expect_identical(grepl("painel-pessoa-foto", as.character(cards[[2]]),
                         fixed = TRUE), FALSE)
})

test_that("a foto da equipe tambem aceita URL http(s) direta", {
  Sys.setenv(painel_equipe =
               "Ana Silva|Pesquisadora|ana@exemplo.br|https://cdn.exemplo.br/ana.jpg")
  on.exit(Sys.unsetenv("painel_equipe"), add = TRUE)
  html <- as.character(AEDi:::painel_brand_equipe()[[1]])
  expect_match(html, "src=\"https://cdn.exemplo.br/ana.jpg\"", fixed = TRUE)
  expect_match(html, "painel-pessoa-foto", fixed = TRUE)
})

test_that("painel_brand_apoios default mantem o box Distintive", {
  Sys.unsetenv("painel_apoios")
  Sys.unsetenv("painel_apoio")
  boxes <- AEDi:::painel_brand_apoios()
  expect_length(boxes, 1)
  html <- as.character(boxes[[1]])
  expect_match(html, "painel-apoio-logo", fixed = TRUE)
  expect_match(html, "https://www.distintive.com.br", fixed = TRUE)
  expect_match(html, "aria-label=\"Distintive (distintive.com.br)\"", fixed = TRUE)
  expect_match(html, "Logotipo da Distintive", fixed = TRUE)
  expect_match(html, "Este painel contou com apoio material e financeiro de ",
               fixed = TRUE)
})

test_that("box default continua respeitando painel_apoio para a frase", {
  Sys.unsetenv("painel_apoios")
  Sys.setenv(painel_apoio = "Financiado por |IPEA|https://www.ipea.gov.br|.")
  on.exit(Sys.unsetenv("painel_apoio"), add = TRUE)
  html <- as.character(AEDi:::painel_brand_apoios()[[1]])
  expect_match(html, ">IPEA</a>", fixed = TRUE)
  expect_match(html, "distintive.com.br", fixed = TRUE)
})

test_that("painel_apoios com duas entradas gera dois boxes", {
  Sys.setenv(painel_apoios = paste(
    "logo-a.png|https://a.example.org|Apoio financeiro da instituição A|Instituição A",
    "https://cdn.exemplo.br/b.png|https://b.example.org|Apoio material da instituição B|",
    sep = ";"))
  on.exit(Sys.unsetenv("painel_apoios"), add = TRUE)
  boxes <- AEDi:::painel_brand_apoios()
  expect_length(boxes, 2)
  html1 <- as.character(boxes[[1]])
  expect_match(html1, "href=\"https://a.example.org\"", fixed = TRUE)
  expect_match(html1, "aria-label=\"Instituição A (a.example.org)\"", fixed = TRUE)
  expect_match(html1, "Apoio financeiro da instituição A", fixed = TRUE)
  expect_match(html1, "aedi_marca/logo-a.png", fixed = TRUE)
  html2 <- as.character(boxes[[2]])
  expect_match(html2, "src=\"https://cdn.exemplo.br/b.png\"", fixed = TRUE)
  expect_match(html2, "aria-label=\"b.example.org\"", fixed = TRUE)
})

test_that("painel_apoios vazio volta ao box Distintive", {
  Sys.setenv(painel_apoios = "")
  on.exit(Sys.unsetenv("painel_apoios"), add = TRUE)
  boxes <- AEDi:::painel_brand_apoios()
  expect_length(boxes, 1)
  expect_match(as.character(boxes[[1]]), "distintive.com.br", fixed = TRUE)
})
