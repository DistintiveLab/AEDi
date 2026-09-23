# Linha de apoio institucional da aba Sobre (painel_apoio) -------------

test_that("painel_brand_apoio default mantem o credito a Distintive", {
  Sys.unsetenv("painel_apoio")
  html <- as.character(AEDi:::painel_brand_apoio())
  expect_match(html, "Este painel contou com apoio material e financeiro de ",
               fixed = TRUE)
  expect_match(html, ">Distintive</a>", fixed = TRUE)
  expect_match(html, "https://www.distintive.com.br", fixed = TRUE)
  expect_match(html, "noopener", fixed = TRUE)
})

test_that("painel_brand_apoio aceita texto|nome|url|depois", {
  Sys.setenv(painel_apoio = "Financiado por |IPEA|https://www.ipea.gov.br| em 2026.")
  on.exit(Sys.unsetenv("painel_apoio"), add = TRUE)
  html <- as.character(AEDi:::painel_brand_apoio())
  expect_match(html, "Financiado por ", fixed = TRUE)
  expect_match(html, ">IPEA</a>", fixed = TRUE)
  expect_match(html, "https://www.ipea.gov.br", fixed = TRUE)
  expect_match(html, " em 2026.", fixed = TRUE)
})

test_that("painel_brand_apoio sem pipe vira texto puro", {
  Sys.setenv(painel_apoio = "Painel institucional do programa X.")
  on.exit(Sys.unsetenv("painel_apoio"), add = TRUE)
  expect_identical(as.character(AEDi:::painel_brand_apoio()),
                   "<p>Painel institucional do programa X.</p>")
})

test_that("painel_brand_apoio com nome sem url nao gera link", {
  Sys.setenv(painel_apoio = "Apoio de |Vários parceiros|")
  on.exit(Sys.unsetenv("painel_apoio"), add = TRUE)
  html <- as.character(AEDi:::painel_brand_apoio())
  expect_identical(grepl("<a ", html, fixed = TRUE), FALSE)
  expect_match(html, "Vários parceiros", fixed = TRUE)
})

test_that("painel_brand_apoio vazio volta ao default", {
  Sys.setenv(painel_apoio = "")
  on.exit(Sys.unsetenv("painel_apoio"), add = TRUE)
  expect_match(as.character(AEDi:::painel_brand_apoio()),
               ">Distintive</a>", fixed = TRUE)
})
