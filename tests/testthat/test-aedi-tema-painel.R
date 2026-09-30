# Tema no painel de indicadores (T4): painel_recursos sobre o núcleo -------

test_that("painel_recursos inclui o núcleo do tema antes dos assets locais", {
  assets <- system.file("painel", package = "AEDi")
  skip_if_not(nzchar(assets))
  r <- AEDi:::painel_recursos(assets, paleta = "pb")
  h <- paste(as.character(r), collapse = "")
  # núcleo inlineado (variáveis + toggle) e div raiz renomeada
  expect_true(grepl("body.aedi-pb", h, fixed = TRUE))
  expect_true(grepl("aedi:paleta", h, fixed = TRUE))
  expect_true(grepl("aedi_tema_raiz", h, fixed = TRUE))
  expect_true(grepl('data-paleta="pb"', h, fixed = TRUE))
  # núcleo vem antes do painel.css (layout sobrepõe quando empata)
  expect_lt(regexpr("body.aedi-pb", h, fixed = TRUE),
            regexpr("painel-topbar", h, fixed = TRUE))
  # antiga div raiz não existe mais
  expect_false(grepl('id="painel_raiz"', h, fixed = TRUE))
})

test_that("painel_recursos prefere assets locais do esqueleto", {
  d <- withr::local_tempfile(pattern = "painel_tema")
  dir.create(d, recursive = TRUE)
  writeLines("/* núcleo local */", file.path(d, "aedi-tema.css"))
  file.copy(list.files(system.file("painel", package = "AEDi"),
                       full.names = TRUE), d, overwrite = TRUE)
  r <- AEDi:::painel_recursos(d, paleta = "govbr")
  h <- paste(as.character(r), collapse = "")
  expect_true(grepl("/* núcleo local */", h, fixed = TRUE))
  # js do núcleo cai para o embutido no pacote
  expect_true(grepl("aedi:paleta", h, fixed = TRUE))
})

test_that("painel_topbar usa o botão do núcleo com id estável", {
  tb <- AEDi:::painel_topbar("Título")
  h <- paste(as.character(tb), collapse = "")
  expect_true(grepl("painel_paleta_btn", h, fixed = TRUE))
  expect_true(grepl("aedi-tema-btn", h, fixed = TRUE))
  expect_false(grepl("painel-paleta-btn", h, fixed = TRUE))
})
