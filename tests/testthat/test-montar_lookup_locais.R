# Regressao do bug de cobertura municipal (2026-09-22): o lookup 6d era
# sombreado pelo geoloc_id (texto) das Regioes Imediatas, que tambem tem 6
# digitos, e a serie municipal era gravada no local da RGINT.
locais_sinteticos <- data.frame(
  local_id  = c(1, 2, 6431, 7000),
  geoloc_id = c(1100023, 1100054, 110002, 33)
)

test_that("prefixo 6d (RAIS) tem prioridade sobre geoloc de RGINT", {
  lookup <- AEDi:::montar_lookup_locais(locais_sinteticos)
  expect_identical(unname(lookup[["110002"]]), 1)
})

test_that("geoloc 7d, prefixo sem colisao e local_id continuam resolvendo", {
  lookup <- AEDi:::montar_lookup_locais(locais_sinteticos)
  expect_identical(unname(lookup[["1100023"]]), 1)
  expect_identical(unname(lookup[["1100054"]]), 2)
  expect_identical(unname(lookup[["110005"]]), 2)
  expect_identical(unname(lookup[["6431"]]), 6431)
  expect_identical(unname(lookup[["7000"]]), 7000)
})

test_that("RGINT nao gera nome de prefixo 6d (filtro local_id < 6000)", {
  lookup <- AEDi:::montar_lookup_locais(locais_sinteticos)
  expect_false("11000" %in% names(lookup))
  expect_identical(unname(lookup[["33"]]), 7000)
})
