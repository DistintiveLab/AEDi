# dependencias.R: partes puras (sem DB) — manifesto, parse e decisao

.deps_df <- function(serie, acao = "pular", regua = "proprio") {
  n <- length(serie)
  data.frame(serie = serie, regua = rep_len(regua, n),
             acao = rep_len(acao, n), stringsAsFactors = FALSE)
}

test_that("ler_dependencias retorna NULL sem manifesto", {
  d <- file.path(tempdir(), "sem_manifesto")
  dir.create(file.path(d, "coleta"), recursive = TRUE, showWarnings = FALSE)
  expect_null(AEDi:::ler_dependencias(d))
})

test_that("manifesto: colunas obrigatorias e valores validos", {
  d <- file.path(tempdir(), "manifesto")
  dir.create(file.path(d, "coleta"), recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(d, "coleta", "dependencias.csv")
  writeLines("script,dep_orig_name,regua\nx,datasus_popmun,proprio", csv)
  expect_error(AEDi:::ler_dependencias(d), "colunas obrigatorias")
  writeLines("script,dep_orig_name,regua,acao\nx,datasus_popmun,ano,pular", csv)
  expect_error(AEDi:::ler_dependencias(d), "regua invalida")
  writeLines("script,dep_orig_name,regua,acao\nx,datasus_popmun,proprio,executa", csv)
  expect_error(AEDi:::ler_dependencias(d), "acao invalida")
  writeLines("script,dep_orig_name,regua,acao,serie_propria\nx.R,datasus_popmun,proprio,pular,a;b",
             csv)
  tab <- AEDi:::ler_dependencias(d)
  expect_identical(tab$script, "x")
  expect_identical(AEDi:::.series_proprias_manifesto(tab), c("a", "b"))
  writeLines(c("script,dep_orig_name,regua,acao,serie_propria",
               "y,dep1,proprio,pular,a",
               "y,dep2,proprio,pular,b;c"), csv)
  expect_identical(AEDi:::.series_proprias_manifesto(AEDi:::ler_dependencias(d)),
                   c("a", "b", "c"))
})

test_that(".series_proprias_script detecta literais com e sem namespace", {
  d <- file.path(tempdir(), "script_parse")
  dir.create(file.path(d, "coleta"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c('AEDi:::gravar_serie_dw("serie_a", df, modo = "append")',
      "gravar_serie_dw('serie_b', df2)",
      'AEDi::db_datawrite("serie_c", df3)',
      "gravar_serie_dw(nome_comp, df4)",
      "x <- 1"),
    file.path(d, "coleta", "script_parse.R"))
  expect_identical(sort(AEDi:::.series_proprias_script("script_parse", d)),
                   sort(c("serie_a", "serie_b", "serie_c")))
})

test_that(".avaliar_dependencias: regua por ano, nao data completa", {
  frescor <- setNames(as.Date(c("2025-12-31", "2025-07-01")),
                      c("propria", "popmun"))
  res <- AEDi:::.avaliar_dependencias(.deps_df("popmun"), frescor, "propria")
  expect_false(res$pular)
  expect_identical(res$motivo, "")
})

test_that(".avaliar_dependencias: dep anterior a serie propria pula", {
  frescor <- setNames(as.Date(c("2025-12-31", "2024-12-31")),
                      c("propria", "dep_velha"))
  res <- AEDi:::.avaliar_dependencias(.deps_df("dep_velha"), frescor, "propria")
  expect_true(res$pular)
  expect_match(res$motivo, "dep_velha")
})

test_that(".avaliar_dependencias: dep ausente com serie propria existente pula", {
  frescor <- setNames(as.Date("2025-12-31"), "propria")
  res <- AEDi:::.avaliar_dependencias(.deps_df("sumida"), frescor, "propria")
  expect_true(res$pular)
  expect_match(res$motivo, "ausente do DW")
})

test_that(".avaliar_dependencias: primeira carga e sem deps executam", {
  frescor <- setNames(as.Date(c("2024-12-31", NA)), c("dep_velha", "futura"))
  res <- AEDi:::.avaliar_dependencias(.deps_df("dep_velha"), frescor, "futura")
  expect_false(res$pular)
  frescor2 <- setNames(as.Date("2025-12-31"), "propria")
  res2 <- AEDi:::.avaliar_dependencias(
    .deps_df(character()), frescor2, "propria")
  expect_false(res2$pular)
  expect_identical(res2$motivo, "")
})

test_that(".avaliar_dependencias: acao avisar nao bloqueia", {
  frescor <- setNames(as.Date(c("2025-12-31", "2024-12-31")),
                      c("propria", "lag"))
  res <- AEDi:::.avaliar_dependencias(.deps_df("lag", acao = "avisar"),
                                      frescor, "propria")
  expect_false(res$pular)
  expect_match(res$motivo, "lag")
})

test_that(".avaliar_dependencias: primeira violacao pular decide", {
  frescor <- setNames(as.Date(c("2025-12-31", "2024-12-31", "2023-12-31")),
                      c("propria", "lag_avisar", "lag_pular"))
  deps <- .deps_df(c("lag_avisar", "lag_pular"), acao = c("avisar", "pular"))
  res <- AEDi:::.avaliar_dependencias(deps, frescor, "propria")
  expect_true(res$pular)
  expect_match(res$motivo, "lag_pular")
})

test_that(".normalizar_registro: acao desconhecida vira avisar (fail-open)", {
  reg <- list(series_proprias = list("a"),
              deps = list(list(serie = "d1", regua = "proprio", acao = "pular"),
                          list(serie = "d2", regua = "fonte", acao = "executa")))
  norm <- AEDi:::.normalizar_registro(reg)
  expect_identical(norm$series_proprias, "a")
  expect_identical(norm$deps$acao, c("pular", "avisar"))
  expect_null(AEDi:::.normalizar_registro(list(deps = NULL)))
})
