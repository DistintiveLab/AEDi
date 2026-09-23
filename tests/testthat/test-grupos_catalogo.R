# Catalogo de grupos do DW (Eixos, Objetivos, Estratos PNAD) ----------

grupos_catalogo_vazio <- function() {
  list(
    grupos = data.frame(datagroup_id = integer(0),
                        datagroup_name = character(0)),
    vinculos = data.frame(mdata_id = integer(0), datagroup_id = integer(0)),
    pais = data.frame(datagroup_id = integer(0),
                      datagroup_parentid = integer(0)))
}

test_that("grupos_catalogo_definicao traz os 12 grupos com os ids do aedidb", {
  def <- AEDi:::grupos_catalogo_definicao()
  # 3 raizes (Eixos, Objetivos, Estratos PNAD) + 7 eixos + 4 objetivos
  expect_identical(nrow(def), 14L)
  expect_identical(def$datagroup_id, c(13L, 14L, 8L, 1:7, 9:12))
  expect_identical(def$datagroup_name,
                   c("Eixos", "Objetivos", "Estratos PNAD",
                     paste0("Eixo ", 1:7), paste0("Objetivo ", 1:4)))
  expect_setequal(unique(def$datagroup_parent[!is.na(def$datagroup_parent)]),
                  c("Eixos", "Objetivos"))
  # so as raizes tem descricao: o texto de Eixo/Objetivo e editorial, do DW
  expect_identical(is.na(def$datagroup_desc),
                   c(rep(FALSE, 3), rep(TRUE, 11)))
})

test_that("grupos_catalogo_tribbles resolve os pais por nome e tipa os ids", {
  tb <- AEDi:::grupos_catalogo_tribbles()
  expect_identical(names(tb$datagroup),
                   c("datagroup_id", "datagroup_name", "datagroup_desc"))
  expect_identical(names(tb$group_parent),
                   c("datagroup_id", "datagroup_parentid"))
  expect_type(tb$datagroup$datagroup_id, "integer")
  expect_type(tb$group_parent$datagroup_id, "integer")
  expect_type(tb$group_parent$datagroup_parentid, "integer")
  expect_identical(tb$group_parent$datagroup_id, c(1:7, 9:12))
  expect_identical(tb$group_parent$datagroup_parentid, c(rep(13L, 7),
                                                         rep(14L, 4)))
  # Estratos PNAD e raiz, nao entra em group_parent
  expect_false(8L %in% tb$group_parent$datagroup_id)
})

test_that("grupos_catalogo_plano declara o catalogo inteiro num DW sem grupos", {
  mdata <- data.frame(mdata_id = 1:5,
                      orig_name = c("educ1", "comp_educ", "citec3",
                                    "objetivo2_1", "pnadc5"))
  v <- grupos_catalogo_vazio()
  plano <- AEDi:::grupos_catalogo_plano(mdata, v$grupos, v$vinculos, v$pais)
  expect_identical(nrow(plano$grupos), 14L)
  expect_identical(plano$grupos$datagroup_id, 1:14)
  expect_identical(nrow(plano$pais), 11L)
  expect_identical(nrow(plano$vinculos), 5L)
  expect_identical(nrow(plano$divergentes), 0L)
  nomes <- stats::setNames(plano$grupos$datagroup_name,
                           plano$grupos$datagroup_id)
  expect_identical(unname(nomes[as.character(plano$vinculos$datagroup_id)]),
                   c("Eixo 1", "Eixo 1", "Eixo 2", "Objetivo 2",
                     "Estratos PNAD"))
  # num DW vazio os ids saem da ordem da definicao: Eixos=1, Objetivos=2,
  # Estratos PNAD=3, Eixo 1..7=4..10, Objetivo 1..4=11..14
  expect_identical(plano$pais$datagroup_id, c(4:10, 11:14))
  expect_identical(unique(plano$pais$datagroup_parentid), c(1L, 2L))
})

test_that("grupos_catalogo_plano nao repete grupo, pai nem vinculo existentes", {
  def <- AEDi:::grupos_catalogo_definicao()
  mdata <- data.frame(mdata_id = 1:3,
                      orig_name = c("educ1", "objetivo1_1", "pnadc2"))
  grupos <- def[, c("datagroup_id", "datagroup_name")]
  pais <- AEDi:::grupos_catalogo_tribbles()$group_parent
  vinculos <- data.frame(mdata_id = c(1L, 2L), datagroup_id = c(1L, 9L))
  plano <- AEDi:::grupos_catalogo_plano(mdata, grupos, vinculos, pais)
  expect_identical(nrow(plano$grupos), 0L)
  expect_identical(nrow(plano$pais), 0L)
  expect_identical(plano$vinculos,
                   data.frame(mdata_id = 3L, datagroup_id = 8L))
})

test_that("grupos_catalogo_plano relata vinculo fora da convencao sem apagar", {
  def <- AEDi:::grupos_catalogo_definicao()
  mdata <- data.frame(mdata_id = 1:2,
                      orig_name = c("educ1", "datasus_popmun"))
  grupos <- def[, c("datagroup_id", "datagroup_name")]
  pais <- AEDi:::grupos_catalogo_tribbles()$group_parent
  vinculos <- data.frame(mdata_id = c(1L, 1L, 2L),
                         datagroup_id = c(3L, 1L, 5L))
  plano <- AEDi:::grupos_catalogo_plano(mdata, grupos, vinculos, pais)
  expect_identical(nrow(plano$grupos), 0L)
  expect_identical(nrow(plano$pais), 0L)
  expect_identical(nrow(plano$vinculos), 0L)
  expect_identical(nrow(plano$divergentes), 2L)
  expect_identical(sort(plano$divergentes$datagroup_name),
                   c("Eixo 3", "Eixo 5"))
  expect_identical(plano$divergentes$esperado[plano$divergentes$mdata_id == 1L],
                   1L)
  expect_true(is.na(plano$divergentes$esperado[plano$divergentes$mdata_id == 2L]))
})

test_that("atualizar_grupos_indicadores e idempotente e nao apaga vinculos", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE mdata (mdata_id INTEGER, orig_name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE datagroup (datagroup_id INTEGER, datagroup_name TEXT, datagroup_desc TEXT)")
  DBI::dbExecute(con, "CREATE TABLE group_parent (datagroup_id INTEGER, datagroup_parentid INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE mdata_group (mdata_id INTEGER, datagroup_id INTEGER)")
  DBI::dbExecute(con, "INSERT INTO mdata VALUES (1, 'educ1'), (2, 'objetivo1_1'), (3, 'pnadc2'), (4, 'datasus_popmun')")
  contar <- function(tabela) {
    as.integer(DBI::dbGetQuery(con,
                               paste("SELECT COUNT(*) AS n FROM", tabela))$n)
  }

  simulado <- AEDi::atualizar_grupos_indicadores(con, simular = TRUE,
                                                 verbose = FALSE)
  expect_identical(nrow(simulado$grupos), 14L)
  expect_identical(nrow(simulado$vinculos), 3L)
  expect_identical(contar("datagroup"), 0L)

  plano <- AEDi::atualizar_grupos_indicadores(con, verbose = FALSE)
  expect_identical(nrow(plano$grupos), 14L)
  expect_identical(nrow(plano$pais), 11L)
  expect_identical(nrow(plano$vinculos), 3L)
  expect_identical(contar("datagroup"), 14L)
  expect_identical(contar("group_parent"), 11L)
  expect_identical(contar("mdata_group"), 3L)

  segunda <- AEDi::atualizar_grupos_indicadores(con, verbose = FALSE)
  expect_identical(nrow(segunda$grupos), 0L)
  expect_identical(nrow(segunda$pais), 0L)
  expect_identical(nrow(segunda$vinculos), 0L)
  expect_identical(contar("datagroup"), 14L)
  expect_identical(contar("group_parent"), 11L)
  expect_identical(contar("mdata_group"), 3L)

  DBI::dbExecute(con, "INSERT INTO mdata_group VALUES (4, 5)")
  terceira <- AEDi::atualizar_grupos_indicadores(con, verbose = FALSE)
  expect_identical(nrow(terceira$divergentes), 1L)
  expect_identical(contar("mdata_group"), 4L)
})
