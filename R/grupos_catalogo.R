# Catalogo de grupos tematicos do DW (Eixos, Objetivos e Estratos PNAD).
#
# As tabelas datagroup/group_parent/mdata_group existem para permitir recortes
# e relacoes entre grupos, mas so db_datawrite(grp=TRUE) escrevia nelas — e
# apenas criando grupos de classificadores do SIDRA no formato
# "<orig_name>_grps". Nenhuma funcao do AEDi declarava nem mantinha os grupos
# tematicos: as linhas de "Eixo 1".."Eixo 7", "Objetivo 1".."Objetivo 4",
# "Eixos" e "Objetivos" do aedidb foram inseridas fora do pacote, e o vinculo
# indicador<->grupo ficou desatualizado (conferido 2026-09-22: 27 linhas em
# mdata_group, cobrindo so o Eixo 1, o Eixo 3, os Estratos PNAD e os Objetivos
# 1 a 3 — os eixos 2, 4, 5, 6 e 7 e o Objetivo 4 tinham zero vinculos).
#
# Este arquivo traz as duas pecas que faltavam:
#   - [grupos_catalogo_tribbles()] declara os grupos num DW novo (usado por
#     [prepare_db()]), com os mesmos ids do aedidb em producao;
#   - [atualizar_grupos_indicadores()] reconcilia (idempotente, sem apagar
#     nada) os grupos e os vinculos num DW ja existente, e pode ser rodada
#     depois de qualquer ETL que adicione indicadores.
#
# A convencao que liga cada indicador ao seu grupo (educ* + comp_educ no Eixo
# 1, objetivo*_* + comp_objetivo* nos Objetivos, pnadc* + comp_pnadc* nos
# Estratos PNAD) fica em uma unica definicao, em R/painel_dw.R
# ([painel_grupo_indicador()]/[painel_grupo_raiz()]), porque o painel tambem
# precisa dela: o esqueleto do painel e autonomo e nao leva este arquivo.

#' Grupos tematicos do catalogo com ids, pais e descricao
#'
#' Os ids repetem os do `aedidb` em producao para que `mdata_group`,
#' `group_parent` e os recortes geograficos continuem validos.
#' @keywords internal
grupos_catalogo_definicao <- function() {
  data.frame(
    datagroup_id = c(13L, 14L, 8L, 1:7, 9:12),
    datagroup_name = c("Eixos", "Objetivos", "Estratos PNAD",
                       paste0("Eixo ", 1:7), paste0("Objetivo ", 1:4)),
    datagroup_parent = c(NA_character_, NA_character_, NA_character_,
                         rep("Eixos", 7), rep("Objetivos", 4)),
    datagroup_desc = c("Eixos da PNDR", "Objetivos da PNDR",
                       "Indicadores de desenvolvimento por estratos da PNAD (nao municipais)",
                       rep(NA_character_, 11)),
    stringsAsFactors = FALSE)
}

#' Linhas de `datagroup`/`group_parent` que declaram o catalogo num DW novo
#'
#' Devolve os dois tribbles tipados como em [prepare_db()], prontos para
#' `rbind` antes da escrita das tabelas (a descricao de Eixo/Objetivo fica NA
#' de propósito: o texto do DW em producao e editorial e nao deve ser
#' sobrescrito nem duplicado no codigo).
#' @keywords internal
grupos_catalogo_tribbles <- function() {
  def <- grupos_catalogo_definicao()
  datagroup <- def[, c("datagroup_id", "datagroup_name", "datagroup_desc")]
  group_parent <- def[!is.na(def$datagroup_parent),
                      c("datagroup_id", "datagroup_parent")]
  names(group_parent)[2] <- "datagroup_parentid"
  group_parent$datagroup_parentid <- def$datagroup_id[
    match(group_parent$datagroup_parentid, def$datagroup_name)]
  datagroup$datagroup_id <- as.integer(datagroup$datagroup_id)
  datagroup$datagroup_name <- as.character(datagroup$datagroup_name)
  datagroup$datagroup_desc <- as.character(datagroup$datagroup_desc)
  group_parent$datagroup_id <- as.integer(group_parent$datagroup_id)
  group_parent$datagroup_parentid <- as.integer(group_parent$datagroup_parentid)
  rownames(datagroup) <- NULL
  rownames(group_parent) <- NULL
  list(datagroup = datagroup, group_parent = group_parent)
}

#' Plano de atualizacao do catalogo: o que falta no DW
#'
#' Funcao pura: recebe o conteudo atual das tabelas e devolve as linhas que
#' seriam inseridas, sem tocar no banco. Os grupos que faltam ja saem com
#' `datagroup_id` atribuido (continuando a numeracao existente), de modo que os
#' pais e os vinculos possam ser resolvidos no mesmo passo e o plano seja
#' exatamente o que [atualizar_grupos_indicadores()] aplica. Os pais sao
#' comparados por nome e os vinculos pelo par (mdata_id, datagroup_id).
#'
#' @param mdata data.frame com `mdata_id` e `orig_name`
#' @param grupos data.frame com `datagroup_id` e `datagroup_name`
#' @param vinculos data.frame com `mdata_id` e `datagroup_id` (mdata_group)
#' @param pais data.frame com `datagroup_id` e `datagroup_parentid`
#'
#' @return lista com `grupos`, `pais` e `vinculos` (linhas a inserir) e
#'   `divergentes` (vinculos existentes que fogem da convencao; so relatorio)
#' @keywords internal
grupos_catalogo_plano <- function(mdata, grupos, vinculos, pais) {
  def <- grupos_catalogo_definicao()
  nome_do_id <- stats::setNames(as.character(grupos$datagroup_name),
                                as.integer(grupos$datagroup_id))
  id_do_nome <- stats::setNames(as.integer(grupos$datagroup_id),
                                as.character(grupos$datagroup_name))

  # grupos ausentes entram no fim da numeracao, como faz a insercao
  faltam <- !(def$datagroup_name %in% names(id_do_nome))
  novos_grupos <- def[faltam, c("datagroup_name", "datagroup_parent",
                                "datagroup_desc")]
  if (nrow(novos_grupos)) {
    ids_novos <- max(c(as.integer(grupos$datagroup_id), 0L)) +
      seq_len(nrow(novos_grupos))
    nome_do_id <- c(nome_do_id, stats::setNames(
      as.character(novos_grupos$datagroup_name), ids_novos))
    id_do_nome <- c(id_do_nome, stats::setNames(
      as.integer(ids_novos), as.character(novos_grupos$datagroup_name)))
  }
  novos_grupos$datagroup_id <- unname(id_do_nome[novos_grupos$datagroup_name])
  novos_grupos <- novos_grupos[, c("datagroup_id", "datagroup_name",
                                   "datagroup_desc")]
  novos_grupos$datagroup_id <- as.integer(novos_grupos$datagroup_id)
  rownames(novos_grupos) <- NULL

  desejados <- def[!is.na(def$datagroup_parent), c("datagroup_name",
                                                   "datagroup_parent")]
  par_pai <- function(id, pai) paste0(nome_do_id[as.character(id)], "|",
                                      nome_do_id[as.character(pai)])
  chaves <- if (nrow(pais)) par_pai(pais$datagroup_id,
                                    pais$datagroup_parentid) else character(0)
  novos_pais <- desejados[!(paste0(desejados$datagroup_name, "|",
                                   desejados$datagroup_parent) %in% chaves), ,
                          drop = FALSE]
  novos_pais <- data.frame(
    datagroup_id = as.integer(unname(id_do_nome[novos_pais$datagroup_name])),
    datagroup_parentid = as.integer(unname(id_do_nome[novos_pais$datagroup_parent])))
  rownames(novos_pais) <- NULL

  grupo_indicador <- painel_grupo_indicador(mdata$orig_name)
  alvo <- !is.na(grupo_indicador) & grupo_indicador %in% names(id_do_nome)
  novos_vinculos <- data.frame(
    mdata_id = as.integer(mdata$mdata_id[alvo]),
    datagroup_id = as.integer(unname(id_do_nome[grupo_indicador[alvo]])))
  if (nrow(novos_vinculos) && nrow(vinculos)) {
    atual <- paste0(as.integer(vinculos$mdata_id), "|",
                    as.integer(vinculos$datagroup_id))
    novos_vinculos <- novos_vinculos[
      !(paste0(novos_vinculos$mdata_id, "|", novos_vinculos$datagroup_id) %in%
          atual), , drop = FALSE]
  }
  rownames(novos_vinculos) <- NULL

  # vinculos que existem no DW mas nao seguem a convencao (ou apontam para
  # grupo de outro indicador): so relatorio, o pacote nunca apaga vinculo
  if (nrow(vinculos)) {
    esperado <- id_do_nome[grupo_indicador]
    divergentes <- data.frame(
      mdata_id = as.integer(vinculos$mdata_id),
      datagroup_id = as.integer(vinculos$datagroup_id),
      datagroup_name = unname(nome_do_id[as.character(vinculos$datagroup_id)]),
      esperado = unname(esperado[match(vinculos$mdata_id,
                                       as.integer(mdata$mdata_id))]))
    divergentes <- divergentes[is.na(divergentes$esperado) |
                                 divergentes$datagroup_id !=
                                   divergentes$esperado, , drop = FALSE]
    rownames(divergentes) <- NULL
  } else {
    divergentes <- data.frame(mdata_id = integer(0), datagroup_id = integer(0),
                              datagroup_name = character(0),
                              esperado = integer(0))
  }
  list(grupos = novos_grupos, pais = novos_pais, vinculos = novos_vinculos,
       divergentes = divergentes)
}

#' Declara e reconcilia os grupos tematicos do catalogo no DW
#'
#' Idempotente e nao destrutivo: cria os `datagroup`/`group_parent` que faltam
#' (Eixos, Objetivos, Estratos PNAD e os grupos Eixo 1..7 / Objetivo 1..4),
#' completa os vinculos de `mdata_group` de todos os indicadores que seguem a
#' convencao de `orig_name` e relata os vinculos que fogem dela — sem apagar
#' nenhuma linha. Rode depois de ETLs que adicionem indicadores; num DW recem
#' criado (sem indicadores) apenas declara os grupos.
#'
#' @param con conexao aberta com o DW; por padrao abre uma com as variaveis
#'   de ambiente `user`, `password`, `host` e `dbname` (mesmos defaults de
#'   [db_datawrite()])
#' @param simular se `TRUE` nao escreve nada: devolve apenas o plano
#' @param verbose imprime o resumo do que foi feito
#'
#' @return invisivel, o plano aplicado (listas `grupos`, `pais`, `vinculos` e
#'   `divergentes`)
#' @export
atualizar_grupos_indicadores <- function(con = NULL, simular = FALSE,
                                         verbose = TRUE) {
  fechar <- is.null(con)
  if (fechar) {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          user = Sys.getenv("user", "aedi"),
                          password = Sys.getenv("password", "aEd1#man@gR"),
                          host = Sys.getenv("host", "127.0.0.1"),
                          dbname = Sys.getenv("dbname", "aedidb"))
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  ler <- function(tabela, colunas) {
    DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s",
                                 paste(colunas, collapse = ", "), tabela))
  }
  mdata <- ler("mdata", c("mdata_id", "orig_name"))
  grupos <- ler("datagroup", c("datagroup_id", "datagroup_name"))
  vinculos <- ler("mdata_group", c("mdata_id", "datagroup_id"))
  pais <- ler("group_parent", c("datagroup_id", "datagroup_parentid"))
  plano <- grupos_catalogo_plano(mdata, grupos, vinculos, pais)

  if (!simular) {
    if (nrow(plano$grupos)) {
      novos <- plano$grupos
      novos$datagroup_desc <- ifelse(is.na(novos$datagroup_desc), "",
                                     novos$datagroup_desc)
      DBI::dbAppendTable(con, "datagroup", novos)
    }
    if (nrow(plano$pais)) {
      DBI::dbAppendTable(con, "group_parent", plano$pais)
    }
    if (nrow(plano$vinculos)) {
      DBI::dbAppendTable(con, "mdata_group", plano$vinculos)
    }
  }

  if (verbose) {
    cat(if (simular) "Simulacao" else "Catalogo de grupos atualizado",
        "-", nrow(plano$grupos), "grupo(s),", nrow(plano$pais),
        "vinculo(s) grupo-pai,", nrow(plano$vinculos), "vinculo(s) indicador-grupo",
        if (nrow(plano$divergentes))
          paste0("; ", nrow(plano$divergentes),
                 " vinculo(s) fora da convencao (revisar)") else "", "\n")
  }
  invisible(plano)
}
