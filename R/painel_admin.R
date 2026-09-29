# Configuracoes por indicador lidas pelo painel (fora do esqueleto) ------
#
# Duas tabelas auxiliares 1:1 com mdata, criadas sob demanda e lidas pelo
# painel de forma tolerante a ausencia (fail-open em painel_mdata_extras):
#
# - mdata_grafico (tipo_grafico): tipo do grafico em destaque da aba
#   Regiao — "linha" (default quando nao ha linha na tabela), "barras"
#   ou "lollipop";
# - mdata_visivel (visivel): indicadores ocultos dos seletores do painel
#   (Regiao/Mapa/Baixar) sem sair do DW — util para series em revisao,
#   variantes v0 e artefatos conhecidos. Ausencia de linha = visivel.

#' Define o tipo do grafico em destaque de um indicador no painel
#'
#' Cria a tabela auxiliar `mdata_grafico` no DW quando falta e grava o tipo
#' do indicador (UPSERT). O painel le a tabela de forma tolerante: sem
#' tabela ou sem linha para o indicador, o grafico segue "linha".
#'
#' @param con conexao DBI (PostgreSQL) com o DW de indicadores
#' @param mdata_id id do indicador na tabela mdata
#' @param tipo um de "linha", "barras", "lollipop"
#' @return invisivelmente TRUE
#' @export
definir_tipo_grafico <- function(con, mdata_id,
                                 tipo = c("linha", "barras", "lollipop")) {
  tipo <- match.arg(tipo)
  mdata_id <- suppressWarnings(as.integer(mdata_id)[1])
  if (is.na(mdata_id)) stop("mdata_id invalido", call. = FALSE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS mdata_grafico (",
    "mdata_id integer PRIMARY KEY",
    "REFERENCES mdata(mdata_id) ON DELETE CASCADE,",
    "tipo_grafico text NOT NULL)"))
  DBI::dbExecute(con, sprintf(paste(
    "INSERT INTO mdata_grafico (mdata_id, tipo_grafico)",
    "VALUES (%d, '%s')",
    "ON CONFLICT (mdata_id) DO UPDATE",
    "SET tipo_grafico = EXCLUDED.tipo_grafico"),
    mdata_id, tipo))
  invisible(TRUE)
}

#' Esconde ou revela um indicador nos seletores do painel
#'
#' Cria a tabela auxiliar `mdata_visivel` no DW quando falta e grava a
#' flag do indicador (UPSERT). Indicadores ocultos saem dos seletores
#' das abas Regiao, Mapa e Baixar, mas a serie continua no DW (downloads
#' direto do banco e cargas seguem intactos). O painel le a tabela de
#' forma tolerante: sem tabela ou sem linha, o indicador e visivel.
#'
#' @param con conexao DBI (PostgreSQL) com o DW de indicadores
#' @param mdata_id id do indicador na tabela mdata
#' @param visivel FALSE esconde o indicador dos seletores (default TRUE)
#' @return invisivelmente TRUE
#' @export
definir_visibilidade <- function(con, mdata_id, visivel = TRUE) {
  mdata_id <- suppressWarnings(as.integer(mdata_id)[1])
  if (is.na(mdata_id)) stop("mdata_id invalido", call. = FALSE)
  if (!isTRUE(visivel) && !identical(visivel, FALSE))
    stop("visivel deve ser TRUE ou FALSE", call. = FALSE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS mdata_visivel (",
    "mdata_id integer PRIMARY KEY",
    "REFERENCES mdata(mdata_id) ON DELETE CASCADE,",
    "visivel boolean NOT NULL DEFAULT TRUE)"))
  DBI::dbExecute(con, sprintf(paste(
    "INSERT INTO mdata_visivel (mdata_id, visivel) VALUES (%d, %s)",
    "ON CONFLICT (mdata_id) DO UPDATE",
    "SET visivel = EXCLUDED.visivel"),
    mdata_id, if (isTRUE(visivel)) "TRUE" else "FALSE"))
  invisible(TRUE)
}
