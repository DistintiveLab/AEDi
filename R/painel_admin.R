# Configuracoes por indicador lidas pelo painel (fora do esqueleto) ------
#
# Duas tabelas auxiliares 1:1 com mdata, criadas sob demanda e lidas pelo
# painel de forma tolerante a ausencia (fail-open em painel_mdata_extras):
#
# - mdata_grafico (tipo_grafico): tipo do grafico em destaque da aba
#   Regiao — "linha" (default quando nao ha linha na tabela), "barras",
#   "lollipop" ou "banda" (min/mediana/max do nivel aberto com a
#   localidade em destaque);
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
#' @param tipo um de "linha", "barras", "lollipop", "banda"
#' @return invisivelmente TRUE
#' @export
definir_tipo_grafico <- function(con, mdata_id,
                                 tipo = c("linha", "barras", "lollipop",
                                          "banda")) {
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

#' Garante o indice de `local_id` na tabela `data_values` do DW
#'
#' `painel_niveis()` e `painel_locais_nivel()` descobrem "quais localidades
#' tem dados" com EXISTS sobre data_values: sem indice em local_id o
#' planejador varre a tabela inteira (~10 milhoes de linhas). O indice
#' tambem acelera o lookup do aquecedor e da regiao por localidade.
#' `CREATE INDEX IF NOT EXISTS` — idempotente; a primeira criacao pode
#' levar ~1 minuto e ocupa espaco extra no banco.
#'
#' @param con conexao DBI (PostgreSQL) com o DW de indicadores
#' @return invisivelmente TRUE
#' @export
garantir_indice_valores <- function(con) {
  DBI::dbExecute(con, paste(
    "CREATE INDEX IF NOT EXISTS idx_data_values_local_id",
    "ON data_values (local_id)"))
  invisible(TRUE)
}

#' Aquece o cache de disco do painel DW apos um ETL
#'
#' Executa as leituras que o painel faria a frio — catalogo, geometrias,
#' localidades por nivel e (com `valores = TRUE`) a serie achatada de cada
#' indicador — gravando os RDS em `cache/` sob `diretorio`, para o proximo
#' arranque do app sair quente do disco. O cache e relativo ao diretorio
#' de trabalho e as chaves carregam host+dbname: rode com o mesmo ambiente
#' do app (o `.Renviron` da pasta, quando existe, e lido antes).
#' `limpar = TRUE` apaga o cache antes, levando embora chaves orfas de
#' versoes antigas (ex.: `valores_ano_<id>_<ano>` do cache por ano).
#'
#' Usado pelo gancho pos-ETL de [atualizar_indicadores()] e disponivel
#' para rodar na mao (Rscript/cron) depois de qualquer carga manual.
#'
#' @param diretorio raiz do app do painel (default: diretorio corrente)
#' @param valores aquece tambem as series achatadas por indicador — a
#'   etapa mais longa; FALSE aquece so catalogo e geometrias
#' @param limpar apaga o cache de disco antes de aquecer (default TRUE)
#' @return invisivelmente um data.frame com o tempo de cada etapa
#' @export
aquecer_painel <- function(diretorio = NULL, valores = TRUE, limpar = TRUE) {
  diretorio <- if (is.null(diretorio)) getwd() else
    path.expand(trimws(as.character(diretorio)[1]))
  if (!dir.exists(diretorio))
    stop("diretorio inexistente: ", diretorio, call. = FALSE)
  renviron <- file.path(diretorio, ".Renviron")
  if (file.exists(renviron))
    tryCatch(readRenviron(renviron), error = function(e) NULL)
  wd_antigo <- getwd()
  on.exit(setwd(wd_antigo), add = TRUE)
  setwd(diretorio)
  if (isTRUE(limpar)) {
    alvo <- painel_cache_dir()
    if (dir.exists(alvo)) unlink(alvo, recursive = TRUE)
  }
  tempos <- list()
  marcar <- function(etapa, expressao) {
    t0 <- proc.time()
    force(expressao)
    tempos[[etapa]] <<- (proc.time() - t0)[["elapsed"]]
  }
  marcar("catalogo_mdata", painel_mdata_cache())
  marcar("catalogo_niveis", painel_niveis_cache())
  marcar("catalogo_hierarquia", painel_hierarquia_cache())
  marcar("catalogo_compostos", painel_compostos_cache())
  marcar("catalogo_codigo_mun", painel_codigo_mun_cache())
  marcar("geo_mun", painel_geo_mun_cache())
  marcar("geo_uf", painel_geo_uf_cache())
  niveis <- as.character(painel_niveis_cache()$nivel_id)
  for (nivel in niveis) {
    marcar(paste0("locais_nivel_", nivel),
           painel_locais_nivel_cache(nivel))
    marcar(paste0("geo_nivel_", nivel), painel_geo_nivel_cache(nivel))
  }
  md <- painel_mdata_cache()
  t0 <- proc.time()
  for (id in md$mdata_id) painel_anos_cache(id)
  tempos[["anos_por_indicador"]] <- (proc.time() - t0)[["elapsed"]]
  if (isTRUE(valores)) {
    t0 <- proc.time()
    for (id in md$mdata_id) painel_valores_por_ano_cache(id)
    tempos[["valores_por_ano"]] <- (proc.time() - t0)[["elapsed"]]
  }
  res <- data.frame(etapa = names(tempos),
                    segundos = round(unlist(tempos, use.names = FALSE), 3))
  rownames(res) <- NULL
  print(res)
  invisible(res)
}
