# Controle de execucao de coletas/indicadores no PostgreSQL "aedidb"
# (fase C2/C3 do roadmap_aedi_agendamento.md, portado do conecta_turismo)
#
# Tabelas criadas por controle_preparar():
#   controle_execucao          - estado atual por (nome_script, etapa)
#   controle_execucao_historico - log de cada execucao
#
# Convencoes: mesma conexao do DW do AEDi (env vars user/password/host/dbname,
# defaults locais). Sem dependencia do duckdb.

#' Conexao com o banco do DW (aedidb)
#' @keywords internal
controle_con <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    user     = Sys.getenv("user",     "aedi"),
    password = Sys.getenv("password", "aEd1#man@gR"),
    host     = Sys.getenv("host",     "127.0.0.1"),
    dbname   = Sys.getenv("dbname",   "aedidb")
  )
}

#' Cria (se ausentes) as tabelas de controle e versoes de carga no aedidb
#' @export
controle_preparar <- function() {
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS controle_execucao (
      nome_script            TEXT PRIMARY KEY,
      etapa                  TEXT,
      ultima_atualizacao     TIMESTAMPTZ,
      ultima_verificacao     TIMESTAMPTZ,
      primeira_carga         TIMESTAMPTZ,
      status                 TEXT,
      linhas_ultima_carga    BIGINT,
      detalhe                TEXT,
      dependencias_json      JSONB,
      atualizado_por         TEXT,
      hash_estado            TEXT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS controle_execucao_historico (
      id            BIGSERIAL PRIMARY KEY,
      nome_script   TEXT,
      inicio        TIMESTAMPTZ,
      fim           TIMESTAMPTZ,
      sucesso       BOOLEAN,
      mensagem      TEXT,
      linhas        BIGINT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS versoes_carga (
      versao          INTEGER PRIMARY KEY,
      iniciado_em     TIMESTAMPTZ,
      finalizado_em   TIMESTAMPTZ,
      codigo_versao   TEXT,
      dump_arquivo    TEXT,
      n_scripts       INTEGER,
      n_ok            INTEGER,
      n_erro          INTEGER,
      observacao      TEXT
    )")
  invisible(TRUE)
}

#' Registra o inicio de um ciclo de carga (modelo A: snapshot por ciclo)
#'
#' Executa pg_dump do aedidb para <dir>/aedidb_v<N>_<AAAAMMDD>.dump e cria a
#' linha da versao. Retorna o numero da versao.
#'
#' @param dir_dump diretorio dos dumps (default ~/backups_aedidb)
#' @param codigo_versao identificador do codigo (default: commit git do AEDi)
#' @export
versao_carga_inicio <- function(dir_dump = "~/backups_aedidb",
                                codigo_versao = git_commit_aedi()) {
  controle_preparar()
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  versao <- 1 + DBI::dbGetQuery(con,
    "SELECT coalesce(max(versao),0) AS v FROM versoes_carga")$v

  dir_dump <- path.expand(dir_dump)
  dir.create(dir_dump, recursive = TRUE, showWarnings = FALSE)
  arquivo <- file.path(dir_dump, sprintf("aedidb_v%d_%s.dump", versao,
                                         format(Sys.Date(), "%Y%m%d")))

  # pg_dump com as mesmas credenciais do DW (filho herda o ambiente)
  .pw_antigo <- Sys.getenv("PGPASSWORD")
  Sys.setenv(PGPASSWORD = Sys.getenv("password", "aEd1#man@gR"))
  on.exit(Sys.setenv(PGPASSWORD = .pw_antigo), add = TRUE)
  args <- c("-Fc", "-d", Sys.getenv("dbname", "aedidb"))
  host <- Sys.getenv("host", "127.0.0.1")
  if (nzchar(host)) args <- c(args, "-h", host)
  args <- c(args, "-U", Sys.getenv("user", "aedi"), "-f", arquivo)
  status <- suppressWarnings(
    system2("pg_dump", args, stdout = FALSE, stderr = FALSE))
  if (!identical(status, 0L) || !file.exists(arquivo))
    stop("pg_dump falhou (status ", status, ") para ", arquivo)

  DBI::dbExecute(con,
    "INSERT INTO versoes_carga (versao, iniciado_em, codigo_versao, dump_arquivo)
     VALUES ($1, now(), $2, $3)",
    params = list(versao, codigo_versao, arquivo))
  message("versao ", versao, ": snapshot em ", arquivo,
          " (", format(structure(file.size(arquivo), class = "object_size"),
                       units = "auto"), ")")
  invisible(versao)
}

#' Fecha o ciclo de carga registrando o resumo da execucao
#' @export
versao_carga_fim <- function(versao, n_scripts, n_ok, n_erro, observacao = "") {
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "UPDATE versoes_carga SET finalizado_em = now(), n_scripts = $2,
        n_ok = $3, n_erro = $4, observacao = $5 WHERE versao = $1",
    params = list(versao, n_scripts, n_ok, n_erro, observacao))
  invisible(TRUE)
}

# commit git do repo AEDi (ou NA fora de repo/git)
git_commit_aedi <- function() {
  tryCatch({
    out <- suppressWarnings(system2("git",
      c("-C", system.file(package = "AEDi") |> dirname() |> dirname(),
        "rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE))
    if (length(out)) out[1] else NA_character_
  }, error = function(e) NA_character_)
}

#' Le o controle de um script (ou de todos)
#' @param nome_script nome do script de coleta (ex.: "objetivo2_1_aedi");
#'   NULL retorna data.frame com todos
#' @export
ler_controle <- function(nome_script = NULL) {
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  if (is.null(nome_script))
    return(DBI::dbGetQuery(con, "SELECT * FROM controle_execucao
                                      ORDER BY nome_script"))
  DBI::dbGetQuery(con, "SELECT * FROM controle_execucao WHERE nome_script = $1",
                  params = list(nome_script))
}

#' Registra o inicio de uma execucao (retorna id do historico p/ fechar depois)
#' @export
controle_inicio <- function(nome_script, etapa = "coleta", por = Sys.info()[["user"]]) {
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "INSERT INTO controle_execucao_historico (nome_script, inicio)
     VALUES ($1, now())", params = list(nome_script))
  DBI::dbGetQuery(con, "SELECT max(id) AS id FROM controle_execucao_historico
                        WHERE nome_script = $1", params = list(nome_script))$id
}

#' Fecha a execucao e atualiza o estado atual do script
#' @export
controle_fim <- function(nome_script, hist_id, sucesso, etapa = "coleta",
                         linhas = NA_integer_, mensagem = "",
                         hash_estado = NA_character_,
                         por = Sys.info()[["user"]]) {
  con <- controle_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "UPDATE controle_execucao_historico
        SET fim = now(), sucesso = $2, mensagem = $3, linhas = $4
      WHERE id = $1", params = list(hist_id, sucesso, mensagem, linhas))
  DBI::dbExecute(con,
    "INSERT INTO controle_execucao (nome_script, etapa, ultima_atualizacao,
                                    ultima_verificacao, primeira_carga, status,
                                    linhas_ultima_carga, detalhe, hash_estado,
                                    atualizado_por)
     VALUES ($1, $2, now(), now(), now(), $3, $4, $5, $6, $7)
     ON CONFLICT (nome_script) DO UPDATE SET
       etapa = EXCLUDED.etapa,
       ultima_atualizacao = now(),
       ultima_verificacao = now(),
       status = EXCLUDED.status,
       linhas_ultima_carga = EXCLUDED.linhas_ultima_carga,
       detalhe = EXCLUDED.detalhe,
       hash_estado = EXCLUDED.hash_estado,
       atualizado_por = EXCLUDED.atualizado_por",
    params = list(nome_script, etapa,
                  ifelse(sucesso, "ok", "erro"), linhas, mensagem,
                  hash_estado, por))
  invisible(TRUE)
}

#' Fingerprint do conteudo processado por um script de coleta: md5 do CSV
#' em cache quando existe (convencao coleta/cache/<script>/<script>.csv)
#' @keywords internal
hash_coleta_csv <- function(nome_script, raiz) {
  f <- file.path(raiz, "coleta", "cache", nome_script,
                 paste0(nome_script, ".csv"))
  if (!file.exists(f)) return(NA_character_)
  digest::digest(f, algo = "md5", file = TRUE)
}

#' C3: verifica, por orig_name do mdata, se o indicador ja esta atualizado no DW
#'
#' Compara o max(refdate) gravado em data_values para cada orig_name com a
#' referencia esperada (refdate_esperada). Retorna data.frame com
#' necessidade de atualizacao por orig_name.
#'
#' @param orig_names vetor de orig_names (mdata); NULL = todos
#' @param refdate_esperada data de referencia que se deseja ter carregada
#'   (default: hoje). Um indicador precisa atualizar quando
#'   max(refdate) < refdate_esperada.
#' @param con conexao aberta opcional
#' @export
verificar_necessidade_atualizacao <- function(orig_names = NULL,
                                              refdate_esperada = Sys.Date(),
                                              con = NULL) {
  if (is.null(con)) { con <- controle_con(); on.exit(DBI::dbDisconnect(con)) }
  sql <- "SELECT m.orig_name, max(d.refdate) AS max_refdate, count(*) AS n
            FROM data_values d JOIN mdata m ON d.mdata_id = m.mdata_id"
  if (!is.null(orig_names))
    sql <- paste0(sql, " WHERE m.orig_name = ANY($1)")
  sql <- paste0(sql, " GROUP BY m.orig_name")
  atual <- if (is.null(orig_names)) DBI::dbGetQuery(con, sql) else
    DBI::dbGetQuery(con, sql, params = list(orig_names))
  if (!nrow(atual)) return(data.frame())
  atual$necessita_atualizacao <-
    is.na(atual$max_refdate) | (atual$max_refdate < refdate_esperada)
  atual[order(!atual$necessita_atualizacao, atual$orig_name), ]
}
