# Orquestrador de atualizacao dos scripts de coleta do AEDi
# (fase C1 do roadmap_aedi_agendamento.md; adaptado do conecta_turismo
#  scripts/executa_atualizacao.R)
#
# Uso interativo:  source("R/executa_atualizacao.R") ; atualizar_indicadores()
# Uso agendado:    Rscript -e 'source("R/executa_atualizacao.R"); atualizar_indicadores()'
#   (rodar a partir da raiz do AEDi)
#
# Scripts em coleta/<nome>.R sao executados com source() em ambiente isolado;
# um arquivo coleta/<nome>.R.ignore (ou .ignore na pasta) pula o script.
# Estado de cada execucao vai para controle_execucao/_historico no aedidb
# (ver R/controle_execucao.R). Falhas nao interrompem o lote.

library(futile.logger)
suppressMessages(flog.layout(layout.format("[~t] [~l] [~f] ~m")))

log_messages <- list(
  inicio        = "Inicio da atualizacao dos indicadores do AEDi",
  fim           = "Fim da atualizacao dos indicadores do AEDi",
  script_inicio = "Executando script: %s",
  script_ok     = "Script concluido: %s (%s linhas no DW nao verificado)",
  script_erro   = "ERRO no script %s: %s",
  script_ignorado = "Script ignorado (.ignore): %s",
  script_pulado   = "Script pulado (sem novidades): %s (%s)",
  nenhum        = "Nenhum script de coleta encontrado em %s"
)

# descobre a raiz do AEDi quando o arquivo e sourcing de outro lugar
.aedi_raiz <- function() {
  cand <- getwd()
  if (file.exists(file.path(cand, "coleta"))) return(cand)
  normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."))
}
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a[1])) b else a

#' Lista os scripts de coleta executaveis (sem .ignore)
listar_scripts_coleta <- function(raiz = .aedi_raiz()) {
  dir_coleta <- file.path(raiz, "coleta")
  arqs <- list.files(dir_coleta, pattern = "\\.R$", ignore.case = TRUE)
  ok <- !file.exists(file.path(dir_coleta, paste0(arqs, ".ignore")))
  arqs[ok]
}

# Pacotes cujas funcoes costumam ser chamadas SEM namespace nos scripts
# de coleta antigos (ex.: dbGetQuery em citec1_aedi). Deteccao por
# export, nessa ordem (DBI antes dos drivers que reexportam)
.pacotes_candidatos <- c("DBI", "RPostgres", "RPostgreSQL", "dplyr",
                         "data.table", "readr", "readxl", "tidyr",
                         "lubridate", "stringr", "purrr", "sf", "digest",
                         "educabR", "sidra", "httr", "jsonlite")

#' Nomes de funcao chamados sem namespace no script (so o parse)
#' @keywords internal
.heads_bare <- function(exprs) {
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is.symbol(e[[1]])) out <<- c(out, deparse(e[[1]]))
    for (a in as.list(e)[-1])
      if (is.function(a)) walk(body(a)) else if (is.call(a)) walk(a)
    invisible()
  }
  for (e in exprs) walk(e)
  unique(out)
}

#' Scripts antigos pressupoem a sessao interativa com library(DBI) etc.;
#' no lote agendado nada esta anexado. Anexa via library() apenas os
#' pacotes cujas funcoes o script chama sem namespace e que ainda nao
#' resolvem no ambiente de execucao. Retorna o que ficou sem resolucao
#' (funcoes proprias do script sao esperadas aqui).
#' @keywords internal
.anexar_pacotes_script <- function(exprs, env) {
  resolve <- \(h) exists(h, envir = env, inherits = TRUE)
  faltam <- unique(.heads_bare(exprs))
  faltam <- faltam[!vapply(faltam, resolve, logical(1))]
  for (p in .pacotes_candidatos) {
    if (!length(faltam)) break
    ex <- tryCatch(getNamespaceExports(p), error = function(e) character())
    if (any(faltam %in% ex))
      tryCatch(suppressPackageStartupMessages(library(p, character.only = TRUE)),
               error = function(e) NULL)
    faltam <- faltam[!vapply(faltam, resolve, logical(1))]
  }
  faltam
}

#' Prove, best-effort, no ambiente do script os objetos de sessao que os
#' scripts de coleta costumam esperar (padrao A5b): con/mdr no DW, rais
#' quando as env vars do banco RAIS estao definidas e locgeoloc do
#' cadastro de locais. Scripts que criam os proprios objetos
#' (if (!exists(...))) reutilizam os fornecidos aqui. Retorna as
#' conexoes abertas aqui, para desconectar ao final do script.
#' @keywords internal
.prover_objetos_sessao <- function(env) {
  abertas <- list()
  if (!exists("con", envir = env, inherits = FALSE)) {
    con <- tryCatch(AEDi:::controle_con(), error = function(e) {
      warning(sprintf("conexao com o DW indisponivel para o script: %s",
                      conditionMessage(e)))
      NULL
    })
    if (!is.null(con)) {
      assign("con", con, envir = env)
      abertas <- c(abertas, list(con))
    }
  }
  if (!exists("mdr", envir = env, inherits = FALSE) &&
      exists("con", envir = env, inherits = FALSE))
    assign("mdr", env$con, envir = env)
  if (!exists("rais", envir = env, inherits = FALSE) &&
      all(nzchar(Sys.getenv(c("mte_rais", "pwdrais", "hostraispsql"))))) {
    rais <- tryCatch(DBI::dbConnect(RPostgres::Postgres(),
                                    dbname = Sys.getenv("mte_rais"),
                                    user = "mte_rais",
                                    password = Sys.getenv("pwdrais"),
                                    host = Sys.getenv("hostraispsql")),
                     error = function(e) {
                       warning(sprintf("conexao com o RAIS indisponivel para o script: %s",
                                       conditionMessage(e)))
                       NULL
                     })
    if (!is.null(rais)) {
      assign("rais", rais, envir = env)
      abertas <- c(abertas, list(rais))
    }
  }
  if (!exists("locgeoloc", envir = env, inherits = FALSE) &&
      exists("con", envir = env, inherits = FALSE)) {
    lgl <- tryCatch(DBI::dbGetQuery(env$con,
                                    "select local_id, local_name, geoloc_id from local"),
                    error = function(e) NULL)
    if (!is.null(lgl)) assign("locgeoloc", lgl, envir = env)
  }
  abertas
}

#' Source do script de coleta com a "sessao" que ele pressupoe: pacotes
#' das funcoes chamadas sem namespace (detectados do parse) e objetos
#' con/mdr/rais/locgeoloc (best-effort). Conexoes abertas aqui sao
#' fechadas ao final do script.
#' @keywords internal
.source_script_coleta <- function(arquivo, raiz, env) {
  .anexar_pacotes_script(parse(file.path(raiz, "coleta", arquivo)), env)
  cons <- .prover_objetos_sessao(env)
  on.exit(suppressWarnings(try(lapply(cons, DBI::dbDisconnect), silent = TRUE)),
          add = TRUE)
  sys.source(file.path(raiz, "coleta", arquivo), envir = env, toplevel.env = env)
}

#' Executa um unico script de coleta com controle de execucao.
#' Com verificar_novidade = TRUE, consulta antes da coleta (C5, ver
#' R/verifica_fonte.R) o mais recente disponivel na fonte e, sem
#' novidades, registra execucao ok e pula o script.
executar_script_coleta <- function(arquivo, raiz = .aedi_raiz(),
                                   verificar_novidade = TRUE) {
  nome <- sub("\\.R$", "", arquivo, ignore.case = TRUE)
  flog.info(log_messages$script_inicio, nome)
  if (verificar_novidade) {
    nov <- tryCatch(AEDi:::verificar_novidade_fonte(nome, raiz),
                    error = function(e) list(
                      pular = FALSE,
                      motivo = paste("verificacao indisponivel:",
                                     conditionMessage(e))))
    if (isTRUE(nov$pular)) {
      hist_id <- AEDi:::controle_inicio(nome)
      AEDi:::controle_fim(nome, hist_id, TRUE, mensagem = nov$motivo,
                          linhas = NA_integer_,
                          hash_estado = if (is.null(nov$assinatura))
                            NA_character_ else nov$assinatura)
      flog.info(log_messages$script_pulado, nome, nov$motivo)
      return(invisible(TRUE))
    }
  }
  hist_id <- AEDi:::controle_inicio(nome)
  t0 <- Sys.time()
  res <- tryCatch({
    env <- new.env(parent = globalenv())
    .source_script_coleta(arquivo, raiz, env)
    nlin <- tryCatch({
      v <- verificar_necessidade_atualizacao(orig_names = NULL)
      NA_integer_
    }, error = function(e) NA_integer_)
    flog.info("Script concluido: %s em %.1f min", nome,
              as.numeric(difftime(Sys.time(), t0, units = "mins")))
    list(ok = TRUE, msg = "ok", nlin = nlin)
  }, error = function(e) {
    flog.error(log_messages$script_erro, nome, conditionMessage(e))
    list(ok = FALSE, msg = conditionMessage(e), nlin = NA_integer_)
  })
  AEDi:::controle_fim(nome, hist_id, res$ok, mensagem = res$msg,
                      linhas = res$nlin,
                      hash_estado = hash_coleta_csv(nome, .aedi_raiz()))
  invisible(res$ok)
}

#' Roda todos (ou os indicados) scripts de coleta
#'
#' Modelo A (versões de carga): antes do lote, executa pg_dump do aedidb
#' (aedidb_v<N>_<AAAAMMDD>.dump, ver versao_carga_inicio) e registra a
#' versão com o commit git do repo. Depois do lote, fecha a versão com o
#' resumo (n ok/erro).
#'
#' @param apenas vetor de nomes (sem .R) para restringir; default todos
#' @param dir_dump diretório dos snapshots (default ~/backups_aedidb)
#' @param snapshot lógico (default TRUE); FALSE pula o pg_dump
#' @param verificar_novidade lógico (default TRUE); FALSE desativa a
#'   pré-verificação C5 e força a coleta mesmo sem novidades na fonte
#' @export
atualizar_indicadores <- function(apenas = NULL, dir_dump = "~/backups_aedidb",
                                   snapshot = TRUE, verificar_novidade = TRUE) {
  flog.info(log_messages$inicio)
  AEDi:::controle_preparar()
  versao <- if (snapshot) AEDi:::versao_carga_inicio(dir_dump = dir_dump) else NA_integer_
  arqs <- listar_scripts_coleta()
  if (!is.null(apenas)) {
    # preserva a ORDEM do argumento apenas (dependencias: quem consome uma
    # serie deve rodar depois de quem a produz), em vez da alfabetica
    nomes <- sub("\\.R$", "", arqs, ignore.case = TRUE)
    ordem <- match(nomes, apenas)
    arqs <- arqs[!is.na(ordem)]
    ordem <- ordem[!is.na(ordem)]
    arqs <- arqs[order(ordem)]
  }
  if (!length(arqs)) { flog.warn(log_messages$nenhum, file.path(.aedi_raiz(), "coleta")); return(invisible(FALSE)) }
  resultados <- setNames(logical(length(arqs)), sub("\\.R$", "", arqs, ignore.case = TRUE))
  for (a in arqs) resultados[[sub("\\.R$", "", a, ignore.case = TRUE)]] <-
    executar_script_coleta(a, verificar_novidade = verificar_novidade)
  if (!is.na(versao))
    AEDi:::versao_carga_fim(versao, length(resultados), sum(resultados),
                            sum(!resultados))
  flog.info(log_messages$fim)
  invisible(resultados)
}
