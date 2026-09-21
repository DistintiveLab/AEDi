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

#' Nome do projeto dono do lote de coleta: basename da raiz
#' (ex.: "AEDi", "pndr_dashboard") - chave `projeto` do controle de execucao
#' @keywords internal
.nome_projeto <- function(raiz) basename(normalizePath(raiz, mustWork = FALSE))

#' Lista os scripts de coleta executaveis (sem .ignore)
listar_scripts_coleta <- function(raiz = .aedi_raiz()) {
  dir_coleta <- file.path(raiz, "coleta")
  arqs <- list.files(dir_coleta, pattern = "\\.R$", ignore.case = TRUE)
  ok <- !file.exists(file.path(dir_coleta, paste0(arqs, ".ignore")))
  arqs[ok]
}

# Pacotes cujas funcoes costumam ser chamadas SEM namespace nos scripts
# de coleta antigos (ex.: dbGetQuery em citec1_aedi). "AEDi" abre para os
# proprios exports (gravar_serie_dw/db_datawrite chamados bare em scripts
# A5b). Deteccao por export, nessa ordem (DBI antes dos drivers que
# reexportam)
.pacotes_candidatos <- c("AEDi", "DBI", "RPostgres", "RPostgreSQL", "dplyr",
                         "data.table", "readr", "readxl", "tidyr",
                         "lubridate", "stringr", "purrr", "sf", "digest",
                         "edubr", "sidra", "httr", "jsonlite")

#' Carrega o .Renviron da raiz do projeto no processo atual
#'
#' O painel dispara a atualizacao em subprocesso (callr::r_bg), que le
#' apenas o ~/.Renviron do usuario; o .Renviron da raiz so e lido quando
#' o R inicia com cwd na raiz. Sem isso, credenciais como as do banco
#' RAIS ficam ausentes no subprocesso mesmo existindo no arquivo.
#' @keywords internal
.carregar_renviron <- function(raiz) {
  f <- file.path(raiz, ".Renviron")
  if (!file.exists(f)) return(invisible(FALSE))
  tryCatch(readRenviron(f),
           error = function(e) warning(sprintf(
             ".Renviron da raiz inacessivel: %s", conditionMessage(e))))
  invisible(TRUE)
}

#' Nomes de funcao chamados sem namespace no script (so o parse)
#' @keywords internal
.heads_bare <- function(exprs) {
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is.symbol(e[[1]])) out <<- c(out, deparse(e[[1]]))
    # argumentos vazios (ex.: df[i, ]) geram o objeto "missing", que erroa de
    # forma nao capturavel ao ser forcado no laco; cada ramo fica protegido
    for (a in as.list(e)[-1])
      tryCatch(
        if (is.function(a)) walk(body(a)) else if (is.call(a)) walk(a),
        error = function(err) NULL)
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
#' quando as env vars do banco RAIS estao definidas (dbname em dbrais ou,
#' na convencao dos scripts de coleta, em mte_rais) e locgeoloc do
#' cadastro de locais. Scripts que criam os proprios objetos
#' (if (!exists(...))) reutilizam os fornecidos aqui. Retorna
#' list(abertas, pendentes): conexoes abertas aqui (para desconectar ao
#' final) e objetos NAO fornecidos com o motivo, para anexar ao erro do
#' script em vez de um generico "object not found".
#' @keywords internal
.prover_objetos_sessao <- function(env) {
  abertas <- list()
  pendentes <- character()
  # scripts escritos para Rscript standalone leem argumentos proprios via
  # commandArgs(TRUE) (ex.: ano-alvo em gastos_tributarios_municipio.R).
  # Dentro do lote, commandArgs() devolveria os args do processo do
  # orquestrador (ja visto: indice de batch lido como "ano 4"). Mascara:
  # o script roda como se sem argumentos; args por script, quando
  # necessarios, vem da env var AEDI_SCRIPT_ARGS (separados por espaco).
  if (!exists("commandArgs", envir = env, inherits = FALSE))
    assign("commandArgs", function(trailingOnly = FALSE) {
      if (!trailingOnly) return(character(0))
      a <- strsplit(Sys.getenv("AEDI_SCRIPT_ARGS", ""), " +")[[1]]
      a[nzchar(a)]
    }, envir = env)
  # helper A5b historico: definido "na sessao" por scripts que hoje estao
  # fora do lote (empregoformal_agricola_nacional.R, .ignore) e usado sem
  # definicao por variantes de populacao e indicadores_agregado_uf
  if (!exists("somasna", envir = env, inherits = FALSE))
    assign("somasna", function(x) sum(x, na.rm = TRUE), envir = env)
  falta <- function(obj, motivo) {
    pendentes <<- c(pendentes, setNames(motivo, obj))
    warning(sprintf("objeto de sessao '%s' nao fornecido: %s", obj, motivo))
    invisible()
  }
  if (!exists("con", envir = env, inherits = FALSE)) {
    con <- tryCatch(AEDi:::controle_con(), error = function(e) {
      falta("con", paste("conexao com o DW indisponivel -", conditionMessage(e)))
      NULL
    })
    if (!is.null(con)) {
      assign("con", con, envir = env)
      abertas <- c(abertas, list(con))
    }
  }
  if (exists("con", envir = env, inherits = FALSE)) {
    if (!exists("mdr", envir = env, inherits = FALSE))
      assign("mdr", env$con, envir = env)
    if (!exists("locgeoloc", envir = env, inherits = FALSE)) {
      lgl <- tryCatch(DBI::dbGetQuery(env$con,
                                      "select local_id, local_name, geoloc_id from local"),
                      error = function(e) NULL)
      if (!is.null(lgl)) assign("locgeoloc", lgl, envir = env)
      else falta("locgeoloc", "cadastro de locais indisponivel (DW inacessivel)")
    }
  } else if (!exists("mdr", envir = env, inherits = FALSE))
    falta("mdr", "sem conexao com o DW")
  vars_rais <- c("mte_rais", "pwdrais", "hostraispsql")
  if (!exists("rais", envir = env, inherits = FALSE)) {
    if (!all(nzchar(Sys.getenv(vars_rais)))) {
      falta("rais", sprintf("vars %s ausentes no ambiente (confira o .Renviron)",
                             paste(vars_rais, collapse = "/")))
    } else {
      db_rais <- Sys.getenv("dbrais", Sys.getenv("mte_rais"))
      rais <- tryCatch(DBI::dbConnect(RPostgres::Postgres(),
                                      dbname = db_rais,
                                      user = Sys.getenv("mte_rais"),
                                      password = Sys.getenv("pwdrais"),
                                      host = Sys.getenv("hostraispsql")),
                       error = function(e) {
                         falta("rais", paste("conexao com o banco RAIS indisponivel -",
                                             conditionMessage(e)))
                         NULL
                       })
      if (!is.null(rais)) {
        assign("rais", rais, envir = env)
        abertas <- c(abertas, list(rais))
      }
    }
  }
  list(abertas = abertas, pendentes = pendentes)
}

#' Source do script de coleta com a "sessao" que ele pressupoe: pacotes
#' das funcoes chamadas sem namespace (detectados do parse) e objetos
#' con/mdr/rais/locgeoloc (best-effort). Conexoes abertas aqui sao
#' fechadas ao final do script.
#' @keywords internal
.source_script_coleta <- function(arquivo, raiz, env) {
  .anexar_pacotes_script(parse(file.path(raiz, "coleta", arquivo)), env)
  sess <- .prover_objetos_sessao(env)
  on.exit(suppressWarnings(try(lapply(sess$abertas, DBI::dbDisconnect), silent = TRUE)),
          add = TRUE)
  # scripts leem/escrevem com caminhos relativos a raiz (ex. coleta/cache/…):
  # garante cwd = raiz durante a execucao mesmo com o lote disparado de
  # outro diretorio (atualizar_indicadores(raiz=))
  cwd_antigo <- setwd(raiz)
  on.exit(setwd(cwd_antigo), add = TRUE)
  tryCatch(
    sys.source(file.path(raiz, "coleta", arquivo), envir = env, toplevel.env = env),
    error = function(e) {
      if (length(sess$pendentes))
        stop(sprintf(
          "%s [objetos de sessao que o lote nao conseguiu fornecer: %s]",
          conditionMessage(e),
          paste(sprintf("%s (%s)", names(sess$pendentes), sess$pendentes),
                collapse = "; ")),
          call. = FALSE, domain = NA)
      stop(e)
    })
}

#' Cria preventivamente os diretorios de cache que o script referencia
#' (convencao coleta/cache/<nome>/…): write_csv nao cria diretorio-pai, e o
#' lote roda sobre projetos cujo cache pode ainda nao existir (ex.: clone
#' novo na VPS). A deteccao e por regex sobre o texto bruto (readLines), que
#' tolera scripts com construtos que quebram varredura da arvore de parse
#' (ex.: subscript vazio df[i, ]) e ate scripts que nao parseiam. Falha de
#' criacao vira erro claro do script (permissao).
#' @keywords internal
.preparar_cache_script <- function(arquivo, raiz) {
  linhas <- readLines(file.path(raiz, "coleta", arquivo), warn = FALSE)
  cams <- unlist(regmatches(linhas,
    gregexpr("coleta/cache/[^\"'[:space:]]+", linhas)), use.names = FALSE)
  for (cam in unique(cams)) {
    alvo <- if (grepl("^[/\\\\]", cam)) dirname(cam) else
      dirname(file.path(raiz, cam))
    if (dir.exists(alvo)) next
    dir.create(alvo, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(alvo))
      stop(sprintf(
        "nao foi possivel criar o diretorio de cache '%s' (verifique permissoes de escrita em %s)",
        alvo, raiz), call. = FALSE)
  }
  invisible(TRUE)
}

#' Executa um unico script de coleta com controle de execucao.
#' Com verificar_novidade = TRUE, consulta antes da coleta (C5, ver
#' R/verifica_fonte.R) o mais recente disponivel na fonte e, sem
#' novidades, registra execucao ok e pula o script.
executar_script_coleta <- function(arquivo, raiz = .aedi_raiz(),
                                   verificar_novidade = TRUE) {
  .carregar_renviron(raiz)
  nome <- sub("\\.R$", "", arquivo, ignore.case = TRUE)
  projeto <- .nome_projeto(raiz)
  flog.info(log_messages$script_inicio, nome)
  if (verificar_novidade) {
    nov <- tryCatch(AEDi:::verificar_novidade_fonte(nome, raiz),
                    error = function(e) list(
                      pular = FALSE,
                      motivo = paste("verificacao indisponivel:",
                                     conditionMessage(e))))
    if (isTRUE(nov$pular)) {
      hist_id <- AEDi:::controle_inicio(nome, projeto = projeto)
      AEDi:::controle_fim(nome, hist_id, TRUE, mensagem = nov$motivo,
                          linhas = NA_integer_,
                          projeto = projeto,
                          hash_estado = if (is.null(nov$assinatura))
                            NA_character_ else nov$assinatura)
      flog.info(log_messages$script_pulado, nome, nov$motivo)
      return(invisible(TRUE))
    }
  }
  hist_id <- AEDi:::controle_inicio(nome, projeto = projeto)
  t0 <- Sys.time()
  res <- tryCatch({
    .preparar_cache_script(arquivo, raiz)
    env <- new.env(parent = globalenv())
    .source_script_coleta(arquivo, raiz, env)
    nlin <- tryCatch({
      v <- AEDi:::verificar_necessidade_atualizacao(orig_names = NULL)
      NA_integer_
    }, error = function(e) NA_integer_)
    flog.info("Script concluido: %s em %.1f min", nome,
              as.numeric(difftime(Sys.time(), t0, units = "mins")))
    list(ok = TRUE, msg = "ok", nlin = nlin)
  }, error = function(e) {
    flog.error(log_messages$script_erro, nome, conditionMessage(e))
    list(ok = FALSE, msg = conditionMessage(e), nlin = NA_integer_)
  })
  # o registro de fim e best-effort: uma falha de bookkeeping (ex.: hash do
  # cache) nao pode abortar o lote inteiro - o script ja foi executado
  tryCatch(
    AEDi:::controle_fim(nome, hist_id, res$ok, mensagem = res$msg,
                        linhas = res$nlin, projeto = projeto,
                        hash_estado = AEDi:::hash_coleta_csv(nome, raiz)),
    error = function(e)
      flog.error("falha ao registrar o fim de %s no controle: %s",
                 nome, conditionMessage(e)))
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
#' @param raiz raiz do projeto dono do lote (diretório com coleta/);
#'   define o `projeto` do controle de execucao. Default: cwd com coleta/
#' @export
atualizar_indicadores <- function(apenas = NULL, dir_dump = "~/backups_aedidb",
                                   snapshot = TRUE, verificar_novidade = TRUE,
                                   raiz = .aedi_raiz()) {
  projeto <- .nome_projeto(raiz)
  .carregar_renviron(raiz)
  flog.info(log_messages$inicio)
  AEDi:::controle_preparar()
  versao <- if (snapshot) AEDi:::versao_carga_inicio(dir_dump = dir_dump,
                                                     projeto = projeto) else NA_integer_
  arqs <- listar_scripts_coleta(raiz)
  if (!is.null(apenas)) {
    # preserva a ORDEM do argumento apenas (dependencias: quem consome uma
    # serie deve rodar depois de quem a produz), em vez da alfabetica
    nomes <- sub("\\.R$", "", arqs, ignore.case = TRUE)
    ordem <- match(nomes, apenas)
    arqs <- arqs[!is.na(ordem)]
    ordem <- ordem[!is.na(ordem)]
    arqs <- arqs[order(ordem)]
  }
  if (!length(arqs)) { flog.warn(log_messages$nenhum, file.path(raiz, "coleta")); return(invisible(FALSE)) }
  resultados <- setNames(logical(length(arqs)), sub("\\.R$", "", arqs, ignore.case = TRUE))
  for (a in arqs) resultados[[sub("\\.R$", "", a, ignore.case = TRUE)]] <-
    executar_script_coleta(a, raiz = raiz, verificar_novidade = verificar_novidade)
  if (!is.na(versao))
    AEDi:::versao_carga_fim(versao, length(resultados), sum(resultados),
                            sum(!resultados))
  flog.info(log_messages$fim)
  invisible(resultados)
}
