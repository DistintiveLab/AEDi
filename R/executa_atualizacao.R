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

#' Executa um unico script de coleta com controle de execucao
executar_script_coleta <- function(arquivo, raiz = .aedi_raiz()) {
  nome <- sub("\\.R$", "", arquivo, ignore.case = TRUE)
  flog.info(log_messages$script_inicio, nome)
  hist_id <- AEDi:::controle_inicio(nome)
  t0 <- Sys.time()
  res <- tryCatch({
    env <- new.env(parent = globalenv())
    sys.source(file.path(raiz, "coleta", arquivo), envir = env,
               toplevel.env = env)
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
                      linhas = res$nlin)
  invisible(res$ok)
}

#' Roda todos (ou os indicados) scripts de coleta
#'
#' @param apenas vetor de nomes (sem .R) para restringir; default todos
#' @export
atualizar_indicadores <- function(apenas = NULL) {
  flog.info(log_messages$inicio)
  AEDi:::controle_preparar()
  arqs <- listar_scripts_coleta()
  if (!is.null(apenas)) arqs <- arqs[sub("\\.R$", "", arqs, ignore.case = TRUE) %in% apenas]
  if (!length(arqs)) { flog.warn(log_messages$nenhum, file.path(.aedi_raiz(), "coleta")); return(invisible(FALSE)) }
  resultados <- setNames(logical(length(arqs)), sub("\\.R$", "", arqs, ignore.case = TRUE))
  for (a in arqs) resultados[[sub("\\.R$", "", a, ignore.case = TRUE)]] <- executar_script_coleta(a)
  flog.info(log_messages$fim)
  invisible(resultados)
}
