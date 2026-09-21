# Cache de dois niveis (memoria do processo + RDS em disco) para as leituras
# do DW remoto: o handshake e lento (~4s) e as consultas agregadas levam
# segundos, mas os dados raramente mudam entre ETLs. O cache em disco
# sobrevive a reinicios do processo (Run App, redeploys no Connect) e o de
# memoria e compartilhado por todas as sessoes Shiny do mesmo processo.
# As chaves incluem host+dbname para nao misturar caches do DW local e do
# remoto. Desativar com painel_sem_cache=1 no ambiente.

.painel_cache_env <- new.env(parent = emptyenv())

#' Diretorio do cache em disco, relativo ao diretorio do app (como www/)
#' @keywords internal
painel_cache_dir <- function() "cache"

#' Chave de cache com host+dbname sanitizados na frente
#' @keywords internal
painel_cache_chave <- function(sufixo) {
  alvo <- paste(Sys.getenv("host", "127.0.0.1"),
                Sys.getenv("dbname", "aedidb"), sep = "_")
  alvo <- gsub("[^A-Za-z0-9_.-]", "_", alvo)
  paste0(alvo, "__", sufixo)
}

#' Busca no cache de dois niveis: memoria -> disco -> gerar. Grava nos dois
#' niveis. ttl_horas controla a validade; entradas vencidas regeram.
#' @keywords internal
painel_cache_get <- function(chave, ttl_horas, gerar) {
  sem_cache <- identical(Sys.getenv("painel_sem_cache"), "1")
  limite <- Sys.time() - ttl_horas * 3600
  arquivo <- file.path(painel_cache_dir(), paste0(chave, ".rds"))
  if (!sem_cache && !is.null(.painel_cache_env[[chave]]) &&
      .painel_cache_env[[chave]]$momento >= limite) {
    return(.painel_cache_env[[chave]]$objeto)
  }
  if (!sem_cache && file.exists(arquivo)) {
    entrada <- tryCatch(readRDS(arquivo), error = function(e) NULL)
    if (!is.null(entrada) && !is.null(entrada$momento) &&
        entrada$momento >= limite) {
      .painel_cache_env[[chave]] <- entrada
      return(entrada$objeto)
    }
  }
  objeto <- gerar()
  if (!sem_cache) {
    entrada <- list(objeto = objeto, momento = Sys.time())
    dir.create(painel_cache_dir(), recursive = TRUE, showWarnings = FALSE)
    tryCatch(saveRDS(entrada, arquivo), error = function(e) NULL)
    .painel_cache_env[[chave]] <- entrada
  }
  objeto
}

#' Descarta o cache de memoria e de disco (rodar apos atualizar o DW com ETL)
#' @keywords internal
painel_cache_limpar <- function() {
  rm(list = ls(.painel_cache_env, all.names = TRUE), envir = .painel_cache_env)
  unlink(file.path(painel_cache_dir(), "*.rds"))
  invisible(TRUE)
}
