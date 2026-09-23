#' Materializa o painel admin do projeto (launcher)
#'
#' Companheira de [admin_app()]: grava na raiz do projeto chamador uma
#' launcher Shiny (`<diretorio>/app.R`) que monta o painel admin somente
#' leitura a partir do pacote AEDi instalado. Launcher fino por design:
#' correcoes e novas abas chegam com o reinstall do AEDi, sem copias
#' derivadas para divergirem.
#'
#' A app le o banco aedidb com as mesmas credenciais do orquestrador
#' (variaveis de ambiente `user`, `password`, `host`, `dbname`) e so faz
#' sentido em localhost/LAN. NAO publicar em host acessivel externamente
#' sem autenticacao na frente (shinymanager/shinyproxy/VPN): a app expoe
#' internals do controle e depende de acesso direto ao banco.
#'
#' @param diretorio caminho do diretorio da app, relativo ao projeto
#'   (default "admin")
#' @param raiz raiz do projeto orquestrado (default: diretorio corrente);
#'   usada apenas para nomear o projeto no README
#' @param sobrescrever substitui um app.R ja existente (default FALSE)
#'
#' @return caminho absoluto do diretorio da app (invisivel)
#' @export
deploy_admin <- function(diretorio = "admin", raiz = NULL,
                         sobrescrever = FALSE) {
  if (is.null(raiz)) raiz <- getwd()
  projeto <- .nome_projeto(raiz)
  versao <- as.character(utils::packageVersion("AEDi"))
  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  app_r <- file.path(diretorio, "app.R")
  readme <- file.path(diretorio, "README.md")
  if (file.exists(app_r) && !isTRUE(sobrescrever))
    stop("app.R ja existe em ", normalizePath(diretorio),
         " - use sobrescrever = TRUE para substituir", call. = FALSE)
  writeLines(c(
    paste0("# Painel admin (somente leitura) do lote ", projeto,
           " - gerada por AEDi::deploy_admin()"),
    paste0("# AEDi ", versao, " - ", format(Sys.Date(), "%Y-%m-%d"),
           " - atualizacoes chegam com o reinstall do pacote"),
    "# Credenciais do aedidb (variaveis de ambiente): user, password, host, dbname",
    'AEDi::admin_app(raiz = "..")',
    ""), app_r, useBytes = TRUE)
  writeLines(c(
    "# Painel admin do lote",
    "",
    paste0("Monitoramento somente leitura do orquestrador do projeto **",
           projeto, "**: status de cada script de coleta (com o motivo dos"),
    "pulos, inclusive dependencias), grafo de dependencias declarado, frescor",
    "das series no banco e historico das execucoes. Gerada por",
    "`AEDi::deploy_admin()` como launcher sobre o pacote AEDi.",
    "",
    "## Rodar localmente",
    "",
    "```r",
    'shiny::runApp("admin")',
    "```",
    "",
    "Requisitos: pacote AEDi instalado e banco aedidb alcancavel com as",
    "mesmas credenciais do orquestrador (variaveis de ambiente `user`,",
    "`password`, `host`, `dbname`).",
    "",
    "## Seguranca",
    "",
    "NAO publicar esta app em host acessivel externamente sem autenticacao",
    "na frente (shinymanager, shinyproxy ou VPN): ela expoe internals do",
    "controle e exige acesso direto ao banco de dados.",
    ""), readme, useBytes = TRUE)
  tryCatch(parse(app_r),
           error = function(e) stop("app.R nao parseia: ",
                                    conditionMessage(e), call. = FALSE))
  cat(sprintf(
    "Launcher do painel admin criada em %s (AEDi %s) - rode com shiny::runApp(\"%s\").\n",
    normalizePath(diretorio), versao, diretorio))
  invisible(normalizePath(diretorio))
}
