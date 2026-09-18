#' Deploy basico do painel de indicadores no projeto corrente
#'
#' Materializa uma app Shiny autonoma do painel em `diretorio` do projeto
#' onde for chamada (default `painel/`), pronta para hospedar — Shiny
#' Server (aponte o site para o diretorio) ou `rsconnect::deployApp()`
#' (shinyapps.io/Posit Connect) — ou rodar localmente com
#' `shiny::runApp(diretorio)`. Diferente de [run_panel()], que apenas
#' lanca o painel na sessao corrente: aqui o projeto passa a ter a
#' propria app, que depende so do pacote AEDi instalado e das
#' credenciais do DW.
#'
#' Sao criados `app.R` (uma linha chamando [panel_app()]) e um
#' `README.md` com as instrucoes de execucao, hospedagem e credenciais.
#'
#' @param diretorio caminho do diretorio da app, relativo ao projeto
#'   (default "painel")
#' @param titulo titulo do header da app gerada
#' @param sobrescrever substitui um `app.R` existente (default FALSE)
#'
#' @return caminho absoluto do diretorio da app (invisivel)
#' @export
deploy_panel <- function(diretorio = "painel",
                         titulo = "Painel de Indicadores",
                         sobrescrever = FALSE) {
  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  app_r <- file.path(diretorio, "app.R")
  readme <- file.path(diretorio, "README.md")
  if (file.exists(app_r) && !isTRUE(sobrescrever))
    stop("app.R ja existe em ", normalizePath(diretorio),
         " — use sobrescrever = TRUE para substituir")
  writeLines(c(
    "# Painel de indicadores do DW — app gerada por AEDi::deploy_panel()",
    "# Credenciais do DW (variaveis de ambiente): user, password, host, dbname",
    sprintf("AEDi::panel_app(titulo = %s)", deparse(titulo)),
    ""), app_r)
  writeLines(c(
    "# Painel de indicadores",
    "",
    "App Shiny autonoma de consulta ao DW de indicadores (mapa municipal e",
    "series temporais), gerada por `AEDi::deploy_panel()` e montada sobre o",
    "pacote AEDi.",
    "",
    "## Rodar localmente",
    "",
    "```r",
    'shiny::runApp("painel")',
    "```",
    "",
    "## Hospedar",
    "",
    "- Shiny Server: aponte a configuracao do site para este diretorio;",
    "- shinyapps.io / Posit Connect:",
    "",
    "```r",
    'rsconnect::deployApp("painel")',
    "```",
    "",
    "## Credenciais do DW",
    "",
    "A conexao usa as variaveis de ambiente `user`, `password`, `host` e",
    "`dbname` (defaults: aedi@127.0.0.1/aedidb). Em hospedagem, configure-as",
    "no painel da plataforma ou no `.Renviron` lido pelo processo do Shiny.",
    "",
    "Requisito: pacote AEDi instalado",
    "('remotes::install_github(\"DistintiveLab/AEDi\")').",
    ""), readme)
  cat("App do painel criada em", normalizePath(diretorio), "\n")
  invisible(normalizePath(diretorio))
}
