# Marca do painel configuravel por variaveis de ambiente (2026-09-20) ------
#
# Padrao adaptado do branding.R do rascunho do pndr_dashboard: titulo,
# subtitulo, paleta inicial e contato do rodape configuraveis sem editar
# codigo (use o .Renviron do projeto), com fallback para o padrao do AEDi.
# A variavel de ambiente SOBREPOE o default passado por argumento (assim o
# painel launcher e o esqueleto gerado por deploy_panel(esqueleto = TRUE)
# podem ser re-branded em producao sem regenerar codigo). O logo continua
# na variavel aedi_logo (ver painel_logo_src()).
#
#   painel_titulo     titulo do topbar
#   painel_subtitulo  subtitulo do topbar
#   painel_paleta     paleta inicial: "govbr" ou "pb"
#   painel_contato    contato do rodape, campos "nome|telefone|email"
#                     separados por "|"; vazio mantem o credito padrao

#' Titulo do painel (usa painel_titulo)
#' @keywords internal
painel_brand_titulo <- function(default = "Painel de Indicadores") {
  Sys.getenv("painel_titulo", default)
}

#' Subtitulo do painel (usa painel_subtitulo)
#' @keywords internal
painel_brand_subtitulo <- function(default = "AEDi — DW de indicadores") {
  Sys.getenv("painel_subtitulo", default)
}

#' Paleta inicial do painel (usa painel_paleta)
#' @keywords internal
painel_brand_paleta <- function(default = "govbr") {
  match.arg(Sys.getenv("painel_paleta", default), c("govbr", "pb"))
}

#' Linha de contato do rodape (usa painel_contato); NULL mantem o credito
#' @keywords internal
painel_brand_contato <- function() {
  valor <- trimws(Sys.getenv("painel_contato", ""))
  if (!nzchar(valor)) return(NULL)
  campos <- strsplit(valor, "|", fixed = TRUE)[[1]]
  length(campos) <- 3
  campos[is.na(campos)] <- ""
  campos <- trimws(campos)
  shiny::tags$span(
    shiny::tags$b(campos[1]),
    if (nzchar(campos[2])) shiny::tags$span(" · ", campos[2]),
    if (nzchar(campos[3]))
      shiny::tags$a(campos[3], href = paste0("mailto:", campos[3])))
}
