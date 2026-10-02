# Marca configuravel do dashboard AEDi (2026-09-02)
#
# O logo (header e rodape da sidebar) e o link do rodape eram fixos da
# Distintive. Agora sao configuraveis por variaveis de ambiente (use o
# .Renviron do projeto), com fallback para o padrao original:
#
#   aedi_logo       caminho de arquivo local (servido via resource path),
#                   URL http(s), ou caminho relativo a inst/app/www/
#                   (ex.: "www/aedi_logo_new.png"). Default: www/aedi-Wide.png
#   aedi_logo_link  href do logo no rodape. Default: distintive.com.br
#   aedi_logo_width largura em px do logo do header. Default: 120
#   aedi_contatos     contatos do dropdown do header do app; entradas
#                     separadas por ";" e campos
#                     "nome|funcao|telefone|email|logo" separados por "|"
#                     (logo opcional: arquivo local, URL http(s) ou caminho
#                     relativo a inst/app/www/; vazio usa o padrao, e "none"
#                     ou "-" desliga o logo do box).
#                     Default: Rodrigo Borges e Distintive
#   aedi_contato_logo logo padrao dos boxes de contato quando a entrada nao
#                     traz o 5o campo (mesma resolucao de aedi_logo).
#                     Default: www/aedi-innovations-Square.png
#   aedi_organizacao  nome da organizacao exibido no rodape da sidebar.
#                     Default: Distintive

#' Resolve o src de uma marca (URL, arquivo local ou www/ do pacote)
#' @keywords internal
resolver_marca_src <- function(marca) {
  if (grepl("^https?://", marca)) return(marca)
  if (file.exists(marca)) {
    dir <- shiny::addResourcePath("aedi_marca", dirname(normalizePath(marca)))
    return(file.path("aedi_marca", basename(marca)))
  }
  marca  # caminho relativo a www/ (com ou sem prefixo www/)
}

#' Resolve o src do logo conforme aedi_logo (arquivo, URL ou www/)
#' @keywords internal
resolver_logo_src <- function() {
  resolver_marca_src(Sys.getenv("aedi_logo", "www/aedi-Wide.png"))
}

#' Logo de um box de contato: 5o campo da entrada, aedi_contato_logo ou o
#' padrao quadrado da marca; "none" (ou "-") desliga
#' @keywords internal
resolver_logo_contato <- function(campo = "") {
  valor <- trimws(campo)
  if (!nzchar(valor)) valor <- trimws(Sys.getenv("aedi_contato_logo", ""))
  if (!nzchar(valor)) valor <- "www/aedi-innovations-Square.png"
  if (tolower(valor) %in% c("none", "-")) return("")
  resolver_marca_src(valor)
}

#' Logo do header (usa aedi_logo e aedi_logo_width)
#' @keywords internal
logo_header_tag <- function() {
  shiny::tags$img(
    src = resolver_logo_src(),
    width = as.integer(Sys.getenv("aedi_logo_width", "120"))
  )
}

#' Logo+link do rodape da sidebar (usa aedi_logo e aedi_logo_link)
#' @keywords internal
logo_rodape_tag <- function(width = 200) {
  shiny::tags$a(
    shiny::tags$img(src = resolver_logo_src(), width = width),
    href = Sys.getenv("aedi_logo_link", "http://www.distintive.com.br")
  )
}

#' Contatos do dropdown do header (usa aedi_contatos)
#' @keywords internal
contatos_header <- function() {
  padrao <- paste0(
    "Rodrigo Borges|Dev./Cientista de Dados|XXX-XXX-XXX|rodrigo@borges.net.br|none;",
    "Distintive|Inteligencia para políticas publicas|61-XXXX-XXXX|apps@distintive.com.br")
  entradas <- trimws(strsplit(Sys.getenv("aedi_contatos", padrao), ";", fixed=TRUE)[[1]])
  lapply(entradas[nzchar(entradas)], \(entrada) {
    campos <- strsplit(entrada, "|", fixed=TRUE)[[1]]
    length(campos) <- 5
    campos[is.na(campos)] <- ""
    campos[5] <- resolver_logo_contato(campos[5])
    do.call(contact_item, as.list(campos))
  })
}
