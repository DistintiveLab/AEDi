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
#                     separadas por ";" e campos "nome|funcao|telefone|email"
#                     separados por "|". Default: Rodrigo Borges e Distintive
#   aedi_organizacao  nome da organizacao exibido no rodape da sidebar.
#                     Default: Distintive

#' Resolve o src do logo conforme aedi_logo (arquivo, URL ou www/)
#' @keywords internal
resolver_logo_src <- function() {
  logo <- Sys.getenv("aedi_logo", "www/aedi-Wide.png")
  if (grepl("^https?://", logo)) return(logo)
  if (file.exists(logo)) {
    dir <- shiny::addResourcePath("aedi_marca", dirname(normalizePath(logo)))
    return(file.path("aedi_marca", basename(logo)))
  }
  logo  # caminho relativo a www/ (com ou sem prefixo www/)
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
    "Rodrigo Borges|Dev./Cientista de Dados|XXX-XXX-XXX|rodrigo@borges.net.br;",
    "Distintive|Inteligencia para políticas publicas|61-XXXX-XXXX|apps@distintive.com.br")
  entradas <- trimws(strsplit(Sys.getenv("aedi_contatos", padrao), ";", fixed=TRUE)[[1]])
  lapply(entradas[nzchar(entradas)], \(entrada) {
    campos <- strsplit(entrada, "|", fixed=TRUE)[[1]]
    length(campos) <- 4
    campos[is.na(campos)] <- ""
    do.call(contact_item, as.list(campos))
  })
}
