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
#   painel_apoio      linha de apoio institucional da aba Sobre, campos
#                     "texto antes|nome|url|texto depois" separados por
#                     "|" (nome vira link da url); sem "|" a linha inteira
#                     vira texto puro; vazio mantem o credito a Distintive
#   painel_equipe     cartoes da equipe ("Quem faz") da aba Sobre, entradas
#                     separadas por ";" e campos "nome|papel|email" por
#                     "|" (email opcional); vazio mantem o cartao do autor
#   painel_apoios     boxes de apoio da aba Sobre, entradas separadas por
#                     ";" e campos "logo|url|frase|nome" por "|" (logo =
#                     arquivo do www/ ou URL; nome opcional, default do
#                     dominio da url); vazio mantem o box padrao Distintive
#                     (com a frase de painel_apoio)

#' Titulo do painel (usa painel_titulo)
#' @keywords internal
painel_brand_titulo <- function(default = "Painel de Indicadores") {
  Sys.getenv("painel_titulo", default)
}

#' Subtitulo do painel (usa painel_subtitulo)
#' @keywords internal
painel_brand_subtitulo <- function(default = "AEDi — banco de dados do painel") {
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

#' Linha de apoio institucional da aba Sobre (usa painel_apoio): campos
#' "texto antes|nome|url|texto depois" separados por "|" — o nome vira
#' link da url e os textos de fora aparecem como estao escritos; sem
#' "|" a linha inteira vira um paragrafo de texto puro
#' @keywords internal
painel_brand_apoio <- function(
  default = paste("Este painel contou com apoio material e financeiro de ",
                  "|Distintive|https://www.distintive.com.br|.", sep = "")) {
  valor <- trimws(Sys.getenv("painel_apoio", ""))
  if (!nzchar(valor)) valor <- default
  campos <- strsplit(valor, "|", fixed = TRUE)[[1]]
  if (length(campos) < 2L) return(shiny::tags$p(valor))
  length(campos) <- 4
  campos[is.na(campos)] <- ""
  nome <- trimws(campos[2])
  url <- trimws(campos[3])
  shiny::tags$p(
    campos[1],
    if (nzchar(nome)) {
      if (nzchar(url))
        shiny::tags$a(nome, href = url,
                      target = "_blank", rel = "noopener")
      else nome
    },
    campos[4])
}

#' Cartoes da equipe da aba Sobre (usa painel_equipe): entradas separadas
#' por ";", campos "nome|papel|email" por "|" (email opcional); vazio
#' mantem o cartao unico do autor
#' @keywords internal
painel_brand_equipe <- function(
  default = paste("Rodrigo Emmanuel Santana Borges|",
                  "Desenvolvedor e cientista de dados|",
                  "rodrigo@borges.net.br", sep = "")) {
  valor <- trimws(Sys.getenv("painel_equipe", ""))
  if (!nzchar(valor)) valor <- default
  entradas <- trimws(strsplit(valor, ";", fixed = TRUE)[[1]])
  lapply(entradas[nzchar(entradas)], function(entrada) {
    campos <- strsplit(entrada, "|", fixed = TRUE)[[1]]
    length(campos) <- 3
    campos[is.na(campos)] <- ""
    campos <- trimws(campos)
    shiny::tags$div(class = "painel-pessoa-card",
      shiny::tags$h3(campos[1]),
      if (nzchar(campos[2]))
        shiny::tags$p(class = "painel-pessoa-papel", campos[2]),
      if (nzchar(campos[3]))
        shiny::tags$p(shiny::tags$strong("Contato: "),
          shiny::tags$a(href = paste0("mailto:", campos[3]), campos[3])))
  })
}

#' Um box de apoio da aba Sobre: logo clicavel ao lado da frase
#' @keywords internal
painel_brand_apoio_box <- function(logo, url, frase, nome) {
  src <- if (nzchar(logo)) painel_marca_src(logo) else painel_logo_src()
  dominio <- gsub("^https?://(www\\.)?", "", url)
  if (!nzchar(nome)) nome <- dominio
  rotulo <- if (nzchar(dominio) && !identical(nome, dominio))
    paste0(nome, " (", dominio, ")") else nome
  shiny::tags$div(class = "painel-apoio",
    shiny::tags$a(class = "painel-apoio-logo", href = url,
      target = "_blank", rel = "noopener", `aria-label` = rotulo,
      shiny::tags$img(src = src, alt = paste("Logotipo da", nome))),
    shiny::tags$div(class = "painel-apoio-texto", frase))
}

#' Boxes de apoio da aba Sobre (usa painel_apoios): entradas separadas por
#' ";", campos "logo|url|frase|nome" por "|" (nome opcional, default do
#' dominio da url); vazio mantem o box padrao Distintive, cuja frase segue
#' a variavel painel_apoio
#' @keywords internal
painel_brand_apoios <- function() {
  valor <- trimws(Sys.getenv("painel_apoios", ""))
  if (!nzchar(valor)) {
    return(list(painel_brand_apoio_box(
      logo = "", url = "https://www.distintive.com.br",
      frase = painel_brand_apoio(), nome = "Distintive")))
  }
  entradas <- trimws(strsplit(valor, ";", fixed = TRUE)[[1]])
  lapply(entradas[nzchar(entradas)], function(entrada) {
    campos <- strsplit(entrada, "|", fixed = TRUE)[[1]]
    length(campos) <- 4
    campos[is.na(campos)] <- ""
    campos <- trimws(campos)
    painel_brand_apoio_box(logo = campos[1], url = campos[2],
      frase = shiny::tags$p(campos[3]), nome = campos[4])
  })
}
