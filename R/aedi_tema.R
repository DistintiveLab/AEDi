# Tema gov.br / preto-e-branco compartilhado (núcleo T1) --------------------
#
# Porta o sistema de paletas do painel de indicadores (R/painel_ui.R +
# www/painel.css/js) para o pacote: variáveis --p-* das duas paletas
# (gov.br azul #1351B4; preto e branco com o roxo Distintive #78529D),
# alternância em tempo real pela classe aedi-pb no <body>, persistência
# em localStorage (chave aedi_paleta) e default institucional pela env
# aedi_paleta. Consomem o núcleo: o admin_app() (T2), o app AEDi (T3) e
# o próprio painel (T4). Sem sync server aqui por design: quem precisa
# avisar o Shiny (o painel recore gráficos) escuta o evento "aedi:paleta"
# despachado pelo aedi-tema.js.

#' Paleta inicial dos apps AEDi (usa a variável de ambiente `aedi_paleta`)
#'
#' Análogo ao [AEDi::painel_brand_paleta()] do painel: a env sobrepõe o
#' default, então o admin e o app AEDi de um projeto podem nascer pb via
#' `.Renviron` sem tocar em código. Valores: "govbr" (padrão) ou "pb".
#' @keywords internal
aedi_tema_paleta_default <- function(default = "govbr") {
  match.arg(Sys.getenv("aedi_paleta", default), c("govbr", "pb"))
}

#' Recursos do núcleo do tema: CSS + JS + base Gov.br + div raiz
#'
#' Inclui as variáveis/toggle de paleta (inst/tema), a base Rawline/Gov.br
#' do shinyGovBRstyle (servida do próprio pacote, sem CDN externo) e a div
#' raiz `#aedi_tema_raiz` com o `data-paleta` inicial lido pelo JS.
#' Colocar uma única vez por app, junto aos recursos de cabeçalho.
#' @keywords internal
aedi_tema_recursos <- function(paleta = c("govbr", "pb")) {
  paleta <- match.arg(paleta)
  tema_dir <- system.file("tema", package = "AEDi")
  if (!nzchar(tema_dir) ||
      !file.exists(file.path(tema_dir, "aedi-tema.css")))
    stop("núcleo do tema ausente nesta instalação do AEDi — reinstale",
         " o pacote", call. = FALSE)
  shiny::tagList(
    htmltools::includeCSS(file.path(tema_dir, "aedi-tema.css")),
    htmltools::includeScript(file.path(tema_dir, "aedi-tema.js")),
    shinyGovBRstyle::use_govbr(),
    shiny::tags$div(id = "aedi_tema_raiz", `data-paleta` = paleta,
                    class = "hidden"))
}

#' Botão de alternância da paleta (classe `aedi-tema-btn`)
#'
#' O aedi-tema.js resolve o clique, troca o rótulo ("Preto e branco" /
#' "Cores Gov.br") e mantém `aria-pressed`. O id é livre (evite colisões
#' com inputs Shiny: botões puros não registram input).
#' @keywords internal
aedi_tema_botao <- function(id = "aedi_tema_btn") {
  shiny::tags$button(id = id, type = "button", class = "aedi-tema-btn",
                     `aria-pressed` = "true", "Preto e branco")
}
