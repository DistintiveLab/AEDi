# Blocos de UI do painel (recursos, topbar, abas, rodape) ------------------
#
# Funcoes pequenas e sem acoplamento ao resto do pacote para que a UI do
# painel seja componivel: panel_app() monta a app launcher com elas e
# deploy_panel(esqueleto = TRUE) as copia para o esqueleto do projeto,
# onde viram base de adaptacao (soltar/adicionar abas, trocar rodape etc).

#' Resolve o src de uma marca (logo) do painel (funciona fora do app)
#'
#' Procura o arquivo informado no diretorio de assets — no esqueleto gerado
#' por [deploy_panel()] e o proprio `www/` local — com fallback para o `www/`
#' embutido no pacote (inst/app/www) via resource path; URLs http(s) passam
#' direto. [painel_logo_src()] e o caso particular da marca do topbar
#' (variavel `aedi_logo`); a aba Sobre reutiliza para logos de apoio.
#'
#' @param logo nome do arquivo da marca (ou URL http/https)
#' @param assets_dir diretorio de assets do painel; default tenta "www"
#'   no diretorio corrente e cai para o embutido no pacote
#' @keywords internal
painel_marca_src <- function(logo, assets_dir = NULL) {
  if (grepl("^https?://", logo)) return(logo)
  candidatos <- if (is.null(assets_dir))
    c("www", system.file("app", "www", package = "AEDi"))
  else assets_dir
  for (d in candidatos[nzchar(candidatos)]) {
    if (file.exists(file.path(d, basename(logo)))) {
      shiny::addResourcePath("aedi_marca", normalizePath(d))
      return(file.path("aedi_marca", basename(logo)))
    }
  }
  if (file.exists(logo)) {
    shiny::addResourcePath("aedi_marca", dirname(normalizePath(logo)))
    return(file.path("aedi_marca", basename(logo)))
  }
  d <- if (is.null(assets_dir))
    system.file("app", "www", package = "AEDi") else assets_dir
  shiny::addResourcePath("aedi_marca", d)
  file.path("aedi_marca", basename(logo))
}

#' Resolve o src do logo para o painel (variavel `aedi_logo`)
#' @inheritParams painel_marca_src
#' @keywords internal
painel_logo_src <- function(assets_dir = NULL) {
  painel_marca_src(Sys.getenv("aedi_logo", "aedi-Wide.png"), assets_dir)
}

#' Opcoes das caixas de selecao do painel: selectize com pesquisa por parte
#' do nome e sem truncar listas longas
#'
#' O selectize.js renderiza no maximo `max_options` itens (default 1000):
#' sem elevar o limite, a lista de localidades (5,7 mil municipios no DW)
#' termina antes do fim e o resto so aparece para quem digita o nome.
#'
#' @param placeholder texto de ajuda dentro da caixa
#' @param max_options numero maximo de itens renderizados de uma vez
#' @keywords internal
painel_opcoes_select <- function(placeholder, max_options = 10000L) {
  list(placeholder = placeholder, maxOptions = max_options)
}

#' Recursos de cabecalho (meta, CSS/JS do painel, Gov.br) e div raiz da paleta
#' @keywords internal
painel_recursos <- function(assets_dir, paleta = c("govbr", "pb")) {
  paleta <- match.arg(paleta)
  # publica o diretorio de assets por URL: o globo busca o contorno
  # mundial (painel-mundo.geojson) em painel_recursos/...
  shiny::addResourcePath("painel_recursos",
                         normalizePath(assets_dir, mustWork = FALSE))
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport",
                       content = "width=device-width, initial-scale = 1")),
    htmltools::includeCSS(file.path(assets_dir, "painel.css")),
    htmltools::includeScript(file.path(assets_dir, "painel.js")),
    lapply(c("painel-geo.js", "painel-map.js", "painel-map-controls.js",
             "painel-globe.js"),
           \(arq) htmltools::includeScript(file.path(assets_dir, arq))),
    shinyGovBRstyle::use_govbr(),
    shiny::tags$div(id = "painel_raiz", `data-paleta` = paleta,
                    class = "hidden"))
}

#' Topbar do painel com marca e botao de troca de paleta
#' @keywords internal
painel_topbar <- function(titulo,
                          subtitulo = "AEDi — banco de dados do painel",
                          logo_src = painel_logo_src()) {
  shiny::tags$header(class = "painel-topbar",
    shiny::tags$a(class = "painel-brand", href = "#",
           `aria-label` = paste(titulo, "— início"),
           shiny::tags$img(src = logo_src, alt = "Logotipo AEDi"),
           shiny::tags$span(
             shiny::tags$span(class = "painel-brand-name", titulo),
             shiny::tags$small(subtitulo))),
    shiny::tags$div(class = "painel-topbar-acoes",
      shiny::tags$button(id = "painel_paleta_btn", type = "button",
                  class = "painel-paleta-btn", `aria-pressed` = "true",
                  "Preto e branco")))
}

#' Abas do painel (uma tabPanel por modulo) — solte/adicione a vontade
#' @keywords internal
painel_abas <- function() {
  list(
    shiny::tabPanel("Região", value = "regiao",
                    mod_panel_regiao_ui("panel_regiao_1")),
    shiny::tabPanel("Mapa", value = "mapa",
                    mod_panel_map_ui("panel_map_1")),
    shiny::tabPanel("Baixar", value = "baixar",
                    mod_panel_baixar_ui("panel_baixar_1")),
    shiny::tabPanel("Sobre", value = "sobre",
                    panel_sobre_ui()))
}

#' Aviso de carregamento (CSS mostra/esconde conforme a conexao)
#' @keywords internal
painel_aviso_carregando <- function() {
  shiny::tags$div(class = "painel-busy", role = "status",
                  `aria-live` = "polite", "Carregando…")
}

#' Rodape do painel; contato (tag) substitui a linha de credito padrao
#' @keywords internal
painel_rodape <- function(contato = NULL) {
  credito <- if (!is.null(contato)) contato else
    shiny::tags$span("Desenvolvido por Rodrigo E. S. Borges · ",
      shiny::tags$a(href = "https://www.distintive.com.br",
                    target = "_blank", rel = "noopener", "Distintive"))
  shiny::tags$footer(class = "painel-rodape",
    shiny::tags$span("Dados: banco de dados do painel (aedidb)."),
    credito)
}
