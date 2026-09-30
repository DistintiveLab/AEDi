#' App UI
#'
#' @param tema paleta do tema gov.br/pb: `"govbr"` (azul) ou `"pb"`
#'   (preto e branco com o roxo da Distintive); default (NULL) resolve
#'   pela variavel de ambiente `aedi_paleta`. O botao de alternancia na
#'   barra lateral direita (aba Tema) troca em tempo real e persiste no
#'   navegador
#'
#' @name app_ui
#' @return tagList for app's UI
#' @importFrom shiny tagList
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom shinymath mathInput
#' @export
app_ui <- function(tema = NULL){

  tema <- if (is.null(tema)) aedi_tema_paleta_default() else
    match.arg(tema, c("govbr", "pb"))

  shiny::tagList(

    # núcleo do tema (variáveis --p-*, toggle gov.br/pb) + chrome do app
    aedi_tema_recursos(tema),
    htmltools::includeCSS(
      system.file("tema", "aedi-app.css", package = "AEDi")),

    # adding external resources
    add_external_resources(),

    # shinydashboardPagePlus with right_sidebar
    shinydashboardPlus::dashboardPage(
      header = header_ui(),
      sidebar = sidebar_ui(),
      body = body_ui(),
      controlbar = right_sidebar_ui(),
      #footer = footer_ui(),
      #title = "AEDi",
      skin = "black" ,
#       enable_preloader = TRUE,
#       loading_duration = 2
    )
  )

}


#' Add External Resources for AEDi
#'
#' function similar to golem?
#'
#' @name add_external_resources
#' @return invisible
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useSweetAlert useShinydashboardPlus
#' @importFrom shiny addResourcePath tags
add_external_resources <- function(){

  shiny::addResourcePath(
    'www', system.file('app/www', package = 'AEDi')
  )

  shiny::tags$head(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    #shinyWidgets::useShinydashboardPlus(),
    # shinyCleave::includeCleave(country = "us"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    shiny::tags$script(src = "www/custom.js")
  )
}
