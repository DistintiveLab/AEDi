#' App UI
#'
#' @name app_ui
#' @return tagList for app's UI
#' @importFrom shiny tagList
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom shinymath mathInput latex2r
#' @export
app_ui <- function(){

  shiny::tagList(

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
