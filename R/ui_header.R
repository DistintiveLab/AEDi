#' Header UI
#'
#' \lifecycle{experimental}
#'
#' @return HTML for app header
#' @export
#' @importFrom shinydashboardPlus dashboardHeader
header_ui <- function() {

  # contacts
  contacts <- c(
    contact_item("Rodrigo Borges", "Dev./Cientista de Dados", "XXX-XXX-XXX", "rodrigo@borges.net.br"),
    contact_item("Distintive", "Inteligencia para políticas publicas", "61-XXXX-XXXX", "apps@distintive.com.br")
  )

  shinydashboardPlus::dashboardHeader(
    title = shiny::tags$img(
      src = "www/aedi-Wide.png",
      width = 120
    ),
    #enable_controlbar = TRUE,
    controlbarIcon = "dashboard",
    # left_menu = header_left_menu_ui(),
    # fixed = TRUE,
    .list = header_buttons_ui("header", contacts = contacts)

  )
}
