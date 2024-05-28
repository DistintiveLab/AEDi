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
    contact_item("Rodrigo Franklin", "Coordenador", "XXX-XXX-XXXX", "labcidades@ufes.br"),
    contact_item("Lauriete Caneva","Coordenadora", "XXX-XXX-XXXX","email"),
    contact_item("Rodrigo Borges", "Dev./Cientista de Dados", "XXX-XXX-XXX", "rodrigo@borges.net.br")
  )

  shinydashboardPlus::dashboardHeader(
    title = shiny::tags$img(
      src = "www/OW-Wide.png",
      width = 120
    ),
    #enable_controlbar = TRUE,
    controlbarIcon = "dashboard",
    # left_menu = header_left_menu_ui(),
    # fixed = TRUE,
    .list = header_buttons_ui("header", contacts = contacts)

  )
}
