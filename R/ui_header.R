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
    contact_item("Rodrigo Borges", "Developer", "XXX-XXX-XXX", "rodrigo@borges.net.br"),
    contact_item("Rodrigo Franklin", "Project Manager", "XXX-XXX-XXXX", "labcidades@ufes.br")
  )

  shinydashboardPlus::dashboardHeader(
    title = shiny::tags$img(
      src = "www/ow_logo_new.png",
      width = 200
    ),
    #enable_controlbar = TRUE,
    controlbarIcon = "dashboard",
    # left_menu = header_left_menu_ui(),
    # fixed = TRUE,
    .list = header_buttons_ui("header", contacts = contacts)

  )
}
