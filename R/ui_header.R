#' Header UI
#'
#' \lifecycle{experimental}
#'
#' @return HTML for app header
#' @export
#' @importFrom shinydashboardPlus dashboardHeader
header_ui <- function() {

  shinydashboardPlus::dashboardHeader(
    title = logo_header_tag(),
    #enable_controlbar = TRUE,
    controlbarIcon = "dashboard",
    # left_menu = header_left_menu_ui(),
    # fixed = TRUE,
    .list = header_buttons_ui("header", contacts = contatos_header())

  )
}
