#' Right Sidebar UI
#'
#' @return HTML for app's right-sidebar
#' @export
#' @importFrom shinydashboardPlus dashboardControlbar
right_sidebar_ui <- function() {

  shinydashboardPlus::dashboardControlbar(
    background = "dark"
  )

}
