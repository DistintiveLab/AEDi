#' Body UI
#'
#' @return HTML for app body
#' @export
#' @importFrom shiny fluidRow column
#' @importFrom shinydashboard dashboardBody tabItems tabItem box
#' @importFrom shinyWidgets radioGroupButtons
body_ui <- function() {

  shinydashboard::dashboardBody(

    shinydashboard::tabItems(

      shinydashboard::tabItem(
        tabName = "upload_data",
        # darkmode::with_darkmode(),
        upload_data_ui("data"),
        # upload_files_ui("data")

      )
    )
  )
}
