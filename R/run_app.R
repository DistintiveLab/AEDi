#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function() {
  if(!file.exists("dashboard_db.sqlite")) {
    prepare_db()
  }
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
