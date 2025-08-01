#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function() {
  # if(!file.exists("dashboard_db.sqlite")) {
  #   prepare_db()
  # }

  basedirs <- c("coleta","manipula","documenta","visualiza")
  lapply(basedirs, \(x) {
    if(!dir.exists(paste0(x,"/cache")))
      dir.create(paste0(x,"/cache"),showWarnings = FALSE,recursive = TRUE)})
  shiny::shinyApp(ui = app_ui(), server = app_server)
}


