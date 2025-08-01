#' App Server Code
#'
#' @param input shiny server input
#' @param output shiny server output
#' @param session shiny server session
#'
#' @return server
#' @export
app_server <- function(input, output, session) {
  # List the first level callModules here
  upload_data_server("data")
  shiny::callModule(header_buttons, "header")
}
