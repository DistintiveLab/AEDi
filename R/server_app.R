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
  shiny::callModule(upload_data, "data")
  shiny::callModule(header_buttons, "header")
}
