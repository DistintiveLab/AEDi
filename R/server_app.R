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
  mod_atualizacao_server("atualizacao")
  mod_explorar_dicionario_server("explorar_dic")
  mod_explorar_univar_server("explorar_uni")
  mod_explorar_dist_server("explorar_dist")
  mod_explorar_bivar_server("explorar_bivar")
  shiny::callModule(header_buttons, "header")
}
