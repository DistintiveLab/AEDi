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
  par_matriz <- mod_explorar_matriz_server("explorar_matriz")
  par_pred <- mod_explorar_preditores_server("explorar_pred")
  ultimo_par <- shiny::reactiveVal(NULL)
  shiny::observeEvent(par_matriz(), ultimo_par(par_matriz()))
  shiny::observeEvent(par_pred(), ultimo_par(par_pred()))
  shiny::observeEvent(ultimo_par(), {
    shiny::updateTabItems(session, "sidebar_menus", selected = "bivariate")
  })
  mod_explorar_bivar_server("explorar_bivar", par_inicial = ultimo_par)
  shiny::callModule(header_buttons, "header")
}
