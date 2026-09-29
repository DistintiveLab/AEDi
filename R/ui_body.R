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

      ),

      shinydashboard::tabItem(
        tabName = "atualizacao",
        mod_atualizacao_ui("atualizacao")
      ),

      shinydashboard::tabItem(
        tabName = "data_dictionary",
        mod_explorar_dicionario_ui("explorar_dic")
      ),

      shinydashboard::tabItem(
        tabName = "univariate",
        mod_explorar_univar_ui("explorar_uni")
      ),

      shinydashboard::tabItem(
        tabName = "distributions",
        mod_explorar_dist_ui("explorar_dist")
      ),

      shinydashboard::tabItem(
        tabName = "bivariate",
        mod_explorar_bivar_ui("explorar_bivar")
      ),

      shinydashboard::tabItem(
        tabName = "cor_matrix",
        mod_explorar_matriz_ui("explorar_matriz")
      ),

      shinydashboard::tabItem(
        tabName = "predictors",
        mod_explorar_preditores_ui("explorar_pred")
      )
    )
  )
}
