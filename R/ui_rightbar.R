#' Right Sidebar UI
#'
#' @return HTML for app's right-sidebar
#' @export
#' @importFrom shinydashboardPlus dashboardControlbar
right_sidebar_ui <- function() {

  shinydashboardPlus::dashboardControlbar(
    skin = "dark",
    shinydashboardPlus::controlbarItem(
      "Tema",
      icon("palette"),
      shiny::tags$div(
        class = "aedi-tema-casa",
        shiny::tags$p(
          class = "help-block",
          "Alterna a paleta institucional: cores Gov.br ou preto e ",
          "branco com o roxo da Distintive. A escolha fica salva neste ",
          "navegador."),
        aedi_tema_botao("app_tema_btn")
      )
    ),
    shinydashboardPlus::controlbarItem(
      "Ajuda",
      icon("question-circle")
    )
  )

}
