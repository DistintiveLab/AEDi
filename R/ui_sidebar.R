#' Sidebar UI
#'
#' @return HTML for app sidebar
#' @export
#' @importFrom desc desc_get_version
#' @importFrom shiny tags h5 br hr icon
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem menuSubItem
sidebar_ui <- function() {

  shinydashboard::dashboardSidebar(
    collapsed=T,
    shiny::tags$div(
      shiny::hr(),
      shiny::h5(
        "LabCidadES/ProdestES",
        shiny::br(),
        "An\u00e0lise Explorat\u00f3ria de Dados e indicadores",
        shiny::br(),
        "Aplicativo Shiny",
        shiny::br(),
        paste0(
          "App Version: ",
          desc::desc_get_version(
            file = system.file(
              "DESCRIPTION",
              package = "AEDi"
            )
          )
        ),
        shiny::hr()
      ),
      align = "center",
      style = "font-weight: bold; color: #ffffff;"
    ),

    shinydashboard::sidebarMenu(
      id = "sidebar_menus",
      shinydashboard::menuItem(
        "Upload de Dados-Fontes",
        tabName = "upload_data",
        icon = shiny::icon("cloud-upload"),
        selected = TRUE
      ),
       shinydashboard::menuItem(
         "Diagn\u00f3stico de Dados",
         tabName = "diagnostics",
         icon = shiny::icon("cogs")
       ),
       shinydashboard::menuItem(
         "Dicion\u00e0rio de Dados",
         tabName = "data_dictionary",
         icon = shiny::icon("list")
       ),
       shinydashboard::menuItem(
         "Insights s/Dados",
         tabName = "insights",
         icon = shiny::icon("lightbulb"),
         startExpanded = FALSE,
         shinydashboard::menuSubItem(
           "Distribui\u00e3\u00f5es",
           tabName = "distributions",
           icon = shiny::icon("area-chart"),
         ),
         shinydashboard::menuSubItem(
           "An\u00e0lise Univariada",
           tabName = "univariate",
           icon = shiny::icon("line-chart")
         ),
         shinydashboard::menuSubItem(
           "An\u00e0lise Bi-Variada",
           tabName = "bivariate",
           icon = shiny::icon("bar-chart")
         )
       ),
       shinydashboard::menuItem(
         "Modelagem de Dados",
         tabName = "model",
         icon = shiny::icon("calculator"),
         startExpanded = FALSE,
         shinydashboard::menuSubItem(
           "Rela\u00e7\u00f5es preditoras",
           tabName = "predictors",
           icon = shiny::icon("balance-scale"),
         ),
         shinydashboard::menuSubItem(
           "Engenharia de Indicador",
           tabName = "feature",
           icon = shiny::icon("sliders")
         )
      )
    ) ,
 shiny::tags$div(
   style = 'padding: 10px; position:fixed; bottom:0px;',
   shiny::tags$a(
     shiny::tags$img(
       src = "www/MMC-Wide.png",
       width = 200
     ),
     href = "http://www.distintive.com.br"
   )
 )
)

}

