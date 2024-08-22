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
        "Análise Exploratória de Dados e indicadores",
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
         "Diagnóstico de Dados",
         tabName = "diagnostics",
         icon = shiny::icon("cogs")
       ),
       shinydashboard::menuItem(
         "Dicionário de Dados",
         tabName = "data_dictionary",
         icon = shiny::icon("list")
       ),
       shinydashboard::menuItem(
         "Insights s/Dados",
         tabName = "insights",
         icon = shiny::icon("lightbulb"),
         startExpanded = FALSE,
         shinydashboard::menuSubItem(
           "Distribuições",
           tabName = "distributions",
           icon = shiny::icon("area-chart"),
         ),
         shinydashboard::menuSubItem(
           "Análise Univariada",
           tabName = "univariate",
           icon = shiny::icon("line-chart")
         ),
         shinydashboard::menuSubItem(
           "Análise Bi-Variada",
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
           "Relações preditoras",
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
     href = "http://www.oliverwyman.com/index.html"
   )
 )
)

}

