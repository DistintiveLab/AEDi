## To be copied in the UI

library(shiny)

uiui <- fluidPage(
  upload_datasus_ui("upload_datasus_1")
)

seserver <- function(input, output, session) {
  upload_datasus_server("upload_datasus_1")

}

shinyApp(uiui, seserver)

