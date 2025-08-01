#' upload_inep submodule - UI Function
#'
#' @description Submodule to upload data from Brazil's Education
#' Ministry inep
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput uiOutput
#' @importFrom educabR le_ideb
#'


#module 'global'
{


  infoinep <- educabR::metainep

infoideb <-educabR::metaideb

gruposinep <- unique(infoinep$assunto_id)
names(gruposinep) <- unique(infoinep$assunto)

infoinep$tabunica <- paste0(infoinep$assunto,"-",infoinep$tabela)

assuntos_ideb <- c("iniciais","finais","medio")
names(assuntos_ideb) <-
  c("IDEB - Municipal - Ensino Fundamental - Anos Iniciais",
    "IDEB - Municipal - Ensino Fundamental - Anos Finais",
    "IDEB - Municipal - Ensino Médio")

variaveisideb <- unique(c(infoideb[[1]]$indicador,infoideb[[2]]$indicador))

recortes <- sort(unique(c(infoideb[[1]]$rede,infoideb[[2]]$rede)))


}


upload_inep_ui <- function(id,parent_session){
  ns <- shiny::NS(id)
  nsp <- shiny::NS(parent_session)

  tagList(

    shiny::selectizeInput(ns("ineptab"),"GRUPO - Procure palavra-chave",
                          choices=gruposinep,selected=3,
                          multiple=F),
    shiny::selectizeInput(ns("inepagr"),"Selecione um Assunto",
                          choices=assuntos_ideb  , selected=character(0),multiple=F),
    shiny::selectizeInput(ns("inepfiltr"),"Selecione um indicador",
                           choices=variaveisideb, selected=character(0),multiple=F),
    shiny::selectizeInput(ns("ineprede"),"Selecione a rede/subrede",
                          choices=recortes, selected=character(0),multiple=F),
    shiny::checkboxInput(ns("preencherv"),label="Replica valor do ano anterior para anos faltantes",value = T),
    shiny::actionButton(ns("buscainep"),"Obter tabela",disabled=T),
    shiny::textInput(ns("urlapi"),label="api-url",width='100%'),
     shiny::textInput(nsp("upload_file"),label="chamada",width='100%')
  )
}

#' upload_inep Server Functions
#'
#' @noRd
upload_inep_server <- function(id,parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    nsp <- parent_session$ns

    observe(
      {
        i <- input$ineptab
        if(!length(i)){i <- ""}
        if(i=="vinculo") {
        shiny::updateSelectizeInput(session, inputId = 'inepagr',
                                    choices = infoagrdropdown,
                                    selected=character(0), server=T,
                                    options=list(
                                      render = I("{
        option: function(item, escape) {
          return '<div>'
            + '<strong>' + escape(item.value) + '</strong>'+
            '<br>' + escape(item.label) +
            '</div>';
        }
      }"),
                                      placeholder="Digite para filtrar...")
        )
          shiny::updateSelectizeInput(session, inputId = 'inepfiltr',
                                      choices = inepv,
                                      selected=character(0), server=T,
                                      options=list(
                                        render = I("{
        option: function(item, escape) {
          return '<div>'
            + '<strong>' + escape(item.value) + '</strong>'+
            '<br>' + escape(item.label) +
            '</div>';
        }
      }"),
                                        placeholder="Digite para filtrar...")
          )

        }

      })


    observe({
      shiny::req(input$inepfiltr,input$ineprede)
      i <- input$ineprede
      if(length(i) != 0 ){
        shinyjs::enable("buscainep")
        shinyjs::show("urlapi")
      } else {
        shinyjs::disable("buscainep")
        shinyjs::hide("urlapi")
      }
    })

    observe({

      shiny::req(input$inepagr,input$ineprede,input$buscainep)
      print("gerando url consulta")

      url <- "educabR::le_ideb"

      apichamada <- paste0(
        "educabR::le_ideb(nivel='",input$inepagr,
        "')|>dplyr::filter(indicador=='",input$inepfiltr,
        "',rede=='",input$ineprede,
        "',detalhe=='",input$inepfiltr,
        "')|>dplyr::select(codigo_municipio,ano,valor)|>",
        "dplyr::rename(`",input$inepfiltr," - Rede ",input$ineprede,"`=valor)")

      if(input$preencherv){
        apichamada <- gsub("(le_ideb[^)]*)\\)","\\1,replica=TRUE)",apichamada)
      }
      print(apichamada)
      shiny::updateTextInput(session=session,"urlapi",value=url)
      shiny::updateTextInput(session=parent_session,"upload_file",value=apichamada)
    })




  })
}


## To be copied in the UI
# upload_inep_ui("upload_inep_1")

## To be copied in the server
# upload_inep_server("upload_inep_1")
