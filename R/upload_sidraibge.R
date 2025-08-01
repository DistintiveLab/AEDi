#' upload_sidraibge submodule - UI Function
#'
#' @description Submodule to upload data from IBGE's SIDRA
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput uiOutput
#' @importFrom sidra sidra tab_meta

sidrameta <- sidra::sidrameta
labsm <- sidrameta$literal
sidrameta$id <- paste0(sidrameta$agregacao,sidrameta$id)
sidrametal <- c("",sidrameta$id)
names(sidrametal) <- c("Digite para filtrar...",labsm)
names(sidrameta)[1:2] <- c("value","label")

upload_sidraibge_ui <- function(id,parent_session){
  ns <- shiny::NS(id)
  nsp <- shiny::NS("data")
  tagList(

    shiny::selectizeInput(ns("sidrafilt"),"Procure palavra-chave",
                          choices=sidrametal[1:3],selected=character(0),
                          multiple=F),
    shiny::selectizeInput(ns("sidratab"),"Selecione uma tabela",
                          choices=NULL, selected=character(0),multiple=F),
    shiny::selectizeInput(ns("sidravar"),"Selecione uma variável",
                          choices=NULL, selected=character(0),multiple=F),
    shiny::actionButton(ns("buscasidra"),"Obter tabela",disabled=T),
    shiny::textInput(ns("urlapi"),label="api-url"),
     shiny::textInput(nsp("upload_file"),label="chamada")
  )
}

#' upload_sidraibge Server Functions
#'
#' @noRd
upload_sidraibge_server <- function(id,parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    nsp <- parent_session$ns



    observe({
      shiny::req(input$sidratab,input$sidravar)
      i <- input$sidravar
      if(length(i) != 0 ){
        shinyjs::enable("buscasidra")
        shinyjs::show("urlapi")
      } else {
        shinyjs::disable("buscasidra")
        shinyjs::hide("urlapi")
      }
    })

    observe({

      shiny::req(input$sidravar,input$sidratab,input$buscasidra)
      print("gerando url api")
      metasid <- desctabela(input$sidratab)
      localidade <- max(c(metasid$nivelTerritorial,1))
      variavel <- gsub("[^[:digit:]].*$","",input$sidravar)
      url <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/",
                    input$sidratab,"/variaveis/",
                    variavel,"/?localidades=",localidade)
      print(paste("Gerado url de acesso aos dados via api:\n",url))
      apichamada <- paste0(
        "sidra::sidra(",input$sidratab,", nivel = '",localidade,"', variavel = ",
        variavel,")")
      print(apichamada)
      shiny::updateTextInput(session=session,"urlapi",value=url)
      shiny::updateTextInput(session=parent_session,"upload_file",value=apichamada)
        })

    observe(
      {
        i <- input$sidrafilt
        if(!length(i)){i <- ""}
        escolhas <-
          filtratabs(i)
        names(escolhas) <- c("value","label")
        shiny::updateSelectizeInput(session, inputId = 'sidratab',
                                    label="Tabela - Procure palavras-chave",
                                    choices = escolhas, server = TRUE,
                                    selected=character(0),
                                    options=list(
                                      maxOptions=13000,
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

      })#|>bindEvent(input$sidrafilt)


    observe({
#            shiny::req(input$sidratab)
            i <- input$sidratab
          escolhas <- try(desctabela(i))
          if(class(escolhas)!="try-error") {
          escolhas <- escolhas$variaveis[,1:2]
          names(escolhas) <- c("value","label")} else {
            escolhas <- data.frame(value="",label="Aguardando valor")
          }
          shiny::updateSelectizeInput(session, inputId = 'sidravar',
                                      label="Variável - Procure palavras-chave",
                                      choices = escolhas, server = TRUE,
                                      selected=character(0),
                                      options=list(
                                        maxOptions=13000,
                                        render = I("{
        option: function(item, escape) {
          return '<div>'
            + '<strong>' + escape(item.value) + '</strong>'+
            '<br>' + escape(item.label) +
            '</div>';
        }
      }"),
                                        placeholder="Digite para filtrar..."))

      })#|>bindCache(input$sidratab)|>
      # bindEvent(input$sidratab,ignoreNULL=T,ignoreInit = T)
      shinyjs::hide(nsp("upload_file"))

      shiny::updateSelectizeInput(session, inputId = 'sidrafilt',
                                label="Procure palavras-chave",
                                choices = sidrameta[1:2], server = TRUE,
                                selected=character(0),
                                options=list(
                                  maxOptions=13000,
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

  })
}


## To be copied in the UI
# upload_sidraibge_ui("upload_sidraibge_1")

## To be copied in the server
# upload_sidraibge_server("upload_sidraibge_1")
