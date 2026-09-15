#' upload_datasus submodule - UI Function
#'
#' @description Submodule to upload data from Brazil's Health Ministry
#' FUNASA DATASUS
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput uiOutput
#' @import from datasus metatabnet

#module 'global'
{
library(datasus)
datasusmeta <- datasus::metatabnet

##FILTRAR - SÓ FUNÇÕES DATASUS IMPLEMENTADAS
datasusimplementado <- as.character(ls.str("package:datasus"))

dsfunc <-
  lapply(
    gsub("(br|uf)$","",
         gsub("(br|uf)$","",
              gsub("_","/.*/",
                   gsub("_[^_]+$","",datasusimplementado)))),
         \(x){dplyr::filter(datasusmeta,grepl(x,taburl))|>dplyr::select(taburl)})
dsgnch <- datasusimplementado[sapply(dsfunc,nrow)>0]

dsfunc <- as.numeric(unlist(data.table::rbindlist(
  lapply(
    gsub("(br|uf)$","",
         gsub("(br|uf)$","",
              gsub("_","/.*/",
                   gsub("_[^_]+$","",dsgnch)))),
    \(x){dplyr::filter(datasusmeta,grepl(x,taburl))|>dplyr::select(id)}))))


datasusmeta <- datasusmeta[dsfunc,]
datasusmetal <- c("",datasusmeta$id)
labsm <- datasusmeta$tabela

gdsl <- c("",datasusmeta$grupo_id)
labsmg <- datasusmeta$grupo

adsl <- c("",datasusmeta$assunto_id)
labsma <- datasusmeta$assunto


names(datasusmetal) <- c("Digite para filtrar...",labsm)
names(gdsl) <- c("Digite para filtrar...",labsmg)
names(adsl) <- c("Digite para filtrar...",labsma)


names(datasusmeta)[6:7] <- c("value","label")

}
upload_datasus_ui <- function(id){
  ns <- shiny::NS(id)

  tagList(

    shiny::selectizeInput(ns("datasusfilt"),"GRUPO - Procure palavra-chave",
                          choices=gdsl[!duplicated(gdsl)],selected=character(0),
                          multiple=F),
    shiny::selectizeInput(ns("datasustab"),"Selecione um assunto",
                          choices=adsl[!duplicated(adsl)], selected=character(0),multiple=F),
    shiny::selectizeInput(ns("datasusvar"),"Selecione uma tabela",
                          choices=datasusmeta[6:7], selected=character(0),multiple=F),
    shiny::actionButton(ns("buscadatasus"),"Obter tabela",disabled=T),
    shiny::textInput(ns("urlapi"),label="api-url"),
#     shiny::textInput(nsp("upload_file"),label="chamada")
  )
}

#' upload_datasus Server Functions
#'
#' @noRd
upload_datasus_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    observe({
      shiny::req(input$datasustab,input$datasusvar)
      i <- input$datasusvar
      if(length(i) != 0 ){
        shinyjs::enable("buscadatasus")
        shinyjs::show("urlapi")
      } else {
        shinyjs::disable("buscadatasus")
        shinyjs::hide("urlapi")
      }
    })

    # observe({
    #
    #   shiny::req(input$datasusvar,input$datasustab,input$buscadatasus)
    #   print("gerando url api")
    #   metadsus <- desctabela(input$datasustab)
    #   localidade <- max(metadsus$nivelTerritorial)
    #   variavel <- gsub("[^[:digit:]].*$","",input$datasusvar)
    #   url <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/",
    #                 input$datasustab,"/variaveis/",
    #                 variavel,"/?localidades=",localidade)
    #   print(paste("Gerado url de acesso aos dados via api:\n",url))
    #   apichamada <- paste0(
    #     "datasus::datasus(",input$datasustab,", nivel = '",localidade,"', variavel = ",
    #     variavel,")")
    #   print(apichamada)
    #   shiny::updateTextInput(session=session,"urlapi",value=url)
    #   shiny::updateTextInput(session=parent_session,"upload_file",value=apichamada)
    #     })

    observe(
      {
        i <- input$datasusfilt
        if(!length(i)){i <- ""}
        escolhas <-
          adsl[c("",datasusmeta$grupo_id) == i]
        escolhas <- data.frame(value=escolhas,label=names(escolhas))
        print(escolhas)
        #names(escolhas) <- c("value","label")
        shiny::updateSelectizeInput(session, inputId = 'datasustab',
                                    label="Tabela - Procure palavras-chave",
                                    choices = escolhas, server = T,
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
                                      placeholder="Digite para LLAfiltrar...")
        )

      })#|>bindEvent(input$datasusfilt)


    observe({
#            shiny::req(input$datasustab)
            i <- input$datasustab
          escolhas <- try(datasusmetal[c("",datasusmeta$assunto_id) == i])
          # if(class(escolhas)!="try-error") {
          # escolhas <- escolhas$variaveis[,1:2]
          # names(escolhas) <- c("value","label")} else {
          #   escolhas <- data.frame(value="",label="Aguardando valor")
          # }
          shiny::updateSelectizeInput(session, inputId = 'datasusvar',
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

      })#|>bindCache(input$datasustab)|>
      # bindEvent(input$datasustab,ignoreNULL=T,ignoreInit = T)


      shiny::updateSelectizeInput(session, inputId = 'datasusfilt',
                                label="GRUPO - Procure palavras-chave",
                                choices = gdsl, server = TRUE,
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
# upload_datasus_ui("upload_datasus_1")

## To be copied in the server
# upload_datasus_server("upload_datasus_1")
