#' upload_raispsql submodule - UI Function
#'
#' @description Submodule to upload data from Brazil's Labour
#' Ministry RAIS
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput uiOutput
#'


#module 'global'
{
  load("data/raismetalayoute.rda")
  load("data/raismetalayoutv.rda")
userais=Sys.getenv("mte_rais")
dbrais=Sys.getenv("dbrais")
hostrais=Sys.getenv("hostraispsql")
pwdrais=Sys.getenv("pwdrais")

conrais <-
  DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                 db=dbrais,user=userais,password=pwdrais,host=hostrais)

avinforais <-
  DBI::dbGetQuery(conrais,"SELECT column_name FROM information_schema.columns
                  WHERE table_schema = 'public' AND table_name = 'rais_vinculo_2023'" )

aeinforais <-
  DBI::dbGetQuery(conrais,"SELECT column_name FROM information_schema.columns
                  WHERE table_schema = 'public' AND table_name = 'rais_estabelecimento_2017'" )

anos_disp <-
  as.numeric(rev(sort(unique(gsub(".*_([0-9]+)$","\\1",DBI::dbGetQuery(conrais,"SELECT table_name FROM information_schema.columns
                  WHERE table_schema = 'public'")$table_name)))))


lraisv <- raismetalayoutv
lraise <- raismetalayoute

lowdash <- \(x){
  iconv(
    gsub("estb$","estab",
    gsub("2\\.0","2_0",
    gsub("emp_em_31/12","vinculo_ativo_31_12",
    gsub("horas_contr","qtd_hora_contr",
    gsub("tamestab","tamanho_estabelecimento",
    gsub("port_defic","ind_portador_defic",
    gsub("lidad$","lidade",
    gsub("nat_","natureza_",
    gsub("(distrito)_","\\1s_",
    gsub("ind_de_(.*_)vinc","ind_\\1vinculado",
    gsub(".*(motivo)_de_","\\1_",
    gsub("qt_","qtd_",
    gsub("afas$","afastamento",
    gsub("_adm$","_admissao",
         gsub("_estbl","_estab",
    gsub("tp_","tipo_",
         gsub("tempo_de","tempo",
    gsub("temp_empr$","tempo_emprego",
    gsub("_\\(sm\\)$","",
    gsub("sexo$","sexo_trabalhador",
    gsub("gr_instrucao_ou_","",
    gsub("clas_(.*)$","\\1_classe",
    gsub("cnae_20","cnae_2_0",
         gsub("sb_clas_20","cnae_2_0_subclasse",
  gsub("fort$","fortaleza",gsub("bairro_","bairros_",
  gsub("med_","media_",gsub("[_]+sm[_]*$","",
    gsub("dez_","dezembro_",
       gsub("\\(r\\$\\)","nom",
            gsub("^rem_","vl_remun_",
                 gsub("caus_afast","causa_afastamento",
                      # gsub("_$","",
                           gsub("[_]+([^_])","_\\1",
                                gsub(" ","_",tolower(x))
                              ))))))))))))))))))))))))))))))))),
  to = "ASCII//TRANSLIT")
  # )
}

retesp <- \(x){gsub("_"," ",gsub("qtd_","",x))}
choicesraisv <-
  avinforais|>
  dplyr::mutate(dplyr::across(column_name,lowdash))|>
                  dplyr::left_join(
                    lraisv|>
                      dplyr::mutate(dplyr::across(`Nome`,lowdash)),by=c("column_name"="Nome"))|>
  dplyr::mutate(`Descricao da Variável`=ifelse(is.na(`Descricao da Variável`) & grepl("^qtd_",column_name),retesp(`column_name`),
                                               `Descricao da Variável`))

choicesraise <-
  aeinforais|>
  dplyr::mutate(dplyr::across(column_name,lowdash))|>
  dplyr::left_join(
    lraise|>
      dplyr::mutate(dplyr::across(`Nome`,lowdash)),by=c("column_name"="Nome"))|>
  dplyr::mutate(`Descricao da Variável`=ifelse(is.na(`Descricao da Variável`) & grepl("^qtd_",column_name),retesp(`column_name`),
                                               `Descricao da Variável`))




raispsqlv <- c("",choicesraisv[!is.na(choicesraisv$`Descricao da Variável`),]$column_name)
labsv <- choicesraisv[!is.na(choicesraisv$`Descricao da Variável`),]$`Descricao da Variável`
names(raispsqlv) <- c("Digite para filtrar...",labsv)


raispsqle <- c("",choicesraise[!is.na(choicesraise$`Descricao da Variável`),]$column_name)
labse <- choicesraise[!is.na(choicesraise$`Descricao da Variável`),]$`Descricao da Variável`
names(raispsqle) <- c("Digite para filtrar...",labse)

#gdsl <- c("",raispsqlmeta$grupo_id)
#labsmg <- raispsqlmeta$grupo

#adsl <- c("",raispsqlmeta$assunto_id)
#labsma <- raispsqlmeta$assunto


#names(gdsl) <- c("Digite para filtrar...",labsmg)
#names(adsl) <- c("Digite para filtrar...",labsma)


#names(raispsqlmeta)[6:7] <- c("value","label")

infoagrv <-
  data.frame(dbcolname=c("qtd_vinculos",avinforais$column_name[grepl("^(vl_|qtd_)",avinforais$column_name)]))

infoagrv$dblowdash <- lowdash(infoagrv$dbcolname)

infoagrv$label <- c("Quantidade de Vínculos Ativos em 31/dez",names(raispsqlv[raispsqlv %in% infoagrv$dblowdash]))

infoagrdropdown <- infoagrv$dbcolname
names(infoagrdropdown) <- infoagrv$label

infoagre <-
  data.frame(dbcolname=c("qtd_estabelecimentos",aeinforais$column_name[grepl("^(vl_|qtd_)",aeinforais$column_name)]))

infoagre$dblowdash <- lowdash(infoagre$dbcolname)

infoagre$label <- c("Quantidade de Estabelecimentos em 31/dez",names(raispsqle[raispsqle %in% infoagre$dblowdash]))

infoagrdropdown <- infoagrv$dbcolname
names(infoagrdropdown) <- infoagrv$label

infoagredropdown <- infoagre$dbcolname
names(infoagredropdown) <- infoagre$label


}


upload_raispsql_ui <- function(id,parent_session){
  ns <- shiny::NS(id)
  nsp <- shiny::NS(parent_session)

  tagList(

    shiny::selectizeInput(ns("raispsqltab"),"GRUPO - Procure palavra-chave",
                          choices=c("estabelecimento","vinculo"),selected="estabelecimento",
                          multiple=F),
    shiny::selectizeInput(ns("raispsqly"),"Ano - selecione o ano",
                          choices=anos_disp,selected=character(0),
                          multiple=F),
    shiny::selectizeInput(ns("raispsqlagr"),"Selecione uma variável para agregação",
                          choices=infoagredropdown  , selected=character(0),multiple=F),
    shiny::selectizeInput(ns("raispsqlfiltr"),"Selecione uma informação para recorte categorial",
                           choices=raispsqle, selected=character(0),multiple=F),
    shiny::textInput(ns("critsel"),label="Critério de recorte",value=""),
    shiny::checkboxInput(ns("negativo"),label="Negar critério",value = F),
    shiny::actionButton(ns("buscaraispsql"),"Obter tabela",disabled=T),
    shiny::textInput(ns("urlapi"),label="api-url"),
     shiny::textInput(nsp("upload_file"),label="chamada")
  )
}

#' upload_raispsql Server Functions
#'
#' @noRd
upload_raispsql_server <- function(id,parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    nsp <- parent_session$ns

    observe(
      {
        i <- input$raispsqltab
        if(!length(i)){i <- ""}
        if(i=="vinculo") {
        shiny::updateSelectizeInput(session, inputId = 'raispsqlagr',
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
          shiny::updateSelectizeInput(session, inputId = 'raispsqlfiltr',
                                      choices = raispsqlv,
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
      #shiny::req(input$raispsqlagr,input$raispsqly)
      anous <- input$raispsqly
      i <- input$raispsqlagr
      if(length(i) != 0 ){
        shinyjs::enable("buscaraispsql")
        shinyjs::show("urlapi")
      } else {
        shinyjs::disable("buscaraispsql")
        shinyjs::hide("urlapi")
      }
    })

    observe({

      shiny::req(input$raispsqlagr,input$buscaraispsql)
      print("gerando url consulta")
      filc <- input$critsel
      variaveln <- input$raispsqlagr
      variavel <- ifelse(variaveln %in% c("qtd_vinculos","qtd_estabelecimentos"),"*",variaveln)
      url <- "pgsql local mte_rais"
      funcao <- ifelse(
        grepl("(vl|qt_[^ve])",input$raispsqlagr),"SUM","COUNT"
      )
      apichamada <- paste0(
        'DBI::dbGetQuery(conrais,"SELECT municipio local, ',funcao,"(",variavel,") ",variaveln,"_agr FROM rais_",input$raispsqltab,"_",input$raispsqly)

      if (input$raispsqltab == 'vinculo') {
        apichamada <- paste0(apichamada," WHERE vinculo_ativo_31_12 = 1  AND ")
      }
      if(filc!="") {
        apichamada <- paste0(apichamada,"WHERE ")
        apichamada <- gsub("AND WHERE ","AND ",apichamada)
        if(input$negativo)  {
          apichamada <- paste(apichamada,"NOT ")
        }
        if(grepl("\\^",filc)){
        apichamada <- paste0(apichamada,"CAST(",input$raispsqlfiltr," AS text) ~* ",filc)
        } else {
          apichamada <- paste0(apichamada,input$raispsqlfiltr," ",filc)
        }
      }
      apichamada <- paste(
        apichamada,'GROUP BY municipio")'
      )
      print(apichamada)
      shiny::updateTextInput(session=session,"urlapi",value=url)
      shiny::updateTextInput(session=parent_session,"upload_file",value=apichamada)
    })




  })
}


## To be copied in the UI
# upload_raispsql_ui("upload_raispsql_1")

## To be copied in the server
# upload_raispsql_server("upload_raispsql_1")
