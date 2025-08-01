#' Upload Data Module - UI
#'
#' @param id Namespace ID
#'
#' @return tagList of shinyFiles buttons
#' @export
#'
#' @examples
#' \dontrun{
#' upload_data_ui("data")
#' }
#' @importFrom DT DTOutput
#' @importFrom readr write_csv
#' @importFrom shiny NS fluidRow column icon uiOutput selectInput textInput tagList
#' reactiveVal is.reactive renderPrint eventReactive
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box
#' @importFrom shinyFiles shinyFilesButton shinyDirButton shinySaveButton
#' @importFrom shinyjs disabled useShinyjs
#' @importFrom sortable bucket_list rank_list add_rank_list
#' @importFrom utils read.csv read.csv2 download.file str
#' @import futile.logger


upload_data_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- shiny::NS(id)

  tagList(
  shinydashboard::tabBox(
    id = ns("data_upload"),
    title = icon_text("cloud-upload", "Carregue e Explore Dados"), #"procure", "Vista pr\u00e9via:"),
    width = 12,

    shiny::tabPanel(
      title = icon_text("file", "Inser\u00e7\u00e3o de Fonte"),
      width = 12,
      flucol(
        shiny::div(
          style = "inline; float:left",
          shiny::selectInput(
            inputId = ns("sourcetype"),
            label = "Tipo de Fonte",
            choices = c(
              "url fixo"=1,
              "arquivo local(upload)"=2,
              "dados.gov.br"=3,
              "ckan"=4,
              "ibge_sidra"=5,
              "ibge_ftp"=6,
              "ipeadata"=7,
              "bcb"=8,
              "arquivo do servidor" = 9,
              "url de pasta ou combinada" = 10,
              "datasus" = 11,
              "raispsql" = 12,
              "inep" = 13),
            selected=11),
          shiny::textInput(ns("nomefonte"),"Nome curto para fonte","nova_fonte",width="200px","Indique um nome para a fonte"),
          shiny::uiOutput(ns("cargatipo")),
          shiny::actionButton(inputId = ns("gettab"),"Obter",icon=shiny::icon("download"))
          # shinyFiles::shinySaveButton(
          #   id = ns("save_file"),
          #   label = "Salvar para Arquivo",
          #   title = "Selecione um arquivo para salvar:",
          #   # buttonType = "primary",
          #   icon = shiny::icon(
          #     "save"
          #   )
          # )|> shinyjs::disabled()
        ),

        # shiny::br(),
        DT::DTOutput(
          ns("files_table")
        ),
        # shiny::br(),
        # shiny::h3("Here will be additional settings for uploading files."),
        # shiny::h5("For Example: Merging Files Together, Selecting Excel Tabs, etc.")
      )
    ),

    shiny::tabPanel(
      title = icon_text("table", "Tabela de Dados"),
      flucol(
        shiny::div(
          # style = "inline; float:left",
          shiny::uiOutput(ns("data_picker"))
        ),
        flucol(
          DT::DTOutput(
            ns("data_table"),
            width = "100%"
          )
        )
      )
    ),

    shiny::tabPanel(
      title = icon_text("book", "Resumo"),
      flucol(
        shiny::uiOutput(ns("data_summary"))
      )
    ),

    shiny::tabPanel(
      title = icon_text("list", "Vari\u00e1veis e Indicadores"),
      fluidRow(
        column(5,shiny::textInput(
        inputId = ns("filtraVars"),
        label = "Insira texto para filtrar vari\u00e1veis de interesse",
        value = ""
      )
      ),
      column(1),
      column(5,
          shiny::textInput(
            inputId= ns("nome_indicador"),
            label = "Insira nome para o indicador",
            value="nome_indicador"
          ))
      ),
      fluidRow(column(5,
        shinymath::mathInput(ns("equacao"),"Insira equa\u00e7\u00e3o"),
        shiny::actionButton(ns("previaindicador"),"Rodar!"),
        shiny::verbatimTextOutput(ns("text_r"), placeholder = TRUE),
        "Trabalho em Andamento"
      ),column(1),
      column(5,
             shiny::radioButtons(ns('varAgreg'),inline = TRUE,
                                 selected=character(0),
                                 'Agregação para var A por grupo?',
                                 choices = agregfunc))),
      fluidRow(
        shiny::uiOutput(ns("mapeiavars")),
        shiny::uiOutput(ns("selecionados"))#,
#        shiny::uiOutput(ns("resultados_dragdrop"))
      ),
      fluidRow(
        shiny::uiOutput(ns("grupo_filtro"))
      )
      )
  )
)
}

#' Upload Data Module - Server
#'
#' @param id shiny id
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return list of uploaded files and data
#' @export
#'
#' @examples
#' \dontrun{
#' shiny::callModule(upload_data, "data")
#' }
#' @importFrom dplyr pull transmute row_number
#' @importFrom DT renderDT datatable
#' @importFrom fs path_package path_home path path_ext_remove path_ext
#' @importFrom purrr map set_names map_dbl
#' @importFrom rio import
#' @importFrom shiny reactive req observe renderUI
#' @importFrom shinymath mathInput
#' @importFrom latex2r latex2r
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom summarytools dfSummary
#' @noRd

upload_data_server <- function(id) {
  moduleServer(id,function(input, output, session) {


  # namespace
  ns <- session$ns
  dadosbase <- reactiveVal("dadosbase")

  ## OLD CODE, WHEN NO DB USED
  #   r <- shiny::reactiveVal(if(dir.exists("manipula/metadados")){
  #   sort(unique(unlist(
  #     lapply(list.files("manipula/metadados",pattern="*.csv",full.names =T),
  #            \(x){utils::read.csv(x)[[1]]}))))
  # })


  pegavals <- \(x) {
    {
      #con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tdbname)
      con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                            dbname=Sys.getenv("dbname"),
                            user=Sys.getenv("user"),
                            password=Sys.getenv("password"),
                            host = Sys.getenv("host"))


      availableind <- DBI::dbGetQuery(con,paste0(
        "SELECT orig_name value, data_desc label FROM ",
        "mdata"))|>dplyr::arrange(value)


      availrecortes <- names(DBI::dbGetQuery(con,paste0(
        "SELECT * FROM ",
        "recortes_geograficos limit 1")))[-1:-3]

      DBI::dbDisconnect(con)

      names(availrecortes) <- availrecortes
      availrecortes <- gsub("_"," ",availrecortes)
      availableind <- rbind(availableind,data.frame(value=c('refdate','local_id'),
                                                    label=c('periodo','local')))
      names(availableind$value) <- availableind$label


      c(availableind$value,availrecortes)
    }
  }
  ## NEW CODE - CHECK IN DB
    r <- shiny::reactiveVal(
      pegavals(0)
    )


    # fvars <-  observe({
    #   req(input$previaindicador)
    #   filtro <- input$filtraVars
    #   vfonte <- sort(unique(unlist(
    #     lapply(list.files("manipula/metadados",pattern="*.csv",full.names =T),
    #            \(x){utils::read.csv(x)[[1]]}))))
    #
    #   r(vfonte)
    # if (nchar(filtro) > 0) {
    #   vfonte <-
    #     vfonte[
    #       grepl(
    #         x = vfonte,
    #         pattern = paste0(input$filtraVars,input$varsdestino),
    #         ignore.case = TRUE
    #       )
    #     ]
    # } else {
    #   vfonte <- vfonte
    # }
    #   r(vfonte)
    # })

  output$mapeiavars <- renderUI({
      column(
        width = 5,
        sortable::bucket_list(
          header = "seleciones as vari\u00e1veis",
          group_name = ns("varsdestino"),
          orientation = "horizontal",
          class = "tamanho_max",
          sortable::add_rank_list(
            text = "Arraste daqui",
            labels = r(),
            options = sortable::sortable_options(sort = T),
            input_id = ns("varsfontes")
          )
        )
      )

  })

  output$selecionados <- renderUI({
    column(5,
           sortable::bucket_list(
             header = "... para aqui",
             orientation = "vertical",
             group_name = ns("varsdestino"),
             sortable::add_rank_list(
               text = "A",
               labels = NULL,
               input_id = ns("varA")
             ),
             sortable::add_rank_list(
               text = "B",
               labels = NULL,
               input_id = ns("varB")
             ),
             sortable::add_rank_list(
               text = "C",
               labels = NULL,
               input_id = ns("varC")
             ),
             sortable::add_rank_list(
               text = "D",
               labels = NULL,
               input_id = ns("varD")
             )
           )
           )
  })

  output$grupo_filtro <-  renderUI({
    tagList(
      column(9,
           sortable::bucket_list(
             header = "Filtro/Grupo",
             orientation = "horizontal",
             group_name = ns("varsdestino"),
             sortable::add_rank_list(
               text = "Agrupar por",
               labels = NULL,
               input_id = ns("varGroup")
             ),
             sortable::add_rank_list(
               text = "Filtrar por",
               labels = NULL,
               input_id = ns("varFilter")
             )
           )
    ),
    column(3,
           tags$br(),
           tags$br(),
           tags$br(),
           shinyWidgets::textInputIcon(icon = icon("filter"),label = "Valor do filtro:",
                         value="",placeholder = "ex: > 2022",inputId = ns("valueFilter")))
    )
  })
  output$resultados_dragdrop <- renderUI({
    tagList(
      fluidRow(
        column(
          width = 12,
          tags$b("Resultados"),
          column(
            width = 12,

            tags$p("Vari\u00e1vel A"),
            shiny::verbatimTextOutput(ns("results_1")),

            tags$p("Vari\u00e1vel B"),
            shiny::verbatimTextOutput(ns("results_2")),

            tags$p("Vari\u00e1vel C"),
            shiny::verbatimTextOutput(ns("results_3"))
          )
        )
      )
    )
  })

  # debugei <- observe({
  #
  #   print("Rodou uma vez o debugei")
  #   input$filtraVars
  #   input$varsfontes
  #   input$varA
  #   })




  output$results_1 <- #reactive({
#    print(reactiveValuesToList(input))
    shiny::renderPrint({
      input$varA

#      debugei() # This matches the input_id of the first rank list
    }
    )
  #})


  output$results_2 <-
    shiny::renderPrint(
      input$varB # This matches the input_id of the second rank list
    )
  output$results_3 <-
    shiny::renderPrint(
      input$varC # Matches the group_name of the bucket list
      # print(reactiveValuesToList(input))
    )

  # volumes for shinyFiles inputs
  volumes <- c(
    'Demo Data' = fs::path_package("AEDi", "extdata"),
    'Home' = fs::path_home(),
    'Documents' = fs::path(fs::path_home(), "pRojetos/AEDi/coleta/cache"),
    shinyFiles::getVolumes()()
  )


  # observers for each button
  shinyFiles::shinyFileChoose(input, "upload_file", session = session, roots = volumes)
  # shinyFiles::shinyDirChoose(input, "upload_folder", roots = volumes)
  # shinyFiles::shinyFileSave(input, "save_file", roots = volumes)


  # uiOutput dependable on selectbox sourcetype

tipocarga <- reactive({
  req(input$sourcetype)


  if ( input$sourcetype == "2") {
    shiny::fileInput(
      inputId =  ns("upload_file"),
      label = "upload (csv,xlsx)",
      #title = "Selecione Arquivo(s) para Upload:",
      multiple = FALSE,
      placeholder = "Nenhum arquivo selecionado",
      # buttonType = "primary",
      buttonLabel = shiny::icon(
        "file"
      ))
  } else  if (input$sourcetype == "1") {

  shiny::textInput( ns("upload_file"),label="URL",placeholder="https://...",width="200px")
  } else if (input$sourcetype == "3") {

  } else if (input$sourcetype == "5") {
    upload_sidraibge_server("sidra",parent_session = session)
    upload_sidraibge_ui(shiny::NS(id,"sidra"),parent_session=session)
  }  else if (input$sourcetype == "9") {
   shinyWidgets::panel(
      shinyFiles::shinyFilesButton(
        id = ns("upload_file"),
        label = "Fa\u00e7a upload",
        title = "Selecione Arquivo(s) para Upload:",
        multiple = TRUE,
        # buttonType = "primary",
        icon = shiny::icon(
          "file"
        )
      )
    )
  } else if (input$sourcetype == "11") {
    upload_datasus_server("datasus",parent_session = session)
    upload_datasus_ui(shiny::NS(id,"datasus"),parent_session='data')
  } else if (input$sourcetype == "12") {
    upload_raispsql_server("raispsql",parent_session = session)
    upload_raispsql_ui(shiny::NS(id,"raispsql"),parent_session='data')
  } else if (input$sourcetype == "13") {
    upload_inep_server("inep",parent_session = session)
    upload_inep_ui(shiny::NS(id,"inep"),parent_session='data')
  }

})

  output$cargatipo <- renderUI({
    tipocarga()

  })


  ##monitor equation
  math <- reactive({
    req(input$equacao)
    req(input$previaindicador)
   # print("VEja aqui o valor:")
   # print(input$equacao)
    input$equacao
  })


  ###util function parse eq
  parseeq <- \(math){
    if(missing(math) | is.null(math)){
      "Insira uma equação no campo acima para que a ação de rodar o código
        possa ser completada."
    } else {
      nfonte <- input$nomefonte
      nind <- input$nome_indicador
      if(grepl("ã",math)) {
        mediana_a <- T
        traduzeq <- gsub("ã","a",math)
      } else {
        mediana_a <- F
        traduzeq <- math
      }

      print(traduzeq)

      baseq <- latex2r::latex2r( traduzeq)

      # if (class(baseq)=='try-error'){
      #   return("erro")
      # }
      fltro <- ifelse(is.null(input$varFilter)|is.na(input$varFilter),"",input$varFilter)
      fltrov <- input$valueFilter
#      print(paste0("filtro é igual a:",fltro,"$"))
#      print(paste0("filtrov é igual a:",fltrov,"$"))
      grpo <- input$varGroup
      somasna <- \(x){sum(x,na.rm=T)}
      #prepara <- nfonte
      prepara <- "dbdbase"
      if(length(prepara)>0) {

        if(length(grpo)>0){
          prepara <- paste0(prepara,"|>
                            dplyr::group_by(",paste0(grpo,collapse=","),")",
#                        summarize(across(where(is.numeric),somasna),across(where(is.character),first))|>
                        "")

        }
        if(length(grpo)>1){
          if(length(input$varAgreg)>0){
          if(input$varAgreg %in% agregfunc) {
          prepara <- paste0(
            prepara,"|> ",
            "dplyr::mutate(",input$varA,"= ",input$varAgreg,"(",input$varA,"))|>dplyr::ungroup()"
          )}
          }
        }
        if (length(fltro)>0 & length(fltrov)>0){
          prepara <- paste0(prepara,"|> filter(",fltro,fltrov,")")
        }
        vex <- c(length(input$varA)>0,
                 length(input$varB)>0 ,
                 length(input$varC)>0,
                 length(input$varD)>0)

        varsval <- letters[1:4][vex]
        valvars <- c(list(input$varA),list(input$varB),
                     list(input$varC),list(input$varD))[vex]

        names(valvars) <- varsval

        valvars <- unlist(valvars,use.names=T)


        codigo <- paste0(nind," <- ",prepara," |>
                     dplyr::rename(setNames(c('",paste0(valvars,collapse="','"),
                         "'), c('",paste0(names(valvars),collapse="','"),"'))) |>
                dplyr::transmute(",nind," = ",baseq,",refdate,local_id)")
        if (mediana_a){
          codigo <- gsub("(transmute.* =[- ]*)a","\\1 median(a,na.rm=T)",codigo)
        }
        if(length(grpo)>0){
          codigo <- gsub("$","|> dplyr::ungroup()",codigo)
        }

        codigo <- gsub('mean.([^)]*).',"rowMeans(dplyr::pick(dplyr::matches('^\\1[[:digit:]]*$')),na.rm=T)",codigo)
        cat(codigo)
        codigo


      } else {baseq}
    }

  }
  nm_novoind <- shiny::reactiveVal()


  ##Math insira equação
  shiny::bindEvent({



    output$text_r <-  shiny::renderText({
      parseeq(math())
    })
  },
    input$previaindicador, input$equacaos
                   )

  observe({
    shiny::req(input$previaindicador,input$varA)
    nm_novoind(input$nome_indicador)
    nindfn <- paste0("coleta/",nm_novoind(),".R")
    futile.logger::flog.appender(
      futile.logger::appender.file(paste0(nindfn,".log")),
      name=nm_novoind())

    vex <- c(length(input$varA)>0,
             length(input$varB)>0 ,
             length(input$varC)>0,
             length(input$varD)>0)
    varsval <- letters[1:4][vex]
    nnullvars <- c(list(input$varA),list(input$varB),list(input$varC),
                   list(input$varD))[vex]

    nnullvars <- unlist(nnullvars,use.names=T)



    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname=Sys.getenv("dbname"),
                          user=Sys.getenv("user"),
                          password=Sys.getenv("password"),
                          host = Sys.getenv("host"))

    ## DETOUR CHECK AND ADD TO METADATA
    ## Check if there is AEDi as institution and a datasource associated with it

    selfrefdatasourcexist <-
      DBI::dbGetQuery(con,paste0("select datasource_id from \n",
      "datasource left join institution on datasource.institution_id = \n",
      "institution.institution_id WHERE \n",
      "institution_sname LIKE '%AEDi'"))



    if(nrow(selfrefdatasourcexist)==0){
      ###create institution, contact and datasource
      niid <- 1+DBI::dbGetQuery(con,"select max(institution_id) from institution")$max
      ncid <- 1+DBI::dbGetQuery(con,"select max(officialer_id) from officialer")$max
      ndid <- 1+DBI::dbGetQuery(con,"select max(datasource_id) from datasource")$max

      aedinst <- data.frame(
        institution_id = niid,
        institution_sname = "AEDi",
        institution_name = "Pacote para R - Análise Exploratória de Dados e Indicadores",
        institution_url = "https://github.com/distintivelab/AEDi",
        institution_desc = "Rodrigo E. S. Borges (Distintive) - criador e implementador"

      )

      aedicontact <- data.frame(
        officialer_id=ncid,
        officialer_name="Rodrigo Emmanuel Santana Borges",
        officialer_email="borges@distintive.com.br",
        officialer_tel=982139405,
        institution_id = niid,
        officialer_obs = "Package Creator/Data Scientist"
      )


      aedids <- data.frame(
        datasource_id = ndid,
        datasource_name = "AEDi - estimação própria",
        datasource_url = "Interno",
        data_freq_id = 9,
        datasource_lastupdate = Sys.time(),
        institution_id = niid,
        officialer_id = ncid,
        datasource_desc = "Derivado a partir dos dados importados para o Backend",
        datasource_delay = 0
      )
      DBI::dbWriteTable(con,"institution",
                         aedinst,append=T,row.names=F)

      DBI::dbWriteTable(con,"officialer",
                        aedicontact,append=T,row.names=F)

      DBI::dbWriteTable(con,"datasource",
                        aedids,append=T,row.names=F)


    }


    ## END DETOUR

    pedvars <- "SELECT * from geonamed_datavalues "

        if(length(nnullvars)) {
      pedvars <- paste0(pedvars,
        "WHERE orig_name IN ('",paste0(nnullvars,collapse="','"),"')")
        }
    print(pedvars)

    #dplyr::mutate(refdate=as.Date.character(paste0(lubridate::year(`refdate`),'-12-31'),tryFormats='%Y-%m-%d'))|>

    #futile.logger::flog.info(pedvars,name = nm_novoind())
    dbdbase <- DBI::dbGetQuery(con,pedvars)

    DBI::dbDisconnect(con)

    comandoformato <- "dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)"
#list(datasus_cnes_prid02br_mun=0)
    dbdbase <- eval(parse(text=comandoformato))
    print(comandoformato)
    print("resultado")
    print(summary(dbdbase))

    indiconstruct <- parseeq(math())

    mensaj <- paste0("###Filtra e prepara dados base\n",
                    ' dbdbase <- DBI::dbGetQuery(con,"',pedvars,'")',
                     "\n","dbdbase <- ",comandoformato,"\n###Cria indicador\n",
                     "dbdbase <- ",indiconstruct)

    write(mensaj,nindfn)
    futile.logger::flog.info(
      mensaj,name = nm_novoind())
    novoindicador <- eval(parse(text=indiconstruct))

    ###PREPARE DATA FOR WRITING
    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname=Sys.getenv("dbname"),
                          user=Sys.getenv("user"),
                          password=Sys.getenv("password"),
                          host = Sys.getenv("host"))

    selfdatasource <-
      DBI::dbGetQuery(con,
                      paste0("select datasource_id from \n",
                             "datasource left join institution on datasource.institution_id = \n",
                             "institution.institution_id WHERE \n",
                             "institution_sname LIKE '%AEDi'"))$datasource_id


    mdata_nind <- data.frame(
      orig_name = nm_novoind(),
      data_name = gsub("_"," ",nm_novoind()),
      data_desc = mensaj
    )

    mdataext_nind <- data.frame(
      data_class_id = 1, # TBD - DEFINE A FUNCTION FOR DEDUCTING,
      data_freq_id = 9, # TBD - GET SELECTED FREQ
      dataunit_num = "", # TBD
      dataunit_den = "", # TBD
      data_type_id = 1, # TBD,
      datasource_id = selfdatasource,
      data_url = "Este aplicativo",
      mdata_obs = paste("Criado com os seguintes comandos:\n",mensaj)
    )




  names(novoindicador) <- c("valor","periodo","local")
  db_datawrite(list(mdata_nind,mdataext_nind),novoindicador,sanitize=F)

  })

  selected_files <- reactive({

    shiny::req(input$upload_file,input$nomefonte,input$gettab)
    shinybusy::add_busy_bar()

    dir.create(paste0("coleta/cache/",input$nomefonte),showWarnings = F,recursive = T)
    #extensao <- gsub(".*(\\.[^.]+)$","\\1",input$upload_file)
    #if(!length(extensao)){
      extensao <- ".csv"
    #}
    vars_fonte <- paste0("manipula/cache/metadados/",input$nomefonte,extensao)
    nomea <- paste0(input$nomefonte,extensao)

    narq <- paste0('coleta/cache/',input$nomefonte,"/",nomea)
    if (input$sourcetype == 2 ) {


      readr::write_csv(readr::read_csv(nomea),narq)
      paste0(getwd(),narq)
    } else
      if (input$sourcetype == 1 ) {

      download.file(input$upload_file,narq,method="wget")
      } else
        if (input$sourcetype == 5 ) {
          # tabela <- httr::GET(input$upload_file)|>httr::content("text")
          # tabela <- jsonlite::fromJSON(tabela, flatten = TRUE)
          # tabela <- tabela$resultados[[1]]$series[[1]]
          tabela <- eval(parse(text=input$upload_file))

          ####DATAFILE WRITER

          readr::write_csv(tabela,narq)

        } else
          if (input$sourcetype == 11 ) {
            assign("mmfreq",9,envir=.GlobalEnv)

             perdatasus <- \(x){
              if(all(nchar(x)==4)) {

                assign("mmfreq",9,envir=.GlobalEnv)
                lubridate::as_date(paste0(x,"07",'01',sep="-"))
              } else if(all(nchar(x)==8 & is.na(as.numeric(x)))) {

                assign("mmfreq",4,envir=.GlobalEnv)
                Sys.setlocale("LC_TIME","pt_BR.UTF-8")
                as.Date.character(paste0(x,"/01"),tryFormats = c("%Y/%B/%d",
                                                                 "%B/%Y/%d"))
              }

             }

            tabela <- eval(parse(text=input$upload_file))

            tabela <- tabela|>
              tidyr::pivot_longer(-1,names_to="periodo",values_to="valor")|>
              dplyr::mutate(across(`periodo`,perdatasus))|>dplyr::rename(local=1)|>
              dplyr::mutate(across(local,\(x){gsub("Total","Brasil",stringr::str_to_title(x))}))

            ####DATAFILE WRITER

            readr::write_csv(tabela,narq)

            ###DB DATA WRITER



            tttex <- input$upload_file

            mdatarow <- data.frame(
              orig_name=input$nomefonte,
              data_name= paste0(gsub("::","_",gsub("\\(.*","",tttex))),
              data_desc="auto import datasus - check source"
            )

            mdata_extsrow <- data.frame(
              data_class_id=1,
              data_freq_id=mmfreq,
              data_type_id=1,
              datasource_id=4,
              data_url=tttex
            )


            db_datawrite(list(mdatarow,mdata_extsrow),tabela,tttex)



          } else if (input$sourcetype == 12 ) {

            gerano <- \(ano) {
              print(paste0('puxando ',gsub("([tl]o_)[0-9]{4}",paste0("\\1",ano),input$upload_file)))
              x <- eval(parse(text=gsub("([tl]o_)[0-9]{4}",paste0("\\1",ano),input$upload_file)))
              x$periodo <- as.Date(paste0(ano,"-12-31"))
              print(summary(x))
              x
            }
            print("fazendo todos os anos")
            tabela <- rbindlist(lapply(2016:2023,gerano))

            assign("mmfreq",9,envir=.GlobalEnv)

            ####DATAFILE WRITER

            readr::write_csv(tabela,narq)

            ###DB DATA WRITER



            tttex <- names(tabela)[ncol(tabela)-1]

            mdatarais <- data.frame(
              orig_name=input$nomefonte,
              data_name= paste0(tttex,"_",gsub('_[^_]+$','',input$nome_fonte)),
                #paste0(gsub("::","_",gsub("\\(.*","",tttex))),
              data_desc="auto import rais - check source code"
            )
             print(mdatarais)

            mdata_extrais <- data.frame(
              data_class_id=1,
              data_freq_id=mmfreq,
              data_type_id=1,
              datasource_id=1,
              data_url=tttex
            )


            tabela <- tabela|>dplyr::rename(valor=ncol(tabela)-1)
            print(summary(tabela))

            db_datawrite(list(mdatarais,mdata_extrais),tabela,tttex)


          } else if (input$sourcetype == 13 ) {

            tabela <- eval(parse(text=input$upload_file))


            assign("mmfreq",9,envir=.GlobalEnv)

            ####DATAFILE WRITER

            readr::write_csv(tabela,narq)

            ###DB DATA WRITER



            tttex <- paste(names(tabela)[ncol(tabela)],
                           gsub(".*nivel='([^']*).*detalhe=='([^']*)'.*",
                                "- \\1 - \\2",input$upload_file))

            mdatainep<- data.frame(
              orig_name=input$nomefonte,
              data_name= tttex,
              data_desc="auto import inep- check source code"
            )

            mdata_extrinep <- data.frame(
              data_class_id=1,
              data_freq_id=mmfreq,
              data_type_id=1,
              datasource_id=1,
              data_url=input$upload_file
            )


            tabela <-
              tabela|>
              dplyr::rename(local=codigo_municipio,periodo=ano,
                            valor=ncol(tabela))

            print(summary(tabela))
            print(tttex)
            print(mdatainep)
            print(mdata_extrinep)

            db_datawrite(list(mdatainep,mdata_extrinep),tabela,tttex)


          }

    datapath=narq

    data.frame(name=nomea,size=file.size(narq),type=extensao,
               datapath=narq)

    })







  # parse selected files
  # selected_files <- shiny::reactive({
  #   shiny::req(input$upload_file)
  #   shinyFiles::parseFilePaths(volumes, input$upload_file)
  # })

  # shiny::observe({
  #   req(selected_files())
  #   print(selected_files())
  # })

  # load data
  selected_files_data <- shiny::reactive({
    shiny::req(selected_files())

    paths <- selected_files() |> dplyr::pull(datapath)
    print(paste0(paths,"CARREGA TABELA SALVA AQUI"))

    purrr::map(
      paths,
      rio::import, # TODO: customize import for excel tabs, etc.
      setclass = "tibble"
    ) |>
      purrr::set_names(fs::path_ext_remove(basename(paths)))
  })

  # extract dims
  selected_files_data_dims <- shiny::reactive({
    shiny::req(selected_files_data())

    data_list <- selected_files_data()

    num_rows <- purrr::map_dbl(data_list, nrow)
    num_cols <- purrr::map_dbl(data_list, ncol)

    list(rows = num_rows, cols = num_cols)

  })

  # pull details on files
  selected_files_info <- shiny::reactive({

    shiny::req(selected_files(), selected_files_data_dims())

    selected_files() |>
      dplyr::transmute(
        index = dplyr::row_number(),
        file = name,
        path = datapath,
        type = fs::path_ext(name),
        num_rows = selected_files_data_dims()$rows,
        num_cols = selected_files_data_dims()$cols,
        last_modified = as.Date.character(file.mtime(path)),
        size = paste0(prettyNum(size, big.mark = ".",decimal.mark = ",", digits = 2, format = "d"), " Bytes"),
        custom_name = fs::path_ext_remove(name),
        custom_desc = "Breve Descri\u00e7\u00e3o..."
      )
  })

  shiny::observe({
    shiny::req(selected_files_info())
    print(selected_files_info())
  })

  # output DT
  output$files_table <- DT::renderDT({
    shiny::req(selected_files_info())

    hold <- selected_files_info()

    DT::datatable(
      hold,
      options = list(
        keys = TRUE,
        dom = "Bt",
        buttons = list(
          'copy', 'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Baixar'
          )
        ),
        paging = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = c(0:9)
          )
        )
      ),
      class = "stripe cell-border",
      rownames = FALSE,
      colnames = c(
        "\u00cdndice",
        "Arquivo",
        "Local",
        "Tipo",
        "# Linhas",
        "# Colunas",
        "\u00daltima modifica\u00e7\u00e3o",
        "Tamanho",
        "Nome personalizado",
        "Descri\u00e7\u00e3o personalizada"
      ),
      caption = paste0("Resumo de arquivos de dados carregados:"),
      style = "bootstrap",
      extensions = c("Buttons", "KeyTable"),
      editable = list(
        target = 'row', disable = list(columns = c(0:7))
      )
    )

  })

  shiny::observe(str(input$files_table_cell_edit))

  metadata_writer <- observe( {
    dir.create(paste0("manipula/metadados"),showWarnings = F,recursive = T)
    shiny::req(selected_files_data(), input$data_picker,
               input$upload_file,input$nomefonte,input$filtraVars)
    if (shiny::is.reactive(selected_files_data) || shiny::is.reactive(input$data_picker) || shiny::is.reactive(input$upload_file) ||
        shiny::is.reactive(input$filtraVars)) {
      # extensao <- gsub(".*(\\.[^.]+)$","\\1",input$upload_file)
      extensao <- ".csv"
      nomea <- paste0(input$nomefonte,extensao)
    narq <- paste0('coleta/cache/',input$nomefonte,"/",nomea)

    vars_fonte <- paste0("manipula/metadados/",input$nomefonte,extensao)
    print(paste("atualiza metadados de ",narq))
    if (extensao == ".csv") {
      if (ncol(utils::read.csv(narq,nrows=10))==1) {
        if (ncol(utils::read.csv2(narq,nrows=10))==1) {
          if (ncol(utils::read.csv(narq,nrows=10,skip = 1))==1) {
            vfonte <- names(utils::read.csv2(narq,nrows = 10,skip=1))[-1]
          } else {
            vfonte <- names(utils::read.csv(narq,nrows = 10,skip=1))[-1]
          }

        } else {
          vfonte <- names(utils::read.csv2(narq,nrows = 10))[-1]
        }
      } else {
        vfonte <- names(utils::read.csv(narq,nrows = 10))[-1]
      }
      write(vfonte,vars_fonte)
    }
    filtro <- input$filtraVars
    if (length(filtro) > 0) {
      vfonte <-
    vfonte[
      grepl(
        x = vfonte,
        pattern = filtro,
        ignore.case = TRUE
      )
    ]
    }
    r(vfonte)
  }
    }
  )


  output$data_picker <- shiny::renderUI({
    shiny::req(selected_files_data())

    shinyWidgets::pickerInput(
      session$ns("data_picker"),
      label = "Selecione os Dados a Mostrar Abaixo:",
      choices = names(selected_files_data()),
      selected = names(selected_files_data())[1],
      width = "300px",
      # options = shinyWidgets::pickerOptions(
      #   style = "primary"
      # ),
      multiple = FALSE
    )

  })

  output$data_table <- DT::renderDT({

    shiny::req(selected_files_data(), input$data_picker)

    hold <- selected_files_data()[[match(input$data_picker, names(selected_files_data()))]]

    DT::datatable(
      hold,
      options = list(
        dom = 'lBftpr',
        buttons = list(
          'copy', 'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      ),
      class = "stripe cell-border",
      rownames = tibble::has_rownames(hold),
      caption = paste0("Vista Pr\u00e9via do  <- junto de Dados Carregado:"),
      style = "bootstrap",
      extensions = "Buttons"
    )
  })

  output$data_summary <- shiny::renderUI({
    req(selected_files_data(), input$data_picker)

    hold <- selected_files_data()[[match(input$data_picker, names(selected_files_data()))]]

    print(
      summarytools::dfSummary(
        hold, graph.magnif = 0.8
      ),
      method = 'render',
      headings = FALSE,
      justify = "c",
      trim.strings = TRUE,
      bootstrap.css = FALSE #,
      # width = 240
    )
  })


})
}


