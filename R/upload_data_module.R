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
#' @importFrom shiny NS fluidRow column icon uiOutput selectInput textInput
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box
#' @importFrom shinyFiles shinyFilesButton shinyDirButton shinySaveButton
#' @importFrom shinyjs disabled
#' @importFrom sortable bucket_list rank_list add_rank_list
#' @importFrom utils read.csv read.csv2 download.file str
upload_data_ui <- function(id) {

  ns <- shiny::NS(id)

  shinydashboard::tabBox(
    id = ns("data_upload"),
    title = icon_text("cloud-upload", "Carregue e Explore Dados"), #"procure", "Vista pr\u00e9via:"),
    width = 12,

    shiny::tabPanel(
      title = icon_text("file", "Inser\u00e7\u00c3o de Fonte"),
      width = 12,
      flucol(
        shiny::div(
          style = "inline; float:left",
          shiny::selectInput(
            inputId = ns("sourcetype"),
            label = "Tipo de Fonte",
            choices = c(
              "url fixo"=1,
              "arquivo local"=2,
              "dados.gov.br"=3,
              "ckan"=4,
              "ibge"=5,
              "ipeadata"=6,
              "bcb"=7,
              "arquivo do servidor" = 8,
              "url de pasta ou combinada" = 9),
            selected=4),
          shiny::textInput(ns("nomefonte"),"Nome curto para fonte","nova_fonte",width="100px","Indique um nome para a fonte"),
          shinybusy::add_busy_spinner(uiOutput(ns("cargatipo")),spin="double-bounce"),
          shinyFiles::shinySaveButton(
            id = ns("save_file"),
            label = "Salvar para Arquivo",
            title = "Selecione um arquivo para salvar:",
            # buttonType = "primary",
            icon = shiny::icon(
              "save"
            )
          )%>% shinyjs::disabled()
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
      title = icon_text("list", "Vari\u00e0veis e Indicadores"),
      fluidRow(
        column(5,shiny::textInput(
        inputId = ns("filtraVars"),
        label = "Insira texto para filtrar vari\u00e0veis de interesse",
        value = "ano"
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
      flucol(
        shinymath::mathInput(ns("equacao"),"Insira equa\u00e7\u00c3o"),
        shiny::actionButton(ns("previaindicador"),"Rodar!"),
        shiny::verbatimTextOutput(ns("text_r"), placeholder = TRUE),
        "Trabalho em Andamento"

      ),
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

}

#' Upload Data Module - Server
#'
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
#' @importFrom shinymath latex2r
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom summarytools dfSummary
upload_data <- function(input, output, session) {

  # namespace
  ns <- session$ns

    r <- reactiveVal(if(dir.exists("manipula/metadados")){
    sort(unique(unlist(
      lapply(list.files("manipula/metadados",pattern="*.csv",full.names =T),
             \(x){base::read.csv(x)[[1]]}))))
  })


    fvars <-  observe({
      filtro <- input$filtraVars
      vfonte <- sort(unique(unlist(
        lapply(list.files("manipula/metadados",pattern="*.csv",full.names =T),
               \(x){base::read.csv(x)[[1]]}))))

      r(vfonte)
    if (nchar(filtro) > 0) {
      vfonte <-
        vfonte[
          grepl(
            x = vfonte,
            pattern = input$filtraVars,
            ignore.case = TRUE
          )
        ]
    } else {
      vfonte <- vfonte
    }
      r(vfonte)
    })

  output$mapeiavars <- renderUI({
      column(
        width = 5,
        sortable::bucket_list(
          header = "seleciones as vari\u00e0veis",
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

            tags$p("Vari\u00e0vel A"),
            shiny::verbatimTextOutput(ns("results_1")),

            tags$p("Vari\u00e0vel B"),
            shiny::verbatimTextOutput(ns("results_2")),

            tags$p("Vari\u00e0vel C"),
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
    renderPrint({
      input$varA

#      debugei() # This matches the input_id of the first rank list
    }
    )
  #})


  output$results_2 <-
    renderPrint(
      input$varB # This matches the input_id of the second rank list
    )
  output$results_3 <-
    renderPrint(
      input$varC # Matches the group_name of the bucket list
      # print(reactiveValuesToList(input))
    )

  # volumes for shinyFiles inputs
  volumes <- c(
    'Demo Data' = fs::path_package("AEDi", "extdata"),
    'Home' = fs::path_home(),
    'Documents' = fs::path(fs::path_home(), "Documents"),
    shinyFiles::getVolumes()()
  )


  # observers for each button
 # shinyFiles::shinyFileChoose(input, "upload_file", session = session, roots = volumes)
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
  } else if (input$sourcetype == "8") {
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
  }

})

  output$cargatipo <- renderUI({
    tipocarga()

  })

  ##Math insira equação
  math = eventReactive(input$previaindicador, input$equacao)

  output$text_r <-  shiny::renderText({
    nfonte <- input$nomefonte
    nind <- input$nome_indicador
    baseq <- shinymath::latex2r(math())
    fltro <- ifelse(is.null(input$varFilter),"",input$varFilter)
    fltrov <- input$valueFilter
    grpo <- input$varGroup
    somasna <- \(x){sum(x,na.rm=T)}
    prepara <- nfonte
    if(length(prepara)>0) {
    if(length(grpo)>0){
      prepara <- paste0(prepara,"|> group_by(",paste0(grpo,collapse=","),")|>
                        summarize(across(where(is.numeric),somasna),across(where(is.character),first))|>
                        group_by(",grpo,")")
    }
    if (length(fltro)>0 & length(fltrov)>0){
      prepara <- paste0(prepara,"|> filter(",fltro,fltrov,")")
    }
    vex <- c(length(input$varA)>0,
             length(input$varB)>0 ,
             length(input$varC)>0,
             length(input$varD)>0)
    varsval <- letters[1:4][vex]
    valvars <- c(input$varA,input$varB,input$varC,input$varD)[vex]
    names(valvars) <- varsval
    print(valvars)
    codigo <- paste0(nind," <- ",prepara," |>
                     rename(setNames(c('",paste0(valvars,collapse="','"),
                "'), c('",paste0(varsval,collapse="','"),"'))) |>
                transmute(",nind," = ",baseq,")")
    codigo
    } else {baseq}

    })


  selected_files <- reactive({
    shiny::req(input$upload_file,input$nomefonte)
    dir.create(paste0("coleta/dados/",input$nomefonte),showWarnings = F,recursive = T)
    extensao <- gsub(".*(\\.[^.]+)$","\\1",input$upload_file)
    vars_fonte <- paste0("manipula/metadados/",input$nomefonte,extensao)
    nomea <- paste0(input$nomefonte,extensao)

    narq <- paste0('coleta/dados/',input$nomefonte,"/",nomea)
    if (input$sourcetype == 2 ) {


      readr::write_csv(readr::read_csv(nomea),narq)
      paste0(getwd(),narq)
    } else
      if (input$sourcetype == 1 ) {

      download.file(input$upload_file,narq,method="wget")
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

  shiny::observe({
    req(selected_files())
    print(selected_files())
  })

  # load data
  selected_files_data <- shiny::reactive({
    shiny::req(selected_files())

    paths <- selected_files() %>% dplyr::pull(datapath)
    print(paths)

    purrr::map(
      paths,
      rio::import, # TODO: customize import for excel tabs, etc.
      setclass = "tibble"
    ) %>%
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

    selected_files() %>%
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
        custom_desc = "Breve Descri\u00e7\u00c3o..."
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
        "\u00daltima modifica\u00e7\u00c3o",
        "Tamanho",
        "Nome personalizado",
        "Descri\u00e7\u00c3o personalizada"
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
    shiny::req(selected_files_data(), input$data_picker,
               input$upload_file,input$nomefonte,input$filtraVars)
    if (is.reactive(selected_files_data) || is.reactive(input$data_picker) || is.reactive(input$upload_file) ||
        is.reactive(input$filtraVars)) {
      extensao <- gsub(".*(\\.[^.]+)$","\\1",input$upload_file)
      nomea <- paste0(input$nomefonte,extensao)
    narq <- paste0('coleta/dados/',input$nomefonte,"/",nomea)

    vars_fonte <- paste0("manipula/metadados/",input$nomefonte,extensao)
    print(paste("atualiza metadados de ",narq))
    if (extensao == ".csv") {
      if (ncol(base::read.csv(narq,nrows=10))==1) {
        if (ncol(base::read.csv2(narq,nrows=10))==1) {
          if (ncol(base::read.csv(narq,nrows=10,skip = 1))==1) {
            vfonte <- names(base::read.csv2(narq,nrows = 10,skip=1))[-1]
          } else {
            vfonte <- names(base::read.csv(narq,nrows = 10,skip=1))[-1]
          }

        } else {
          vfonte <- names(base::read.csv2(narq,nrows = 10))[-1]
        }
      } else {
        vfonte <- names(base::read.csv(narq,nrows = 10))[-1]
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
      caption = paste0("Vista Pr\u00e9via do Conjunto de Dados Carregado:"),
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

}


