####Pega infos para compor buscas


#input n. tabela
#"https://sidra.ibge.gov.br/t/8694/n1/all/n3/all/v/7167,7168/p/all/c11046/all/d/v7167%205,v7168%205"

#Função metadados de tabela
desctabela <- \(tabela) {
  print(paste("Busca Tabela",tabela))
  requireNamespace("httr")
  requireNamespace("rvest")

  ##extração de vars e class
  extvc <- \(x,partag="p",hier="following-sibling::",ntab=1){
    rvest::html_table(
      rvest::html_elements(conteudo,xpath=paste0("//",partag,"[contains(span , '/",x,"')]/",hier,"table"))[[ntab]]
    )|>dplyr::mutate_all(\(x){gsub("\\r\\n","",x)})|>
      tidyr::separate_wider_regex(X1,patterns= c(codigo="[^:blank:]+","[:blank:]+",valor=".*"))
  }

  resp <- httr::GET(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=",
                           tabela))
  conteudo <- httr::content(resp)

  #Períodos
  periodos <- gsub("\\(.*\\):","",rvest::html_text(
    rvest::html_elements(conteudo,xpath="//p[contains(span , '/P')]/span")[-1]
  ))
  periodos <- data.frame(
    unidade = periodos[1],
    periodo = as.numeric(strsplit(periodos[2],",")[[1]])
  )
  #variaveis
  variaveis <- extvc("V")

  #classificadores

  qtdclass <- length(rvest::html_elements(conteudo,xpath="//td[contains(span , '/C')]/table"))

  extclassc <- \(nclass) {
    classificadores <- extvc("C","td","",nclass)

    classificadores$codclass <- rvest::html_text(
      rvest::html_elements(conteudo,xpath=paste0("//span[contains(@id, 'lblIdClassificacao')][1]"))[[nclass]]
    )

    classificadores$classificador <- rvest::html_text(
      rvest::html_elements(conteudo,xpath=paste0("//span[contains(@id, 'lblClassificacao')][1]"))[[nclass]]
    )
    classificadores
  }

  if (qtdclass==0) {
    classificadores <- list()
  } else {
  classificadores <- dplyr::bind_rows(lapply(1:qtdclass,extclassc))
  }

  #Niveis Territoriais
  "https://apisidra.ibge.gov.br/LisUnitTabAPI.aspx?c=8694&n=1&i=P"

  ##Recupera níveis territoriais disponíveis
  nivdisp <-
    html_text(html_elements(conteudo,xpath="//table/tr[./td/span[ text()='/N']]/td/span[ text()='/N']/following-sibling::span[1]"))

  territoriosdisp <- \(nter) {
    territs <- rvest::html_table(
      rvest::html_elements(
        rvest::read_html(
          paste0("https://apisidra.ibge.gov.br/LisUnitTabAPI.aspx?c=",
                 tabela,"&n=",nter,"&i=P")),
        xpath="//table")[[2]]
    )
    territs$nivel_territorial <- nter
    territs
  }


  niveis_territoriais <- dplyr::bind_rows(lapply(nivdisp,territoriosdisp))

  resp <- list('tabela'=tabela,props=list(periodos,classificadores,niveis_territoriais))
  saveRDS(resp,paste0("data-raw/sidraumaum/",tabela,".rds"))
  Sys.sleep(1)
  resp
}


