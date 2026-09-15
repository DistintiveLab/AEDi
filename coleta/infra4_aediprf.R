library(rvest)
linkprf <- "https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-da-prf"

paglistaprf <-
  rvest::read_html(linkprf)
#//*[@id="parent-fieldname-text"]/table[2]
nomestabslistaprf <-
  rvest::html_table(paglistaprf)[[2]]$X1

urlsprf <-
  rvest::html_elements(paglistaprf,xpath = '//*[@id="parent-fieldname-text"]/table[2]/tbody/tr/td')

urlsprf <- urlsprf[seq(2,98,by=2)]|>html_element('a')|>html_attr('href')

tabelaprf <-
  data.frame(
    nome_tabela = nomestabslistaprf[-1],
    link_tabela = urlsprf[-1]
  )

tabelaprf$link_tabela <-
  gsub("https://drive.google.com/file/d/(.*)/view\\?usp=sharing/download","https://drive.usercontent.google.com/u/0/uc?id=\\1&export=download",
       tabelaprf$link_tabela)

tabelaprf <-
  tabelaprf|>
  dplyr::filter(grepl('ocorrência',nome_tabela))
#https://drive.usercontent.google.com/u/0/uc?id=1-PJGRbfSe7PVjU37A3wTCls_NRXyVGRD&export=download
f <- tempfile(fileext = "zip")
download.file(tabelaprf$link_tabela[2],f)
unzip(f,exdir='coleta/cache/infra4_aedi2/')

acidentes24 <-
  readr::read_csv2('coleta/cache/infra4_aedi2/datatran2024.csv')

acidentes24 <-
  acidentes24|>
  dplyr::group_by(lubridate::year(data_inversa),municipio)|>
  count()
