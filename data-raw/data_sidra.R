#available metadata sidra
library(rvest)

agregados <- tibble::tribble(~id, ~agregado,
                             "A","Assunto",
                             "C","Classificação",
                             "N","Nível geográfico",
                             "P","Período",
                             "E","Periodicidade",
                             "V","Variável"
)

agregacao <- \(x="A"){
  baseag <- "https://servicodados.ibge.gov.br/api/v3/agregados"
  resp <- httr::GET(paste0(baseag,"?acervo=",x))
  conteudo <- httr::content(resp)
  agregado <- data.table::rbindlist(lapply(1:length(conteudo),\(x) as.data.frame(conteudo[[x]])))
  agregado$agregacao <- x
  agregado
}

lista_agregados <- lapply(agregados$id,agregacao)
names(lista_agregados) <- agregados$agregado
sidrameta <- data.table::rbindlist(lista_agregados)

dir.create("inst/extdata",showWarnings = F)

readr::write_csv(sidrameta, "inst/extdata/sidrameta.csv")


usethis::use_data(sidrameta, overwrite = TRUE)
