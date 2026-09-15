##educ1 - 2019 a 2020 - para correlação com série antiga - 2021 a 2024 - complementa


censoescolarmeta <- educabR::metainep|>dplyr::filter(grepl("Microdados da Educação Básica",tabela,fixed = F))

ce_1924 <- censoescolarmeta|>
  dplyr::filter(as.numeric(periodo)>2018)

ce_1924$nm_arqs1924 <- gsub(".*/([^/]*)$","\\1",ce_1924$tab_url)

cacheind <- 'coleta/cache/educ1_esgoto/'


baixaf <- \(x,y){
  download.file(x,paste0(cacheind,y),method='libcurl',extra = "--insecure")}

mapply(baixaf,ce_1924$tab_url,ce_1924$nm_arqs1924)


#for i in *.zip; do unzip -j  $i   "*.csv" -d . ; done
#CSV no arquivo 2020
#prename 's/CSV/csv/g' *.CSV

arqmicrocensos <- list.files(path = "coleta/cache/educ1_esgoto",
                    pattern = "microda.*.csv",
                    full.names = TRUE,
                    recursive = TRUE)

reading_data <- function(x){
  data <- data.table::fread(x, sep = ";", dec = ",", select = c("NU_ANO_CENSO", "CO_MUNICIPIO", "IN_ESGOTO_REDE_PUBLICA"),fill=TRUE) %>%
    dplyr::group_by(NU_ANO_CENSO, CO_MUNICIPIO) %>%
    dplyr::summarise(qtd_esgoto = sum(IN_ESGOTO_REDE_PUBLICA, na.rm = TRUE),
              n = sum(!is.na(IN_ESGOTO_REDE_PUBLICA))) %>%
    dplyr::transmute(NU_ANO_CENSO = NU_ANO_CENSO,
              CO_MUNICIPIO = CO_MUNICIPIO,
              value = qtd_esgoto/n*100,
              variavel = "educ1") %>%
    dplyr::ungroup() %>%
    dplyr::rename(codmun = CO_MUNICIPIO,
           ano = NU_ANO_CENSO)
  return(data)
}

educ1_aedi <- purrr::map_dfr(.x = arqmicrocensos, .f = reading_data) %>%
  dplyr::mutate(codmun = trunc(codmun/10)) %>%
  dplyr::select(ano, codmun, variavel, value) %>%
  dplyr::ungroup()

#data.table::fwrite(educ1_aedi,'coleta/cache/educ1_esgoto/educ1_aedi.csv')
educ1_orig <- DBI::dbGetQuery(mdr,"select
                                refdate,geoloc_id,value from data_values a left join local b ON a.local_id = b.local_id left join mdata c on a.mdata_id = c.mdata_id where orig_name LIKE 'educ1%'")

educ1_orig <- educ1_orig|>
  dplyr::transmute(ano=lubridate::year(refdate),
            codmun=trunc(geoloc_id/10),
            educ1_base=value)

comparaeduc1 <- educ1_aedi|>
#  dplyr::filter(ano %in% 2019:2022)|>
  dplyr::left_join(educ1_orig)

cor(comparaeduc1$educ1_base,comparaeduc1$value,use='complete.obs')
#1

summary(comparaeduc1|>dplyr::transmute(
  ano,local=codmun,educ1_base=value,
  educ1_aedi=educ1bd))
