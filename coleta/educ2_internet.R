##educ2 - 2019 a 2020 - para correlação com série antiga - 2021 a 2024 - complementa


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

reading_educ2_aedi <- function(x){
  data <- data.table::fread(x, sep = ";", dec = ",", select = c("NU_ANO_CENSO", "CO_MUNICIPIO", "IN_INTERNET"),fill=TRUE) %>%
    dplyr::group_by(NU_ANO_CENSO, CO_MUNICIPIO) %>%
    dplyr::summarise(qtd_internet = sum(IN_INTERNET, na.rm = TRUE),
              n = sum(!is.na(IN_INTERNET))) %>%
    dplyr::transmute(NU_ANO_CENSO = NU_ANO_CENSO,
              CO_MUNICIPIO = CO_MUNICIPIO,
              value = qtd_internet/n*100,
              variavel = "educ2") %>%
    dplyr::ungroup() %>%
    dplyr::rename(codmun = CO_MUNICIPIO,
           ano = NU_ANO_CENSO)
  return(data)
}

educ2_aedi <- purrr::map_dfr(.x = arqmicrocensos, .f = reading_data_internet) %>%
  dplyr::mutate(codmun = trunc(codmun/10))

#data.table::fwrite(educ2_aedi|>dplyr::left_join(locgeoloc|>dplyr::mutate(codmun=trunc(geoloc_id/10)))|>dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),local_id,educ2=value),'coleta/cache/educ2_aedi/educ2_aedi.csv')

educ2_orig <- DBI::dbGetQuery(mdr,"select
                                refdate,geoloc_id,value from data_values a left join local b ON a.local_id = b.local_id left join mdata c on a.mdata_id = c.mdata_id where orig_name LIKE 'educ2%'")
educ2_orig <- educ2_orig|>
  dplyr::transmute(ano=lubridate::year(refdate),
            codmun=trunc(geoloc_id/10),
            educ2_base=value)

comparaeduc2 <- educ2_aedi|>
  dplyr::filter(ano >2014)|>
  dplyr::left_join(educ2_orig)



cor(comparaeduc2$educ2_base,comparaeduc2$value,use="complete.obs")
#0.99992
