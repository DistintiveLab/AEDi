##educ3 - 2019 a 2020 - para correlação com série antiga - 2021 a 2023 - complementa


censoescolarmeta <- educabR::metainep|>dplyr::filter(grepl("micro.*superior_\\d{4}",tab_url,fixed = F))

ce_1923 <- censoescolarmeta|>
  dplyr::filter(as.numeric(periodo)>2018)


ce_1923 <- rbind(ce_1923,
                 apply(ce_1923[1,],2,\(x){gsub("2022","2023",x)}),
                 apply(ce_1923[1,],2,\(x){gsub("2022","2021",x)}))

ce_1923$nm_arqs1923 <- gsub(".*/([^/]*)$","\\1",ce_1923$tab_url)

cacheind <- 'coleta/cache/educ3_educprof/'

baixaf <- \(x,y){
  download.file(x,paste0(cacheind,y),method='libcurl',extra = "--insecure")}

mapply(baixaf,ce_1924$tab_url,ce_1924$nm_arqs1924)


#for i in *.zip; do unzip -j  $i   "*.csv" -d . ; done
#CSV no arquivo 2020
#prename 's/CSV/csv/g' *.CSV


