indic_sentido <- readr::read_csv("inst/extdata/indicadores_sentido.csv")

## +1 quando 'maior melhor', para que o número maior fique como ranking mais alto
indic_sentido <-
  indic_sentido|>dplyr::mutate(
    orig_name=ifelse(grepl("obj",orig_name),
                     paste0(orig_name,"_",n_indicador),
                     paste0(orig_name,n_indicador)))



ranqueia <- \(x) {
  rank(x,ties.method="min",na.last=TRUE)
}

indseixo <- \(eixo){
  consulta <-
    DBI::dbGetQuery(mdr,
                    paste0("select refdate,local_id,orig_name,value from data_values a left join mdata b on
                         a.mdata_id = b.mdata_id where orig_name like '",
                           eixo,"%'"))
  consulta
}

rankings_individuais <- \(indicadores_grupo) {
  baseind <- indicadores_grupo|>
    dplyr::left_join(indic_sentido|>dplyr::select(orig_name,maior_melhor))|>
    dplyr::group_by(refdate,orig_name)|>
    dplyr::mutate(value=ranqueia(value*maior_melhor))

  maxano <- lubridate::year(max(baseind$refdate))
  for (i in unique(baseind$orig_name)){
    maxanoind <- lubridate::year(max(baseind[baseind$orig_name==i,]$refdate))
    if(maxanoind<maxano){
      baseind_comp <-
      data.table::rbindlist(
          lapply((maxanoind+1):maxano,\(x) baseind[baseind$orig_name==i & lubridate::year(baseind$refdate)==maxanoind,]|>
                 dplyr::mutate(refdate=as.Date(paste0(x,"-12-31"))))
      )
      baseind <- rbind(baseind,baseind_comp)

    }
  }
  baseind

}


indicador_composto <- \(tabelarankings){
  tabelarankings|>
    dplyr::group_by(refdate,local_id)|>
    dplyr::summarize(indsmedia=mean(value))|>
    dplyr::mutate(minimo=min(indsmedia),maximo=max(indsmedia),
                  normalizado=(indsmedia-minimo)/(maximo-minimo))
}

obj1_inds_ind <- indseixo('objetivo1')
obj1_ri <- rankings_individuais(obj1_inds_ind)
obj1_composto <- indicador_composto(obj1_ri)

comp_obj1orig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_objetivo1'")

cobj1_compara <-
  comp_obj1orig|>
  dplyr::left_join(obj1_composto|>dplyr::select(refdate,local_id,normalizado))

cor(cobj1_compara$value,cobj1_compara$normalizado,use='complete.obs')
#[1] 0.9912002
summary(cobj1_compara)

readr::write_csv(obj1_composto|>dplyr::filter(refdate>"2012-12-31"),'coleta/cache/objetivo1_composto_aedi/2013_2024_objetivo1_composto_aedi.csv')
