empregoformal_agricola_municipal <- readr::read_csv("coleta/cache/empregoformal_agricola_municipal/empregoformal_agricola_municipal.csv")


empformagrmun <- \(ano){
  resultado <- DBI::dbGetQuery(rais,
                  paste0("select municipio,count(*) from rais_vinculo_",
                         ano, " WHERE cnae_2_0_classe < 2000 AND vinculo_ativo_31_12 = 1 GROUP BY municipio"))
  resultado$ano <- ano
  resultado
}

empagr24 <-
  empformagrmun(2024)

empagr23 <-
  empformagrmun(2023)

empagr13 <-
  empformagrmun(2013)

empregoformal_agricola_municipalc24 <-
  data.table::rbindlist(
    lapply(2013:2024,
    empformagrmun
  ))


readr::write_csv(empregoformal_agricola_municipalc24,'coleta/cache/empregoformal_agricola_municipal/empregoformal_agricola_municipal_2024parcial.csv')

agrmunmdid <- recupmdata_id('empregoformal_agricola_municipal')

empregoformal_agricola_municipalc24$mdata_id <- agrmunmdid

empregoformal_agricola_municipalc24 <-
  empregoformal_agricola_municipalc24|>
  dplyr::left_join(locgeoloc,by=c("municipio"="local"))|>
  dplyr::transmute(
    mdata_id,
    local_id,
    refdate=as.Date(paste0(ano,'-12-31')),
    value=count
  )

empregoformal_agricola_municipalc24 <-
  empregoformal_agricola_municipalc24[!is.na(empregoformal_agricola_municipalc24$local_id),]

dbx::dbxUpsert(upsertdb,'data_values',
               empregoformal_agricola_municipalc24,where_cols=c('mdata_id','refdate','local_id'))
