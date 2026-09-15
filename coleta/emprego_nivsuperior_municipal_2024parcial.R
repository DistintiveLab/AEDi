pega_cursosuperior <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, escolaridade_apos_2005 > 8 nivel_superior, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1   GROUP BY municipio, escolaridade_apos_2005 > 8" )

  )
  a$ano <- ano
  a
}

empregos_fsuperior2024 <- pega_cursosuperior(2024)

emprego_nivsuperior_municipal <- data.table::fread("coleta/cache/emprego_nivsuperior_municipal/emprego_nivsuperior_municipal.csv")

emprego_nivsuperior_municipal <-
  dplyr::bind_rows(emprego_nivsuperior_municipal,
                   empregos_fsuperior2024|>
                     dplyr::filter(nivel_superior==TRUE)|>
                     dplyr::select(-nivel_superior)|>
                     dplyr::transmute(local,qtd_vinculos_agr,periodo=data.table::as.IDate(as.Date(paste0(ano,"-12-31")))))

emprego_nivsuperior_municipal <- emprego_nivsuperior_municipal[emprego_nivsuperior_municipal$local< 999999,]
readr::write_csv(emprego_nivsuperior_municipal,'coleta/cache/emprego_nivsuperior_municipal/emprego_nivsuperior_municipal_2024_p.csv')

emprego_formal_municipal <- data.table::fread("coleta/cache/emprego_formal_municipal/emprego_formal_municipal.csv")

emprego_fmun2024 <- empregos_fsuperior2024|>
  dplyr::group_by(local,ano)|>
  dplyr::summarize(value=sum(qtd_vinculos_agr,na.rm=TRUE))


emprego_fmun2024 <- emprego_fmun2024[emprego_fmun2024$local!= 999999,]

efm2024 <- emprego_fmun2024|>
  dplyr::left_join(locgeoloc)

efm2024 <- efm2024|>dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=92,
    local_id,
    refdate=data.table::as.IDate(as.Date(paste0(ano,'-12-31'))),
    value
  )
emprego_formal_municipal <-
  emprego_formal_municipal|>
  dplyr::bind_rows(
efm2024
  )

readr::write_csv(emprego_formal_municipal,'coleta/cache/emprego_formal_municipal/emprego_formal_municipal_2024parcial.csv')

upsertdb <- dbx::dbxConnect(
  adapter="postgres",
  host = Sys.getenv('host'),
  dbname=Sys.getenv('dbname'),
  user=Sys.getenv('user'),
  password=Sys.getenv('password'),
)



##necessário código de upsert.R
dbx::dbxUpsert(upsertdb,'data_values',
               efm2024,where_cols=c('mdata_id','refdate','local_id'))

##necessário código de upsert.R
mdata_id_ensm <- recupmdata_id('emprego_nivsuperior_municipal')

ensm2024 <- empregos_fsuperior2024|>
  dplyr::filter(nivel_superior==TRUE)|>
  dplyr::select(-nivel_superior)|>
  dplyr::left_join(locgeoloc)|>
  dplyr::transmute(mdata_id=mdata_id_ensm,local_id,refdate=data.table::as.IDate(as.Date(paste0(ano,"-12-31"))),
                   value=qtd_vinculos_agr)

ensm2024 <-
  ensm2024[!is.na(ensm2024$local_id),]

dbx::dbxUpsert(upsertdb,'data_values',
               ensm2024,where_cols=c('mdata_id','refdate','local_id'))
