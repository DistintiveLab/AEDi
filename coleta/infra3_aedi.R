
datasus_20152025 <- microdatasus::fetch_datasus(2015,1,2024,12,information_system = 'SIH-RD',
                            vars = c('UF_ZI','ANO_CMPT','MUNIC_RES','DIAG_PRINC','MUNIC_MOV'))



arqssih <- list.files('coleta/cache/infra3_aedi2',pattern='*.dbc',recursive = T,full.names = T)

aih_interesse <- c("A09",
                   "A25", "B15", "A90", "A95", "B55", "B74", "B50", "B57", "A27",
                   "B65", "Z135", "H543", "H10", "B08", "B36", "B820", "B839")

le_sih_drsai <- \(x) {
  read.dbc::read.dbc(x)|>dplyr::select(ANO_CMPT,MUNIC_MOV,DIAG_PRINC)|>
    dplyr::filter(grepl(paste0("^(",paste0(aih_interesse,collapse="|"),")"),DIAG_PRINC))|>
    dplyr::group_by(ANO_CMPT,MUNIC_MOV)|>
    dplyr::summarise(internacoes = dplyr::n())|>
    dplyr::ungroup()
}

library(parallel)
library(doParallel)
cl <- makeCluster(8,type='FORK')
registerDoParallel(cl,8)
infra3_aedi_2015_16 <- data.table::rbindlist(
  parLapply(cl,arqssih[grepl("/201[56]",arqssih)],le_sih_drsai)
)

infra3_aedi_2017_19 <- data.table::rbindlist(
  parLapply(cl,arqssih[grepl("/201[79]",arqssih)],le_sih_drsai)
)

infra3_aedi_2018_20_21_22 <- data.table::rbindlist(
  parLapply(cl,arqssih[grepl("/20[12][8012]",arqssih)],le_sih_drsai)
)

infra3_aedi_2023_24 <- data.table::rbindlist(
  parLapply(cl,arqssih[grepl("/202[34]",arqssih)],le_sih_drsai)
)

infra3_aedi_2013_14 <- data.table::rbindlist(
  parLapply(cl,arqssih[grepl("/201[34]",arqssih)],le_sih_drsai)
)

infra3_aedi_b <- data.table::rbindlist(
  list(infra3_aedi_2013_14,
    infra3_aedi_2015_16,
       infra3_aedi_2017_19,
       infra3_aedi_2018_20_21_22,
       infra3_aedi_2023_24)
)

infra3_aedi_b<- infra3_aedi_b|>
  dplyr::group_by(ANO_CMPT,MUNIC_MOV)|>
  dplyr::summarize(internacoes=sum(internacoes,na.rm=T))

infra3_aedi_b<- infra3_aedi_b|>dplyr::ungroup()

infra3_aedi<- infra3_aedi|>
  dplyr::group_by(ANO_CMPT,MUNIC_MOV)|>
  dplyr::summarize(internacoes=sum(internacoes,na.rm=T))

popmun <- dbGetQuery(con,"select refdate,trunc(geoloc_id/10) MUNIC_MOV,value from
                     data_values a left join mdata b on a.mdata_id = b.mdata_id left join
                     local c on a.local_id = c.local_id where orig_name like 'datasus_pop%'  and extract('year' from refdate)>2014")




infra3_aedi_r <- infra3_aedi_b|>
  janitor::clean_names()|>
  dplyr::transmute(refdate=as.Date(paste0(as.character(ano_cmpt),"-12-31")),local=as.numeric(as.character(munic_mov)),internacoes)|>
  dplyr::left_join(popmunicipal|>dplyr::mutate(refdate=as.Date(paste0(lubridate::year(refdate),"-12-31"))))

infra3_aedi_r <-
  infra3_aedi_r|>
  dplyr::transmute(refdate,local,infra3_aedi = 10000*ifelse(is.na(internacoes),0,internacoes)/populacao)


#Conferência
# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))

infra3_orig <- dbGetQuery(mdr,"select refdate,trunc(geoloc_id/10) MUNIC_MOV,value from
                     data_values a left join mdata b on a.mdata_id = b.mdata_id left join
                     local c on a.local_id = c.local_id where orig_name like 'infra3%'")

infra3_compara <- infra3_orig|>
  dplyr::rename(local=munic_mov)|>
  dplyr::left_join(infra3_aedi_r)

infra3_compara[is.na(infra3_compara)] <- 0

cor(infra3_compara$value,infra3_compara$infra3_aedi,use='complete.obs')
#0.9448734

summary(infra3_compara|>dplyr::transmute(refdate,local,infra3_base=value,infra3_aedi))
readr::write_csv(infra3_aedi_r,"coleta/cache/infra3_aedi2/infra3_aedi_2013_2024.csv")
