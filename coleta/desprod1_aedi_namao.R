
eci_atualizado <- readxl::read_xlsx("coleta/cache/dataviva_atualizacao/dataviva_atualizacao.xlsx")

eci_atualizado <-
  eci_atualizado|>dplyr::rename(codmun=`ID IBGE Municípios`)|>
  tidyr::pivot_longer(-codmun,names_to="refdate",values_to="eci_cedeplar_novo")|>
    dplyr::mutate(refdate=as.Date(paste0(refdate,"-12-31")))

  mdr <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname=Sys.getenv("tdbname"),
                 user=Sys.getenv("userdb"),
                 password=Sys.getenv("passwddbdev"),
                 host=Sys.getenv("hostdbdev"))

desprod1_orig <- dbGetQuery(mdr,
                            "select refdate,a.local_id,geoloc_id codmun,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id left join
                               local c on a.local_id = c.local_id where
                               orig_name like 'desprod1%'")

desprod1_compara <-
  desprod1_orig|>
  dplyr::left_join(eci_atualizado)

cor(desprod1_compara$value,desprod1_compara$eci_cedeplar_novo,use="complete.obs")
#[1] 0.9992217
summary(desprod1_compara|>dplyr::filter(refdate>'2018-12-31'))
