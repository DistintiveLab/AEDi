###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('rais_media_remdez_s38','rais_mediana_remmed_s38')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- objetivo1_1_via_aedis <- dbdbase |>
                     dplyr::rename(setNames(c('rais_media_remdez_s38','rais_mediana_remmed_s38'), c('a','b'))) |>
                dplyr::transmute(objetivo1_1_via_aedis = a - b,refdate,local_id)

#Conferência
mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      dbname=Sys.getenv("tdbname"),
                      user=Sys.getenv("userdb"),
                      password=Sys.getenv("passwddbdev"),
                      host=Sys.getenv("hostdbdev"))

#locgeoloc <- dbGetQuery(mdr,'select * from local')

obj1_1_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value obj1_1_base from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo1_1%'")

obj1_1_compara <-
  obj1_1_orig|>
  dplyr::left_join(
    objetivo1_1_via_aedis)

cor(obj1_1_compara$obj1_1_base,obj1_1_compara$objetivo1_1_via_aedis,use="complete.obs")
#0.999971

