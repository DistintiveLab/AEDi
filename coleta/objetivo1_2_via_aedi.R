###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('ideb_media_basico_redep','ideb_basico_rede_mediana')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>

      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- objetivo1_2_via_aedi <- dbdbase |>
                     dplyr::rename(setNames(c('ideb_media_basico_redep','ideb_basico_rede_mediana'), c('a','b'))) |>
                dplyr::transmute(objetivo1_2_via_aedi = a - b,refdate,local_id)

#readr::write_csv(dbdbase,'coleta/cache/objetivo1_2_via_aedi/objetivo1_2_via_aedi.csv')
###Conferência

# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))
#
# locgeoloc <- dbGetQuery(mdr,'select * from local')
#
# objetivo1_2_orig <- dbGetQuery(mdr,
#                                "select refdate,local_id,value from data_values a
#                                left join mdata b on a.mdata_id = b.mdata_id where
#                                orig_name like 'objetivo1_2%'")
#
#
# obj1_2_comp <-
#   objetivo1_2_orig|>
#   dplyr::left_join(
#     dbdbase)
#
# cor(obj1_2_comp$value,obj1_2_comp$objetivo1_2_via_aedi,use='complete.obs')
#0.9958384
