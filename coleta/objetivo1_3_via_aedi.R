###Filtra e prepara dados base
 dbdbase <- DBI::dbGetQuery(con,"SELECT * from named_datavalues WHERE orig_name IN ('profissionais_de_saude_pc','prof_saude_pc_mediana_nacional')")
dbdbase <- dbdbase|>
    dplyr::mutate(data_freq_id=max(data_freq_id))|>
      tidyr::pivot_wider(names_from='orig_name',values_from = 'value', values_fill = list(datasus_cnes_prid02br_mun=0))
###Cria indicador
dbdbase <- objetivo1_3_via_aedi <- dbdbase |>
                     dplyr::rename(setNames(c('profissionais_de_saude_pc','prof_saude_pc_mediana_nacional'), c('a','b'))) |>
                dplyr::transmute(objetivo1_3_via_aedi = a - b,refdate,local_id)

readr::write_csv(dbdbase,'coleta/cache/objetivo1_3_via_aedi/objetivo1_3_via_aedi.csv')
###Conferência

# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))
#
#
# objetivo1_3_orig <- dbGetQuery(mdr,
#                                "select refdate,local_id,value from data_values a
#                                left join mdata b on a.mdata_id = b.mdata_id where
#                                orig_name like 'objetivo1_3%'")
#
# lubridate::month(dbdbase$refdate) <- 12
# lubridate::day(dbdbase$refdate) <- 31
# obj1_3_comp <-
#   objetivo1_3_orig|>
#   dplyr::left_join(
#     dbdbase)
#
 # cor(obj1_3_comp$value,obj1_3_comp$objetivo1_3_via_aedi,use='complete.obs')
#0.5176143


###novamente pos conversa pedro
#medicos
medicos <-
  data.table::rbindlist(
    lapply(2010:2024,\(x){
      d <- datasus::cnes_proc02br_mun(periodo = paste0('Dez/',x),ocupacoes_medicos=1:72)
      d$refdate <- as.Date(paste0(x,"-07-01"))
      d})
  )

medicos <-
  medicos|>tidyr::separate_wider_delim(Município,delim=" ",names = c('local','municipio'),too_few = 'align_end',too_many = 'merge')

medicos <-
  medicos|>dplyr::filter(!is.na(local))|>dplyr::mutate(local=as.numeric(local))|>
  dplyr::left_join(locgeoloc)|>dplyr::left_join(datasus_popmun)

saveRDS(medicos,"coleta/cache/datasus_profsaude/medicos_ocupacoes_p_mun_2010_2024.rds")

medicos <-
  medicos|>
  dplyr::mutate(medicos_pc=Total/value)|>
  dplyr::group_by(refdate)|>
  dplyr::mutate(medianageral=median(medicos_pc))



medicos <-
  medicos|>
  dplyr::mutate(objet13med=medicos_pc-medianageral)

lubridate::month(medicos$refdate) <- 12
lubridate::day(medicos$refdate) <- 31

obj1_3 <- medicos|>dplyr::select(refdate,local_id,objet13med)

obj13_compara3 <-
  objetivo1_3_orig|>
  dplyr::left_join(medicos|>dplyr::select(refdate,local_id,objet13med))|>
  dplyr::transmute(refdate,local_id,obj1_3_base=value,obj1_3_aedi=objet13med)

cor(obj13_compara3$obj1_3_base,obj13_compara3$obj1_3_aedi,use="complete.obs")
#0.9998693
#0.9936311
##Com ocupacões baixadas tabnet
medicos <- data.table::fread("coleta/cache/datasus_cnes_proc02br_mun/cnes_cnv_proc02br205355179_48_47_38.csv",nrows=5571)

names(medicos) <-
  c('cd_ibge',
    paste0(2013:2021,'-07-01'))

medicos$cd_ibge <- as.numeric(substr(medicos$cd_ibge,1,6))

medicos <- medicos|>dplyr::mutate(across(-cd_ibge,\(x){ifelse(is.na(as.numeric(x)),0,as.numeric(x))}))|>
  tidyr::pivot_longer(-1,names_to='refdate',values_to='n_medicos')

medicos$refdate <- as.Date(medicos$refdate)

medicos <- medicos|>
  dplyr::filter(!is.na(cd_ibge))|>
  dplyr::left_join(locgeoloc,by=c('cd_ibge'='local'))|>dplyr::left_join(datasus_popmun)

medicos <-
  medicos|>
  dplyr::mutate(medicos_pc=n_medicos/value,
                medianageral=median(medicos_pc),
                obj13_aedi4=medicos_pc-medianageral)

lubridate::month(medicos$refdate) <- 12
lubridate::day(medicos$refdate) <- 31

obj13_compara4 <-
  objetivo1_3_orig|>
  dplyr::left_join(medicos|>dplyr::select(refdate,local_id,obj13_aedi4))|>
  dplyr::transmute(refdate,local_id,obj1_3_base=value,obj1_3_aedi=obj13_aedi4)

cor(obj13_compara4$obj1_3_base,obj13_compara4$obj1_3_aedi,use="complete.obs")
#0.9998085
