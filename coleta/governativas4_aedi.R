#Calculo próprio
#governativas4_aedi <- readRDS("coleta/cache/governativas4_aedi/ifsm2015204.rds")

#Recalculo com método exato adaptado para siconfir - necessidade de criar colunas zeradas quando necessário
governativas4_aedi <- readRDS("coleta/cache/governativas4_aedi/ifsmparcial.rds")
#Conferência gov4
gov4aedic <- governativas4_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=codmun,
#                   gov4_aedi=ifsm)
                    gov4_aedi=value)
# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

governativas4_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'governativas4%'")

gov4_compara <- governativas4_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::left_join(gov4aedic)


cor(gov4_compara$value,gov4_compara$gov4_aedi,use='complete.obs')
#0.5509997 - correlação geral calculo simplificado

#0.9990894 - correlação geral recálculo exato

summary(gov4_compara)

# gov4_compara <-
#   gov4_compara[gov4_compara$gov4_aedi>0 & !is.na(gov4_compara$value) & !is.na(gov4_compara$gov4_aedi),]
#
# cor(gov4_compara$value,gov4_compara$gov4_aedi,use='complete.obs')
# #0.8070553 - quando não considerados 0 que, em parte, foram imputações
