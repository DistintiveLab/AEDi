##necessita comp_obj1_aedi primeiro (definição de funções para cálculo do ranking)

obj2_inds_ind <- indseixo('objetivo2')
lubridate::month(obj2_inds_ind$refdate) <- 12
lubridate::day(obj2_inds_ind$refdate) <- 31
obj2_ri <- rankings_individuais(obj2_inds_ind)
obj2_composto <- indicador_composto(obj2_ri)

comp_obj2orig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_objetivo2'")

cobj2_compara <-
  comp_obj2orig|>
  dplyr::left_join(obj2_composto|>dplyr::select(refdate,local_id,normalizado))

cor(cobj2_compara$value,cobj2_compara$normalizado,use='complete.obs')
#[1] 0.9977225
summary(cobj2_compara)

readr::write_csv(obj2_composto|>dplyr::filter(lubridate::month(refdate)==12)|>
                   dplyr::transmute(refdate,local_id,value=normalizado),'coleta/cache/objetivo2_composto_aedi/obj2_c_aedi24.csv')
