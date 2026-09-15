##necessita comp_obj1_aedi primeiro (definição de funções para cálculo do ranking)

obj3_inds_ind <- indseixo('objetivo3')
obj3_ri <- rankings_individuais(obj3_inds_ind)
obj3_composto <- indicador_composto(obj3_ri)

comp_obj3orig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_objetivo3'")

cobj3_compara <-
  comp_obj3orig|>
  dplyr::left_join(obj3_composto|>dplyr::select(refdate,local_id,normalizado))

cor(cobj3_compara$value,cobj3_compara$normalizado,use='complete.obs')
#[1] 0.9289509
summary(cobj3_compara)

readr::write_csv(obj3_composto|>
                   dplyr::filter(refdate>'2012-12-31',refdate<'2024-12-31')|>
                   dplyr::transmute(refdate,local_id,value=normalizado),'coleta/cache/objetivo3_composto_aedi/obj3_c_aedi.csv')
