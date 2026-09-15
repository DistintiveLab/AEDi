##necessita comp_obj1_aedi primeiro (definição de funções para cálculo do ranking)

obj4_inds_ind <- indseixo('objetivo4')
obj4_ri <- rankings_individuais(obj4_inds_ind)
obj4_composto <- indicador_composto(obj4_ri)

comp_obj4orig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_objetivo4'")

cobj4_compara <-
  comp_obj4orig|>
  dplyr::left_join(obj4_composto|>dplyr::select(refdate,local_id,normalizado))

cor(cobj4_compara$value,cobj4_compara$normalizado,use='complete.obs')
# 0,8722818 - ultima rodada

#[1] 0.8638569
summary(cobj4_compara)

readr::write_csv(obj4_composto|>dplyr::transmute(refdate,local_id,value=normalizado),'coleta/cache/objetivo4_composto_aedi/obj4_c_aedi.csv')
