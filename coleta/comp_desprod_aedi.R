##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

desprod_inds_ind <- indseixo('desprod')

# ##temporário pré atualização BD: dataviva
# datavivanovo <-
#   eci_atualizado|>
#   dplyr::left_join(locgeoloc,by=c("codmun"="geoloc_id"))|>
#   dplyr::mutate(orig_name="desprod1")|>
#   dplyr::select(refdate,local_id,orig_name,value=eci_cedeplar_novo)
#
# desprod_inds_ind <-
#   desprod_inds_ind|>
#   dplyr::filter((orig_name=='desprod1' & refdate<'2019-12-31')|orig_name!="desprod1")|>
#   dplyr::bind_rows(datavivanovo)
#
# ##fim parte temporária
desprod_ri <- rankings_individuais(desprod_inds_ind)
desprod_composto <- indicador_composto(desprod_ri)


comp_desprodorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_desprod'")

cdesprod_compara <-
  comp_desprodorig|>
  dplyr::left_join(desprod_composto|>dplyr::select(refdate,local_id,normalizado))

cor(cdesprod_compara$value,cdesprod_compara$normalizado,use='complete.obs')
#[1] 0.9854374 - pré atualização de dataviva
# 0.9977206 - após atualização dataviva
summary(cdesprod_compara)

readr::write_csv(desprod_composto|>
                   dplyr::transmute(refdate,local_id,value=normalizado),'coleta/cache/desprod_composto_aedi/desprod_c_aedi.csv')
