##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

governativas_inds_ind <- indseixo('governativas')

# ##temporario pre atualização banco de dados
# gov4_aedic <- gov4aedic|>
#   dplyr::left_join(locgeoloc)|>
#   dplyr::transmute(refdate,local_id,orig_name="governativas4",value=gov4_aedi)
#
# governativas_inds_ind <-
#   governativas_inds_ind|>
#   dplyr::filter(orig_name!="governativas4")|>
#   dplyr::bind_rows(gov4_aedic)
#
# #fim temporário

governativas_ri <- rankings_individuais(governativas_inds_ind)
governativas_composto <- indicador_composto(governativas_ri)


comp_governativasorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_governativas'")

cgovernativas_compara <-
  comp_governativasorig|>
  dplyr::left_join(governativas_composto|>dplyr::select(refdate,local_id,normalizado))


cor(cgovernativas_compara$value,cgovernativas_compara$normalizado,use='complete.obs')
# 0.9446341
#0.9415102 - com versão atualizada eci
summary(cgovernativas_compara)

readr::write_csv(
  governativas_composto|>
    dplyr::filter(refdate>'2013-12-31',
                  refdate<'2024-12-31')|>
    dplyr::transmute(
      refdate,local_id,value=normalizado),
  'coleta/cache/governativas_composto_aedi/governativas_c_aedi.csv'
)
