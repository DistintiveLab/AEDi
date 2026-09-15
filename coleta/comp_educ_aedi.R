##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

educ_inds_ind <- indseixo('educ')

educ_ri <- rankings_individuais(educ_inds_ind)
educ_composto <- indicador_composto(educ_ri)


comp_educorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_educ'")

ceduc_compara <-
  comp_educorig|>
  dplyr::left_join(educ_composto|>dplyr::select(refdate,local_id,normalizado))


cor(ceduc_compara$value,ceduc_compara$normalizado,use='complete.obs')
#0.9904214 - pendente recálculo AFD
#0.9904164 - após recálculo
summary(ceduc_compara)

readr::write_csv(
  educ_composto|>
    dplyr::filter(refdate>'2014-12-31')|>
    dplyr::transmute(refdate,local_id,value=normalizado),
  'coleta/cache/educ_composto_aedi/educ_c_aedi.csv'
)
