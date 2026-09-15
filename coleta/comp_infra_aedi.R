##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

infra_inds_ind <- indseixo('infra')

infra_ri <- rankings_individuais(infra_inds_ind)
infra_composto <- indicador_composto(infra_ri)


comp_infraorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_infra'")

cinfra_compara <-
  comp_infraorig|>
  dplyr::left_join(infra_composto|>dplyr::select(refdate,local_id,normalizado))

cinfra_compara <-
  cinfra_compara|>
   dplyr::filter(
     refdate>'2016-12-31',
     refdate<'2021-12-31')

cor(cinfra_compara$value,cinfra_compara$normalizado,use='complete.obs')
#0.42 - 2017 a 2020
#0.5895781
#
summary(cinfra_compara)

readr::write_csv(infra_composto|>
                   #dplyr::filter(refdate>'2014-12-31',refdate<'2024-12-31')|>
                   dplyr::transmute(refdate,local_id,value=normalizado),
                 'coleta/cache/infra_composto_aedi/infra_c_aedi24p.csv')
