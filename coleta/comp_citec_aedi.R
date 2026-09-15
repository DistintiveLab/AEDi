##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

citec_inds_ind <- indseixo('citec')

citec_ri <- rankings_individuais(citec_inds_ind)
citec_composto <- indicador_composto(citec_ri)


comp_citecorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_citec'")

ccitec_compara <-
  comp_citecorig|>
  dplyr::left_join(citec_composto|>dplyr::select(refdate,local_id,normalizado))


cor(ccitec_compara$value,ccitec_compara$normalizado,use='complete.obs')
#0.8688893 - com ultima rodada
#0.9832535   - PRE NOVA VERSÃO DE 2_4 BADEPI
summary(ccitec_compara)


##Com novo badepi

citec_inds_ind <- indseixo('citec')

citec_ri <- rankings_individuais(citec_inds_ind)
citec_composto <- indicador_composto(citec_ri)

ccitec_compara2 <-
  comp_citecorig|>
  dplyr::left_join(citec_composto|>dplyr::select(refdate,local_id,normalizado))


cor(ccitec_compara2$value,ccitec_compara2$normalizado,use='complete.obs')
#0.9832535   - PRE NOVA VERSÃO DE 2_4 BADEPI
summary(ccitec_compara2)

readr::write_csv(citec_composto|>
                   dplyr::filter(refdate>'2012-12-31',
                                 refdate<'2024-12-31')|>
                   dplyr::transmute(refdate,local_id,value=normalizado),
                 'coleta/cache/citec_composto_aedi/citec_c_aedi.csv')
