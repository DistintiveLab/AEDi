##necessita comp_obj 1_aedi primeiro (definição de funções para cálculo do ranking)

dessoc_inds_ind <- indseixo('dessoc')

dessoc_ri <- rankings_individuais(dessoc_inds_ind)
dessoc_composto <- indicador_composto(dessoc_ri)


comp_dessocorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_dessoc'")


cdessoc_compara <-
  comp_dessocorig|>
  dplyr::left_join(dessoc_composto|>dplyr::select(refdate,local_id,normalizado))


cor(cdessoc_compara$value,cdessoc_compara$normalizado,use='complete.obs')
# 0.92035 - questão no rankeamento -
#diferencial salarial feminino considerado maior pior antes (dúvida?), o que
#não faz sentido para o indicador salario_medio_feminino/salario_medio_masculino

summary(cdessoc_compara)
