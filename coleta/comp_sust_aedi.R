##Script para calcular indicador composto

indseixo <- \(eixo){
consulta <-
  DBI::dbGetQuery(mdr,
                  paste0("select refdate,local_id,orig_name,value from data_values a left join mdata b on
                         a.mdata_id = b.mdata_id where orig_name like '",
                         eixo,"%'"))
consulta
}

indic_sentido <- readr::read_csv("inst/extdata/indicadores_sentido.csv")

## 1 quando 'maior melhor', para que o número maior fique como ranking mais alto
indic_sentido <-
  indic_sentido|>dplyr::mutate(
    orig_name=ifelse(grepl("obj",orig_name),
                     paste0(orig_name,"_",n_indicador),
                     paste0(orig_name,n_indicador)))
sustindsr <-
  indseixo("sust")

data.table::setDT(sustindsr)
sustindsr <- sustindsr[refdate>'2013-12-31',]

sust_ri <- rankings_individuais(sustindsr)
sust_composto <- indicador_composto(sust_ri)


# ranqueia <- \(x) {
#   rank(x,ties.method="min",na.last=TRUE)
# }
# sustindsrank_individuais <-
#   sustindsr|>
#   dplyr::left_join(indic_sentido|>dplyr::select(orig_name,maior_melhor))|>
#   dplyr::group_by(refdate,orig_name)|>
#   dplyr::mutate(value=ranqueia(value*maior_melhor))
#
# sustindsrankg <-
#   sustindsrank_individuais|>
#   dplyr::group_by(refdate,local_id)|>
#   dplyr::summarize(sustmedia=mean(value))|>
#   dplyr::mutate(minimo=min(sustmedia),maximo=max(sustmedia),
#                 normalizado=(sustmedia-minimo)/(maximo-minimo))
#

comp_sustorig <-
  dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'comp_sust'")

csust_compara <-
  comp_sustorig|>
  dplyr::left_join(
#    sustindsrankg|>
      sust_composto|>
      dplyr::select(refdate,local_id,normalizado))

cor(csust_compara$value,csust_compara$normalizado,use='complete.obs')

summary(csust_compara)
#0.9752942
#Engenharia reversa - ties method average 0,94 cor , min 0.9753409
#0.9753409

readr::write_csv(
  sustindsrankg|>
    dplyr::filter(refdate>'2013-12-31',
                  refdate<'2024-12-31')|>
    dplyr::transmute(refdate,local_id,value=normalizado),
'coleta/cache/sust_composto_aedi/sust_c_aedi.csv'
)
