Sys.setlocale(category='LC_TIME',locale='pt_BR.UTF-8')
#datasus::sih_nrbr_mun()
periodos <- stringr::str_to_title(format.Date(seq.Date(as.Date('2013-01-01'),as.Date('2024-12-01'),by='month'),'%b/%Y'))
internacao_desnutricao <-
  datasus::sih_nibr_mun(
    periodo=periodos,
    coluna='Ano processamento',
    categoria_cid10 = 'Desnutrição' )

internacao_desnutricao <-
  internacao_desnutricao|>
  tidyr::separate_wider_delim(Município,delim=" ",names = c("cd_mun","nm_mun"),
                              too_many="merge",too_few="align_end")

internacao_desnutricao$cd_mun <- as.numeric(internacao_desnutricao$cd_mun)
internacao_desnutricao[is.na(internacao_desnutricao)] <- 0

internacao_total <-
  datasus::sih_nibr_mun(
    periodo=periodos,
    coluna='Ano processamento')

internacao_total <-
  internacao_total|>
  tidyr::separate_wider_delim(Município,delim=" ",names = c("cd_mun","nm_mun"),
                              too_many="merge",too_few="align_end")

internacao_total$cd_mun <- as.numeric(internacao_total$cd_mun)
internacao_total[1,1] <- 0


internacao_para_indicador <-
  internacao_desnutricao|>dplyr::select(-"Total")|>
  tidyr::pivot_longer(-1:-2,names_to="ano",values_to="internacao_desnutricao")|>
  dplyr::left_join(internacao_total|>dplyr::select(-"Total")|>
              tidyr::pivot_longer(-1:-2,names_to="ano",values_to="internacao_total"))



internacao_para_indicador <-
  internacao_para_indicador|>
  dplyr::mutate(
  dessoc1 = 100*internacao_desnutricao/internacao_total
  )

readr::write_csv(internacao_para_indicador,'coleta/cache/dessoc1_aedi/dessoc1_aedi1324.csv')
#Conferência
dessoc1_orig <- DBI::dbGetQuery(mdr,"select TRUNC(geoloc_id/10) cd_mun,EXTRACT('YEAR' from refdate) ano, value from data_values a left join mdata b on a.mdata_id = b. mdata_id left join local c on a.local_id = c.local_id where orig_name LIKE 'dessoc1%'")

dessoc1_compara <-
  internacao_para_indicador|>
  dplyr::mutate(ano=as.numeric(ano))|>
  dplyr::left_join(dessoc1_orig)

dessoc1_compara <-
  dessoc1_compara[!is.na(dessoc1_compara$value) & !is.na(dessoc1_compara$dessoc1),]

#Morb CID-10
#<option value="125">Desnutrição</option>
#<option value="128">Seqüelas de desnutrição e de outras defic nutric</option>

#<option value="126">Deficiência de vitamina A</option>
#<option value="127">Outras deficiências vitamínicas</option>
