
lemissoes <- readxl::read_xlsx('coleta/cache/sust4_aedi/Dados municipais resumido - CO2e GWP-AR5 v12.0.xlsx',
                               sheet = "Dados")

lemissoes_r <- lemissoes|>
  janitor::clean_names()|>
  dplyr::select(emissao_remocao_bunker,setor_de_emissao,
                id_territorio,municipio,contains("x"))|>
  dplyr::filter(nchar(id_territorio==7) &
                  setor_de_emissao %in% c("Agropecuária", "Processos Industriais") &
                  emissao_remocao_bunker %in% c("Emissão", "Remoção"))|>
  tidyr::pivot_longer(cols = 5:last_col(),
                      names_to="ano",
                      values_to="value")|>
  dplyr::group_by(id_territorio,municipio,ano)|>
  dplyr::summarise(value=sum(value,na.rm=T))|>
  dplyr::ungroup()|>
  dplyr::arrange(id_territorio)|>
  dplyr::mutate(ano=as.numeric(gsub("x","",ano)))|>
  dplyr::filter(!grepl("NA",municipio))

sust4 <- readRDS('coleta/cache/sust4_aedi/emissoes_agro_industria.rds')

sust4 <- sust4|>
  dplyr::transmute(refdate=as.Date(paste0(ano,"-12-31")),
                                   geoloc_id=id_territorio-1e7,
                                   emissoes=value/1e6)

sust4_orig <- dbGetQuery(mdr,"select refdate,geoloc_id,value from
                     data_values a left join mdata b on a.mdata_id = b.mdata_id left join
                     local c on a.local_id = c.local_id where orig_name like 'sust4%'")

sust4_compara <-
  sust4_orig|>
  dplyr::left_join(
    sust4
  )

cor(sust4_compara$value,sust4_compara$emissoes)
#0.9958012

readr::write_csv(sust4,"coleta/cache/sust4_aedi/sust4_aedi.csv")

summary(sust4_compara|>dplyr::transmute(refdate,geoloc_id,sust4_base=value,sust4_aedi=emissoes))
