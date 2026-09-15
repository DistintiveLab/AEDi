### rep r-dist

# userais="mte_rais"
# passwordrais="aEd1#man@gRpublicrais"
# hostprais="38.242.154.34"

# con <-
#   DBI::dbConnect(  RPostgreSQL::PostgreSQL(), dbname=unique( catalog[ , "dbfile" ] )[[1]], user=userais,password=passwordrais,
#                    host=hostprais,port=5432     )
#
#
# #### recálculo indicador de centralidade
#
# vinculos_ano_setor <- \(ano) {
#   a <- dbGetQuery(con,paste0("SELECT municipio, cnae_2_0_classe setor,  COUNT(*) qtd_vinc from rais_vinculo_",ano,
#                       " GROUP BY municipio, setor"))
#   a$ano <-  ano
#   a
# }
#
#
# #emprego_por_setormun <- vinculos_ano_setor(2014)
#
# emprego_por_cnae_mun <- data.table::rbindlist(lapply(2013:2023,vinculos_ano_setor))


objetivo4_3_aedi <- emprego_por_cnae_mun|>

  dplyr::mutate(
    uf = trunc(municipio/10000),
    setor = trunc(setor/1000))|>
  dplyr::group_by(municipio, ano, setor, uf) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE)) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::mutate(vinc_munic = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by( setor, ano) |>
  dplyr::mutate(vinc_setor_br = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(vinc_br = sum(vinc_setor, na.rm = TRUE),
                value = abs((vinc_setor/vinc_munic)-(vinc_setor_br/vinc_br))) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)/2) |>
  dplyr::mutate(variavel = "objetivo4_3") |>
  dplyr::rename(codmun = municipio) |>
  dplyr::select(ano, codmun, variavel, value) |>
  dplyr::ungroup()

saveRDS(objetivo4_3_aedi,'coleta/cache/objetivo4_3_via_aedi/objetivo4_3_aedi.rds')

objetivo4_3_aedi <- readRDS("coleta/cache/objetivo4_3_via_aedi/objetivo4_3_aedi.rds")
##Conferência

objetivo4_3_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo4_3%'")

obj4_3_compara <- objetivo4_3_orig|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(objetivo4_3_aedi|>
                     dplyr::transmute(
                       refdate=as.Date(paste0(ano,"-12-31")),
                       local=codmun,
                       obj4_3_aedi=value
                     ))

cor(obj4_3_compara$value,obj4_3_compara$obj4_3_aedi,use='complete.obs')
#0.9207802
#0.9899445
summary(obj4_3_compara|>dplyr::transmute(refdate,local_id,obj4_3_base=value,obj4_3_aedi))
#
