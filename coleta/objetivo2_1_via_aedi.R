### Cria indexes nas tabelas de vínculos do banco postgresql da RAIS criado


con <-
  DBI::dbConnect(  RPostgreSQL::PostgreSQL(), dbname=Sys.getenv("mte_rais", "mte_rais"), user="mte_rais",password=Sys.getenv("pwdrais"),
                   host=Sys.getenv("hostraispsql"),port=5432     )


#### recálculo indicador de centralidade

vinculos_ano_setor <- \(ano) {
  a <- dbGetQuery(con,paste0("SELECT municipio, cnae_2_0_classe setor,  COUNT(*) qtd_vinc from rais_vinculo_",ano,
                             " WHERE vinculo_ativo_31_12 = 1 GROUP BY municipio, setor"))
  a$ano <-  ano
  a
}


#emprego_por_setormun <- vinculos_ano_setor(2014)

emprego_por_cnae_mun <- data.table::rbindlist(lapply(AEDi:::anos_rais(con), vinculos_ano_setor))


objetivo2_1_aedi <- emprego_por_cnae_mun|>

  dplyr::mutate(
    uf = trunc(municipio/10000),
    setor = trunc(setor/1000))|>
  dplyr::group_by(municipio, ano, setor, uf) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE)) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::mutate(vinc_munic = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(uf, setor, ano) |>
  dplyr::mutate(vinc_setor_uf = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(uf, ano) |>
  dplyr::mutate(vinc_uf = sum(vinc_setor, na.rm = TRUE),
                value = (vinc_setor/vinc_munic)*(1-(vinc_setor_uf/vinc_uf))*log(vinc_setor/vinc_munic)) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)*-1) |>
  dplyr::mutate(variavel = "objetivo2_1") |>
  dplyr::rename(codmun = municipio) |>
  dplyr::select(ano, codmun, variavel, value) |>
  dplyr::ungroup()

saveRDS(objetivo2_1_aedi,'coleta/cache/objetivo2_1_aedi/objetivo2_1_aedi.rds')

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("objetivo2_1",
  data.frame(local = objetivo2_1_aedi$codmun,
             periodo = as.Date(paste0(objetivo2_1_aedi$ano, "-12-31")),
             valor = objetivo2_1_aedi$value))
DBI::dbDisconnect(con)

#Conferência (tolerante a banco de conferencia inacessivel)
try({
mdr <- DBI::dbConnect(RPostgres::Postgres(),
                      user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
                      host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))
locgeoloc <- dbGetQuery(mdr,'select * from local')
objetivo2_1_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo2_1%'")
obj2_1_compara <- objetivo2_1_orig|>
  dplyr::left_join(locgeoloc)|>
  dplyr::mutate(codmun=trunc(geoloc_id/10),ano=lubridate::year(refdate))|>
  dplyr::left_join(objetivo2_1_aedi|>dplyr::rename(valor=value))|>
  dplyr::transmute(refdate,local_id,obj2_1_base=value,obj2_1_via_aedi=valor)
cat("cor objetivo2_1:", cor(obj2_1_compara$obj2_1_base,obj2_1_compara$obj2_1_via_aedi,use='complete.obs'), "\n")
DBI::dbDisconnect(mdr)
}, silent = TRUE)
