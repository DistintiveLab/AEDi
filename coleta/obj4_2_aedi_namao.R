#emprego_minearacao_municipal

# autocontencao (padrao A5b)
if (!exists("rais") || !inherits(rais, "DBIConnection")) rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  dbname=Sys.getenv("mte_rais"), user="mte_rais",
  password=Sys.getenv("pwdrais"), host=Sys.getenv("hostraispsql"))
if (!exists("con") || !inherits(con, "DBIConnection")) con <- DBI::dbConnect(RPostgres::Postgres(),
  user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
  host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))
if (!exists("mdr") || !inherits(mdr, "DBIConnection")) mdr <- con
if (!exists("locgeoloc") || !is.data.frame(locgeoloc)) locgeoloc <- DBI::dbGetQuery(con,
  "select local_id, local_name, geoloc_id from local")



pegamineracao <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe BETWEEN 4999 AND 9999 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

emprego_mineracao_municipal <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegamineracao))

empregoformalmun <- dbGetQuery(con,
                               "SELECT trunc(geoloc_id/10) local,value, extract('year' from refdate) ano
                                 from data_values a left join mdata b on a.mdata_id = b.mdata_id
                                 left join local c on a.local_id = c.local_id where orig_name like 'emprego_formal_mun%' and extract('year' from refdate) > 2013")


emprego_mineracao_municipal <-
  empregoformalmun|>
  dplyr::left_join(emprego_mineracao_municipal|>dplyr::mutate(ano=as.numeric(ano)))


emprego_mineracao_municipal$propmin <- emprego_mineracao_municipal$qtd_vinculos_agr/emprego_mineracao_municipal$value

emprego_mineracao_municipal <- emprego_mineracao_municipal|>
  dplyr::ungroup()|>dplyr::group_by(ano)|>
  dplyr::mutate(brmediamin = sum(qtd_vinculos_agr,na.rm=T)/sum(value))|>
  dplyr::ungroup()

emprego_mineracao_municipal$obj4_2_aedi <- emprego_mineracao_municipal$propmin/emprego_mineracao_municipal$brmediamin

emprego_mineracao_municipal[is.na(emprego_mineracao_municipal)] <- 0

#Conferência
teste_aedi <- emprego_mineracao_municipal|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   obj4_2_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("objetivo4_2",
  data.frame(local = teste_aedi$geoloc_id,
             periodo = teste_aedi$refdate,
             valor = teste_aedi$obj4_2_aedi))
DBI::dbDisconnect(rais)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

objetivo4_2_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'objetivo4_2%'")

obj4_2_compara <- objetivo4_2_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(teste_aedi)

cor(obj4_2_compara$value,obj4_2_compara$obj4_2_aedi,use='complete.obs')
#0.9971916

