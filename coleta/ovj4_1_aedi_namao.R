#empregoformal_agricola_municipal

rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      dbname=Sys.getenv("mte_rais"),
                      user="mte_rais",
                      password=Sys.getenv("pwdrais"),
                      host=Sys.getenv("hostraispsql"))

# autocontencao (padrao A5b)
if (!exists("con") || !inherits(con, "DBIConnection")) con <- DBI::dbConnect(RPostgres::Postgres(),
  user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
  host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))



pegagricola <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                       ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe <4000 GROUP BY municipio")
                )
  a$ano <- ano
  a
}
#empregoformal_agricola_municipal <- data.table::rbindlist(lapply(2016:2023,pegagricola))

empregoformal_agricola_municipal <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pegagricola))

#empregoformal_agricola_municipal <- readr::read_csv("coleta/cache/empregoformal_agricola_municipal/empregoformal_agricola_municipal.csv")
# empregoformal_agricola_municipal <-
#   data.table::rbindlist(
#     list(
#       pegagricola(2013),
#       empregoformal_agricola_municipal
#     ))
#

empregoformalmun <- dbGetQuery(con,
                                 "SELECT trunc(geoloc_id/10) local,value, extract('year' from refdate) ano
                                 from data_values a left join mdata b on a.mdata_id = b.mdata_id
                                 left join local c on a.local_id = c.local_id where orig_name like 'emprego_formal_mun%' and extract('year' from refdate) > 2012")


empregoformalmun <- empregoformalmun|>
  dplyr::left_join(empregoformal_agricola_municipal)

empregoformalmun[is.na(empregoformalmun$qtd_vinculos_agr),]$qtd_vinculos_agr <- 0
empregoformalmun$propagr <- empregoformalmun$qtd_vinculos_agr/empregoformalmun$value

empregoformalmun <- empregoformalmun|>
  dplyr::ungroup()|>dplyr::group_by(ano)|>
  dplyr::mutate(brmediaagr = sum(qtd_vinculos_agr,na.rm=T)/sum(value))|>
  dplyr::ungroup()

empregoformalmun$obj4_1_aedi <- empregoformalmun$propagr/empregoformalmun$brmediaagr

empregoformalmun[is.na(empregoformalmun)] <- 0

saveRDS(empregoformalmun,'coleta/cache/objetivo4_1_via_aedi/objetivo4_1_aedic24.rds')

#Conferência
teste_aedi <- empregoformalmun|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
            geoloc_id=local,
            obj4_1_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("objetivo4_1",
  data.frame(local = teste_aedi$geoloc_id,
             periodo = teste_aedi$refdate,
             valor = teste_aedi$obj4_1_aedi))
DBI::dbDisconnect(rais)

# conferencia protegida (objetivo4_1_orig so existe em sessoes antigas)
if (exists("objetivo4_1_orig")) {
locgeoloc <- dbGetQuery(con,
                        "SELECT * from local where local_id < 5571")

obj4_1_compara <- objetivo4_1_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(teste_aedi)

cor(obj4_1_compara$value,obj4_1_compara$obj4_1_aedi,use='complete.obs')
#0.9977868
#0.9705379 <- com correto < 2000 cnae_classe
}
