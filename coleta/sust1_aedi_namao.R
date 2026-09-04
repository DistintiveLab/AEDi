#salmedio_semadmpub_municipal_genero - pega dif genero

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
if (!exists("popmunicipal") || !is.data.frame(popmunicipal))
  popmunicipal <- DBI::dbGetQuery(con, "
    SELECT DISTINCT ON (local, extract(year from a.refdate))
           trunc(c.geoloc_id/10) local, a.refdate, a.value populacao
      FROM data_values a
      JOIN mdata  b ON a.mdata_id = b.mdata_id
      JOIN local  c ON a.local_id  = c.local_id
     WHERE b.orig_name = 'datasus_popmun'
     ORDER BY local, extract(year from a.refdate), a.refdate DESC") |>
    (\(d) dplyr::mutate(d, ano = lubridate::year(refdate)))()



pega_emprego_recic <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe BETWEEN 38000 AND 39999   GROUP BY municipio")
  )
  a$ano <- ano
  a
}


empregos_reci_gresid <-
  data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pega_emprego_recic))



sust1_aedi <- popmunicipal|>
  dplyr::mutate(local,ano=lubridate::year(refdate))|>
  dplyr::left_join(empregos_reci_gresid)

sust1_aedi[is.na(sust1_aedi)] <- 0

sust1_aedi$sust1_aedi <- 1e6*sust1_aedi$qtd_vinculos_agr/sust1_aedi$populacao

readr::write_csv(sust1_aedi,"coleta/cache/sust1_aedi/sust1_aedi.csv")
#Conferência
sust1_aedic <- sust1_aedi|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id=local,
                   sust1_aedi)

# serie no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("sust1",
  data.frame(local = sust1_aedic$geoloc_id,
             periodo = sust1_aedic$refdate,
             valor = sust1_aedic$sust1_aedi))
DBI::dbDisconnect(rais)

# locgeoloc <- dbGetQuery(con,
#                         "SELECT * from local where local_id < 5571")

sust1_orig <- dbGetQuery(mdr,"select refdate,local_id,value from data_values a left join mdata b on a.mdata_id = b.mdata_id where orig_name like 'sust1%'")

sust1_compara <- sust1_orig|>dplyr::filter(refdate > '2013-12-31')|>
  dplyr::left_join(locgeoloc|>dplyr::select(local_id,geoloc_id))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/10))|>
  dplyr::left_join(sust1_aedic)

cor(sust1_compara$value,sust1_compara$sust1_aedi,use='complete.obs')
#0.9999198


