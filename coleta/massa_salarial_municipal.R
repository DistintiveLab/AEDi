# ###Filtra e prepara dados base
#  dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('rais_vinculos_s38','rais_vlr_rem_dez_s38')")
# dbdbase <- dbdbase|>
#     dplyr::mutate(data_freq_id=max(data_freq_id))|>
#
#       tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
# ###Cria indicador
# dbdbase <- massa_salarial_municipal <- dbdbase |>
#                      dplyr::rename(setNames(c('rais_vinculos_s38','rais_vlr_rem_dez_s38'), c('a','b'))) |>
#                 dplyr::transmute(massa_salarial_municipal = a * b,refdate,local_id)

rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("mte_rais"),
                       user="mte_rais",
                       password=Sys.getenv("pwdrais"),
                       host=Sys.getenv("hostraispsql"))

# autocontencao: conexoes/objetos de sessao usados adiante
if (!exists("con") || !inherits(con, "DBIConnection"))
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        user=Sys.getenv("user", "aedi"),
                        password=Sys.getenv("password", "aEd1#man@gR"),
                        host=Sys.getenv("host", "127.0.0.1"),
                        dbname=Sys.getenv("dbname", "aedidb"))
if (!exists("locgeoloc"))
  locgeoloc <- DBI::dbGetQuery(con, "select local_id, local_name, geoloc_id from local")



massal_mun <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

massalmun <- data.table::rbindlist(
  lapply(AEDi:::anos_rais(rais),
         massal_mun)
)

# serie base no DW (recalculo completo, padrao A5b)
AEDi:::gravar_serie_dw("massa_salarial_municipal",
  data.frame(local = massalmun$local,
             periodo = as.Date(paste0(massalmun$ano, "-12-31")),
             valor = massalmun$massa_salarial))

dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('datasus_popmun')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)


massalmun <- massalmun|>
  dplyr::left_join(locgeoloc|>
                     dplyr::filter(local_id<5900)|>dplyr::mutate(geoloc_idd=trunc(geoloc_id/10)),
                   by = c("local"="geoloc_idd"))|>
  dplyr::mutate(refdate=as.Date(paste0(ano,"-07-01")))|>
  dplyr::left_join(dbdbase|>dplyr::select(refdate,local_id,datasus_popmun))


massalmun <-
  massalmun|>
  dplyr::mutate(uf=trunc(local/10000))|>
  dplyr::group_by(ano,uf)|>
  dplyr::filter(uf!=99)|>
  dplyr::mutate(pop_max = max(datasus_popmun),
                massa_salarial_uf = ifelse(datasus_popmun == pop_max, massa_salarial, NA),
                massa_salarial_uf = max(massa_salarial_uf, na.rm = TRUE),
                value = massa_salarial/massa_salarial_uf)

massalmun <-
  massalmun|>dplyr::mutate(
    value=massa_salarial/massa_salarial_uf
  )

obj2_3_aedi_recalc <- massalmun|>
  dplyr::transmute(
    refdate=as.Date(paste0(ano,"-12-31")),
    local_id,
    obj2_3_aedi = value)

AEDi:::gravar_serie_dw("objetivo2_3_via_aedi",
  data.frame(local = obj2_3_aedi_recalc$local_id,
             periodo = obj2_3_aedi_recalc$refdate,
             valor = obj2_3_aedi_recalc$obj2_3_aedi))

if (exists("objetivo2_3_orig")) {
obj2_3_compara2 <-
  objetivo2_3_orig|>
  dplyr::left_join(obj2_3_aedi_recalc)|>
  dplyr::transmute(refdate,local_id,obj2_3_base=value,
            obj2_3_aedi)

cor(obj2_3_compara2$obj2_3_base,obj2_3_compara2$obj2_3_aedi,use='complete.obs')
summary(obj2_3_compara2)
}
DBI::dbDisconnect(rais)
