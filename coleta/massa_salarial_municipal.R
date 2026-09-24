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

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),unused_fn=dplyr::first)


# regra semantica de municipio do DW: bloco historico (1..5570) MAIS os
# incorporados apos o bloco PNAD (7088+; ver incorporar_municipio_ibge);
# o filtro antigo (<5900) deixava entrar locais do bloco PNAD (5571..7087)
mun_semantico <- locgeoloc |>
  dplyr::filter(local_id < 5571 | local_id > 7087) |>
  dplyr::mutate(geoloc_idd = trunc(geoloc_id / 10))

# codigos RAIS sem municipio no DW (ex.: criacao ainda nao incorporada)
# nao podem seguir com local_id NA: dropa e avisa
sem_match <- sort(setdiff(unique(massalmun$local), mun_semantico$geoloc_idd))
if (length(sem_match))
  warning("massa_salarial_municipal: codigos RAIS sem municipio no DW: ",
          paste(sem_match, collapse = ", "))
massalmun <- massalmun |>
  dplyr::filter(local %in% mun_semantico$geoloc_idd) |>
  dplyr::left_join(mun_semantico, by = c("local" = "geoloc_idd")) |>
  dplyr::mutate(refdate = as.Date(paste0(ano, "-07-01"))) |>
  dplyr::left_join(dbdbase |> dplyr::select(refdate, local_id, datasus_popmun))


# maxsna guardado: sem ele max(x, na.rm = TRUE) devolve -Inf quando a coluna
# inteira e NA (ex.: ano sem populacao no DW), e value = massa/-Inf = 0 -- foi
# o que zerou objetivo2_3 em 2025.
maxsna <- \(x) { m <- suppressWarnings(max(x, na.rm = TRUE)); if (is.infinite(m) || is.na(m)) NA_real_ else m }

massalmun <-
  massalmun|>
  dplyr::mutate(uf=trunc(local/10000))|>
  dplyr::filter(uf!=99)|>
  dplyr::group_by(ano,uf)|>
  dplyr::mutate(
    pop_max = maxsna(datasus_popmun),
    # denominador: massa do municipio mais populoso do estado (proxy da capital)
    massa_salarial_uf = maxsna(ifelse(!is.na(pop_max) & !is.na(datasus_popmun) &
                                        datasus_popmun == pop_max,
                                      massa_salarial, NA)),
    # ano sem populacao: cai no maximo da massa do estado (mesma grandeza)
    massa_salarial_uf = ifelse(is.na(massa_salarial_uf),
                               maxsna(massa_salarial), massa_salarial_uf),
    value = ifelse(is.finite(massa_salarial_uf),
                   massa_salarial/massa_salarial_uf, NA_real_))|>
  dplyr::ungroup()

# diagnostico: sem isto o ano sem populacao zerava em silencio
sem_pop <- massalmun|>dplyr::filter(is.na(datasus_popmun))|>dplyr::distinct(ano)|>dplyr::pull(ano)
if (length(sem_pop))
  warning("massa_salarial_municipal: sem datasus_popmun em ",
          paste(sem_pop, collapse = ", "),
          " -- denominador caiu no maximo da massa do estado")
sem_valor <- massalmun|>dplyr::filter(is.na(value))|>dplyr::distinct(ano)|>dplyr::pull(ano)
if (length(sem_valor))
  warning("massa_salarial_municipal: ", paste(sem_valor, collapse = ", "),
          " ficaram sem valor (NA, nao zero)")

obj2_3_aedi_recalc <- massalmun|>
  dplyr::transmute(
    refdate=as.Date(paste0(ano,"-12-31")),
    local_id,
    obj2_3_aedi = value)

# cache para o upsert.R ler (o upsert roda em sessao manual, sem o objeto)
dir.create("coleta/cache/objetivo2_3_via_aedi", recursive = TRUE, showWarnings = FALSE)
saveRDS(obj2_3_aedi_recalc, "coleta/cache/objetivo2_3_via_aedi/obj2_3_aedi.rds")

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
