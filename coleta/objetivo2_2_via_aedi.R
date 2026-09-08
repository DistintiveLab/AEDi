# objetivo2_2_via_aedi (mdata 88): populacao municipal relativa ao maximo
# estadual (datasus_popmun / maxpopestadual), refdate 31/12 do ano. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('datasus_popmun','maxpopestadual')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

o22 <- dbdbase |>
  dplyr::rename(a = 'datasus_popmun', b = 'maxpopestadual') |>
  dplyr::transmute(local_id, refdate, objetivo2_2_via_aedi = a / b) |>
  dplyr::mutate(refdate = as.Date(paste0(lubridate::year(refdate), "-12-31")))

AEDi:::gravar_serie_dw("objetivo2_2_via_aedi",
  data.frame(local = o22$local_id, periodo = o22$refdate,
             valor = o22$objetivo2_2_via_aedi))
DBI::dbDisconnect(con)
