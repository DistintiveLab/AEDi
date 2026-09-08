# objetivo3_3_via_aedi (mdata 97): razao de crescimento populacional
# (pop / pop do ano anterior), refdate 31/12 do ano. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('datasus_popmun')")

o33 <- dbdbase |>
  dplyr::group_by(local_id) |>
  dplyr::arrange(local_id, refdate) |>
  dplyr::mutate(objetivo3_3_via_aedi = value / dplyr::lag(value)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, objetivo3_3_via_aedi) |>
  dplyr::mutate(refdate = as.Date(paste0(lubridate::year(refdate), "-12-31")))

AEDi:::gravar_serie_dw("objetivo3_3_via_aedi",
  data.frame(local = o33$local_id, periodo = o33$refdate,
             valor = o33$objetivo3_3_via_aedi))
DBI::dbDisconnect(con)
