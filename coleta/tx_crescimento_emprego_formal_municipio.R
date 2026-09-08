# tx_crescimento_emprego_formal_municipio (mdata 93): variacao percentual
# anual do emprego formal municipal (percentvar_sna do builder). Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('emprego_formal_municipal')")

tx <- dbdbase |>
  dplyr::group_by(local_id) |>
  dplyr::arrange(local_id, refdate) |>
  dplyr::mutate(tx = 100 * (value - dplyr::lag(value)) / dplyr::lag(value)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, tx)

AEDi:::gravar_serie_dw("tx_crescimento_emprego_formal_municipio",
  data.frame(local = tx$local_id, periodo = tx$refdate, valor = tx$tx))
DBI::dbDisconnect(con)
