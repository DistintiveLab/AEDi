# objetivo3_1_via_aedi (mdata 95): participacao percentual do emprego de
# nivel superior no emprego formal municipal, do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('emprego_nivsuperior_municipal','emprego_formal_municipal')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

o31 <- dbdbase |>
  dplyr::rename(a = 'emprego_nivsuperior_municipal', b = 'emprego_formal_municipal') |>
  dplyr::transmute(local_id, refdate, objetivo3_1_via_aedi = 100 * a / b)

AEDi:::gravar_serie_dw("objetivo3_1_via_aedi",
  data.frame(local = o31$local_id, periodo = o31$refdate,
             valor = o31$objetivo3_1_via_aedi))
DBI::dbDisconnect(con)
