# objetivo3_2_via_aedi (mdata 96): salario medio municipal (massa salarial /
# emprego formal), do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('massa_salarial_municipal','emprego_formal_municipal')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

o32 <- dbdbase |>
  dplyr::rename(a = 'massa_salarial_municipal', b = 'emprego_formal_municipal') |>
  dplyr::transmute(local_id, refdate, objetivo3_2_via_aedi = a / b)

AEDi:::gravar_serie_dw("objetivo3_2_via_aedi",
  data.frame(local = o32$local_id, periodo = o32$refdate,
             valor = o32$objetivo3_2_via_aedi))
saveRDS(o32, 'coleta/cache/objetivo3_2_via_aedi/objetivo3_2_aedi.rds')
DBI::dbDisconnect(con)
