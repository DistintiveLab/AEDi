# emprego_agricola_sobre_total_mun (mdata 99): participacao do emprego
# agricola no emprego formal municipal, do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('empregoformal_agricola_municipal','emprego_formal_municipal')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

ea <- dbdbase |>
  dplyr::rename(a = 'empregoformal_agricola_municipal', b = 'emprego_formal_municipal') |>
  dplyr::transmute(local_id, refdate, emprego_agricola_sobre_total_mun = a / b)

AEDi:::gravar_serie_dw("emprego_agricola_sobre_total_mun",
  data.frame(local = ea$local_id, periodo = ea$refdate,
             valor = ea$emprego_agricola_sobre_total_mun))
DBI::dbDisconnect(con)
