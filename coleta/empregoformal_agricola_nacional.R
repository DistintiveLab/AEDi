# empregoagricola_prop_nacional (mdata 100): participacao nacional do emprego
# agricola no emprego formal, por refdate (valor nacional replicado por
# local, semantica do builder). Padrao A5b.

somasna <- \(x) sum(x, na.rm = TRUE)

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

pn <- dbdbase |>
  dplyr::group_by(refdate) |>
  dplyr::rename(a = 'empregoformal_agricola_municipal', b = 'emprego_formal_municipal') |>
  dplyr::mutate(across(c(a, b), somasna)) |>
  dplyr::ungroup() |>
  dplyr::distinct(refdate, .keep_all = TRUE) |>
  dplyr::transmute(refdate, valor = a / b)

serie <- merge(dbdbase[, c('local_id', 'refdate')], pn, by = 'refdate')

AEDi:::gravar_serie_dw("empregoagricola_prop_nacional",
  data.frame(local = serie$local_id, periodo = serie$refdate,
             valor = serie$valor))
DBI::dbDisconnect(con)
