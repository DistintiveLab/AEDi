# max_massa_salarial_na_uf (mdata 90): maximo estadual da massa salarial
# municipal por refdate (cada municipio recebe o maximo do seu estado).
# Padrao A5b.

maxsna <- \(x) { m <- max(x, na.rm = TRUE); if (is.infinite(m)) NA else m }

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
        WHERE orig_name IN ('massa_salarial_municipal')")

mx <- dbdbase |>
  dplyr::group_by(estado, refdate) |>
  dplyr::mutate(max_massa_salarial_na_uf = maxsna(value)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, max_massa_salarial_na_uf)

AEDi:::gravar_serie_dw("max_massa_salarial_na_uf",
  data.frame(local = mx$local_id, periodo = mx$refdate,
             valor = mx$max_massa_salarial_na_uf))
DBI::dbDisconnect(con)
