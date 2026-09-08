# rais_mediana_remmed_s38 (mdata 85): mediana nacional por refdate da
# remuneracao media s38 (mesma semantica do builder: cada local recebe a
# mediana do seu refdate). Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ('rais_media_remdez_s38')")

med <- dbdbase |>
  dplyr::group_by(refdate) |>
  dplyr::mutate(rais_mediana_remmed_s38 = median(value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, rais_mediana_remmed_s38) |>
  dplyr::distinct()

AEDi:::gravar_serie_dw("rais_mediana_remmed_s38",
  data.frame(local = med$local_id, periodo = med$refdate,
             valor = med$rais_mediana_remmed_s38))
DBI::dbDisconnect(con)
