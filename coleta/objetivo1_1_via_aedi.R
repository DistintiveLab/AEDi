# objetivo1_1_via_aedi (mdata 86): remuneracao media s38 menos a mediana
# nacional, por municipio-ano, do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ('rais_media_remdez_s38','rais_mediana_remmed_s38')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

o11 <- dbdbase |>
  dplyr::rename(a = 'rais_media_remdez_s38', b = 'rais_mediana_remmed_s38') |>
  dplyr::transmute(local_id, refdate, objetivo1_1_via_aedi = a - b)

AEDi:::gravar_serie_dw("objetivo1_1_via_aedi",
  data.frame(local = o11$local_id, periodo = o11$refdate,
             valor = o11$objetivo1_1_via_aedi))
DBI::dbDisconnect(con)
