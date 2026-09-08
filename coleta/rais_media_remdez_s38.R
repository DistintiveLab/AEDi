# rais_media_remdez_s38 (mdata 84): remuneracao media de dezembro (s38) =
# massa salarial s38 / vinculos s38, por municipio-ano, do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ('rais_vlr_rem_dez_s38','rais_vinculos_s38')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

md <- dbdbase |>
  dplyr::rename(a = 'rais_vlr_rem_dez_s38', b = 'rais_vinculos_s38') |>
  dplyr::transmute(local_id, refdate, rais_media_remdez_s38 = a / b)

AEDi:::gravar_serie_dw("rais_media_remdez_s38",
  data.frame(local = md$local_id, periodo = md$refdate,
             valor = md$rais_media_remdez_s38))
DBI::dbDisconnect(con)
