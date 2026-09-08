# ideb_media_basico_redep (mdata 75) e ideb_basico_rede_mediana (mdata 76):
# recalculo completo (replace) a partir das series-base do DW. Padrao A5b.
# Orquestrador: rodar DEPOIS de ideb_anos_iniciais/finais_redep atualizados.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ('ideb_anos_iniciais_redep','ideb_anos_finais_redep')") |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

mb <- dbdbase |>
  dplyr::rename(a1 = 'ideb_anos_iniciais_redep', a2 = 'ideb_anos_finais_redep') |>
  dplyr::transmute(local_id, refdate,
                   ideb_media_basico_redep = (a1 + a2) / 2) |>
  dplyr::filter(!is.na(ideb_media_basico_redep))

AEDi:::gravar_serie_dw("ideb_media_basico_redep",
  data.frame(local = mb$local_id, periodo = mb$refdate, valor = mb$ideb_media_basico_redep))

## mediana nacional por refdate da media basico (semantica do builder)
dbd <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ('ideb_media_basico_redep')")
med <- dbd |>
  dplyr::group_by(refdate) |>
  dplyr::mutate(ideb_basico_rede_mediana = median(value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, ideb_basico_rede_mediana) |>
  dplyr::distinct()

AEDi:::gravar_serie_dw("ideb_basico_rede_mediana",
  data.frame(local = med$local_id, periodo = med$refdate,
             valor = med$ideb_basico_rede_mediana))
DBI::dbDisconnect(con)
