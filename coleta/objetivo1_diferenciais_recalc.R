# objetivo1_2_via_aedi (77) e objetivo1_3_via_aedi (81): diferenciais vs
# mediana nacional (Ideb basico e profissionais de saude per capita).
# Recalculo completo (replace) do DW. Padrao A5b.

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

calc_diferencial <- function(par_a, par_b) {
  d <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
        WHERE orig_name IN ($1, $2)", params = list(par_a, par_b)) |>
    dplyr::mutate(data_freq_id = max(data_freq_id)) |>
    tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                       id_cols = c('local_id', 'refdate'))
  d |>
    dplyr::rename(a = dplyr::all_of(par_a), b = dplyr::all_of(par_b)) |>
    dplyr::transmute(local_id, refdate, valor = a - b)
}

o12 <- calc_diferencial('ideb_media_basico_redep', 'ideb_basico_rede_mediana')
AEDi:::gravar_serie_dw("objetivo1_2_via_aedi",
  data.frame(local = o12$local_id, periodo = o12$refdate, valor = o12$valor))

o13 <- calc_diferencial('profissionais_de_saude_pc', 'prof_saude_pc_mediana_nacional')
AEDi:::gravar_serie_dw("objetivo1_3_via_aedi",
  data.frame(local = o13$local_id, periodo = o13$refdate, valor = o13$valor))
DBI::dbDisconnect(con)
