# maxpopestadual (mdata 87): maximo estadual da populacao municipal
# (datasus_popmun) por refdate — recorte usado como denominador/referencia.
# Recalculo completo (replace) a partir do DW. Padrao A5b.

maxsna <- \(x) { m <- max(x, na.rm = TRUE); if (is.infinite(m)) NA else m }

# conexao LOCAL incondicional: nunca herdar con de sessao (pode apontar
# para o painelpndr remoto, onde as matviews tem outros dados)
con <- DBI::dbConnect(RPostgres::Postgres(),
                        user = Sys.getenv("user", "aedi"),
                        password = Sys.getenv("password", "aEd1#man@gR"),
                        host = Sys.getenv("host", "127.0.0.1"),
                        dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM geonamed_datavalues
                           WHERE orig_name IN ('datasus_popmun')")

mx <- dbdbase |>
  dplyr::group_by(refdate, estado) |>
  dplyr::mutate(maxpop = maxsna(value)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, maxpopestadual = maxpop)

AEDi:::gravar_serie_dw("maxpopestadual",
  data.frame(local = mx$local_id,
             periodo = mx$refdate,
             valor = mx$maxpopestadual))
DBI::dbDisconnect(con)
