# prof_saude_pc_mediana_nacional (mdata 80): mediana nacional do per capita
# de profissionais de saude por refdate (mesma semantica do builder: cada
# local recebe a mediana nacional do seu refdate). Recalculo completo
# (replace) a partir do DW. Padrao A5b.

# conexao LOCAL incondicional: nunca herdar con de sessao (pode apontar
# para o painelpndr remoto, onde as matviews tem outros dados)
con <- DBI::dbConnect(RPostgres::Postgres(),
                        user = Sys.getenv("user", "aedi"),
                        password = Sys.getenv("password", "aEd1#man@gR"),
                        host = Sys.getenv("host", "127.0.0.1"),
                        dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
                           WHERE orig_name IN ('profissionais_de_saude_pc')")

med <- dbdbase |>
  dplyr::group_by(refdate) |>
  dplyr::mutate(prof_saude_pc_mediana_nacional = median(value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local_id, refdate, prof_saude_pc_mediana_nacional) |>
  dplyr::distinct()

AEDi:::gravar_serie_dw("prof_saude_pc_mediana_nacional",
  data.frame(local = med$local_id,
             periodo = med$refdate,
             valor = med$prof_saude_pc_mediana_nacional))
DBI::dbDisconnect(con)
