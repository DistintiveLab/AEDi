# profissionais_de_saude_pc (mdata 79): profissionais de saude per capita.
# Recalculo completo (replace) a partir das series do DW
# (profissionais_de_saude / datasus_popmun). Padrao A5b.

# conexao LOCAL incondicional: nunca herdar con de sessao (pode apontar
# para o painelpndr remoto, onde as matviews tem outros dados)
con <- DBI::dbConnect(RPostgres::Postgres(),
                        user = Sys.getenv("user", "aedi"),
                        password = Sys.getenv("password", "aEd1#man@gR"),
                        host = Sys.getenv("host", "127.0.0.1"),
                        dbname = Sys.getenv("dbname", "aedidb"))

dbdbase <- DBI::dbGetQuery(con, "SELECT * FROM named_datavalues
                           WHERE orig_name IN ('profissionais_de_saude','datasus_popmun')")
dbdbase <- dbdbase |>
  dplyr::mutate(data_freq_id = max(data_freq_id)) |>
  tidyr::pivot_wider(names_from = 'orig_name', values_from = 'value',
                     id_cols = c('local_id', 'refdate'))

pc <- dbdbase |>
  dplyr::rename(a = 'profissionais_de_saude', b = 'datasus_popmun') |>
  dplyr::transmute(profissionais_de_saude_pc = a / b, refdate, local_id)

AEDi:::gravar_serie_dw("profissionais_de_saude_pc",
  data.frame(local = pc$local_id,
             periodo = pc$refdate,
             valor = pc$profissionais_de_saude_pc))
DBI::dbDisconnect(con)
