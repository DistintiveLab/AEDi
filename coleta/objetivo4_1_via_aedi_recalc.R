# objetivo4_1_via_aedi (103): indice de especializacao agricola
# (proporcao agricola municipal / proporcao agricola brasileira).
# Mesma formula do objetivo4_1 (52), serie _via_aedi. Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

pega <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1 AND cnae_2_0_classe < 4000 GROUP BY municipio"))
  a$ano <- ano
  a
}
ag <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), pega))
DBI::dbDisconnect(rais)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
efm <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) local,
        extract(year from d.refdate)::int ano, d.value
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'emprego_formal_municipal'")
DBI::dbDisconnect(con)

x <- efm |>
  dplyr::left_join(ag, by = c("local", "ano")) |>
  dplyr::mutate(qtd_vinculos_agr = dplyr::coalesce(qtd_vinculos_agr, 0)) |>
  dplyr::mutate(propagr = qtd_vinculos_agr / value) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(brmediaagr = sum(qtd_vinculos_agr, na.rm = TRUE) / sum(value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(valor = propagr / brmediaagr,
                valor = ifelse(is.na(valor), 0, valor)) |>
  dplyr::transmute(local, periodo = as.Date(paste0(ano, "-12-31")), valor)

AEDi:::gravar_serie_dw("objetivo4_1_via_aedi",
  data.frame(local = x$local, periodo = x$periodo, valor = x$valor))
