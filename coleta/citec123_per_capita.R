# citec1 (6), citec2 (7), citec3 (8): per capita de C&T.
# citec1 = micro/pequenas empresas biotec+saude (ja gravado por
#   citec1_biotecsaude.R como citec1_aedi; aqui grava o mdata historico 6)
# citec2 = vinculos em ocupacoes de C&T (CBO 203,234,395) x 1e6/hab
# citec3 = vinculos em CNAE 72 (pesquisa/desenvolvimento) x 1e6/hab
# Fonte mte_rais + popmun. Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))
anos <- AEDi:::anos_rais(rais)

pega_ct <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1
               AND trunc(cbo_ocupacao_2002/1000) IN (203,234,395)
               GROUP BY municipio"))
  a$ano <- ano
  a
}
ct <- data.table::rbindlist(lapply(anos, pega_ct))

pega_cnae72 <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1
               AND trunc(cnae_2_0_classe/1000) = 72 GROUP BY municipio"))
  a$ano <- ano
  a
}
c72 <- data.table::rbindlist(lapply(anos, pega_cnae72))
DBI::dbDisconnect(rais)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) local,
        extract(year from d.refdate)::int ano, d.value pop
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun'")
peq <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) local,
        extract(year from d.refdate)::int ano, d.value peq
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'pequenas_empresas_biotecsaude_mun'")
DBI::dbDisconnect(con)

calc_pc <- function(dados, nome) {
  x <- pop |>
    dplyr::left_join(dados, by = c("local", "ano")) |>
    dplyr::mutate(qtd = dplyr::coalesce(qtd_vinculos, 0)) |>
    dplyr::filter(pop > 0) |>
    dplyr::transmute(local, periodo = as.Date(paste0(ano, "-12-31")),
                     valor = 1e6 * qtd / pop)
  AEDi:::gravar_serie_dw(nome,
    data.frame(local = x$local, periodo = x$periodo, valor = x$valor))
}

calc_pc(ct,   "citec2")
calc_pc(c72,  "citec3")

# citec1 (6) = mesmo calculo do citec1_aedi (102), mdata historico
x1 <- pop |>
  dplyr::left_join(peq, by = c("local", "ano")) |>
  dplyr::mutate(qtd = dplyr::coalesce(peq, 0)) |>
  dplyr::filter(pop > 0) |>
  dplyr::transmute(local, periodo = as.Date(paste0(ano, "-12-31")),
                   valor = 1e6 * qtd / pop)
AEDi:::gravar_serie_dw("citec1",
  data.frame(local = x1$local, periodo = x1$periodo, valor = x1$valor))
