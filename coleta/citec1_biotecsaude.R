# pequenas_empresas_biotecsaude_mun (101) e citec1_aedi (102):
# estabelecimentos de pequeno porte (tamanho < 6) em setores de biotecnologia
# e saude (CNAE 211,212,266,325) por municipio; citec1 = 1e6*estab/pop.
# Fonte direta mte_rais (rais_estabelecimento_). Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

anos_estab <- sort(as.numeric(gsub("\\D", "", grep("^rais_estabelecimento_[0-9]+$",
  DBI::dbGetQuery(rais, "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='public' AND table_name ~ '^rais_estabelecimento_[0-9]+$'")$table_name,
  value = TRUE))))

peq <- data.table::rbindlist(lapply(anos_estab, \(x) {
  # granularidade da coluna cnae varia por epoca: ~2013-2023 = 5 digitos
  # (grupo = /100); 2024-2025 = 7 digitos, subclasse com DV (grupo = /10000)
  p <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio, COUNT(*) estabelecimento FROM rais_estabelecimento_",
           x, " WHERE tamanho_estabelecimento < 6 AND
               (trunc(cnae_2_0_classe/100) IN (211,212,266,325)
                OR trunc(cnae_2_0_classe/10000) IN (211,212,266,325))
               GROUP BY municipio"))
  p$ano <- x
  p
}), fill = TRUE)
readr::write_csv(peq, "coleta/cache/pequenas_empresas_biotecsaude_mun/pequenas_empresas_biotecsaude_mun.csv")
DBI::dbDisconnect(rais)

AEDi:::gravar_serie_dw("pequenas_empresas_biotecsaude_mun",
  data.frame(local = peq$municipio,
             periodo = as.Date(paste0(peq$ano, "-12-31")),
             valor = peq$estabelecimento))

# citec1_aedi: per capita (popmun de julho do mesmo ano -> refdate 31/12)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) codmun,
        extract(year from d.refdate)::int ano, d.value pop
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun'")
DBI::dbDisconnect(con)

c1 <- peq |>
  dplyr::left_join(pop, by = c("municipio" = "codmun", "ano" = "ano")) |>
  dplyr::filter(!is.na(pop), pop > 0) |>
  dplyr::transmute(local = municipio,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   citec1_aedi = 1e6 * estabelecimento / pop)

AEDi:::gravar_serie_dw("citec1_aedi",
  data.frame(local = c1$local, periodo = c1$periodo, valor = c1$citec1_aedi))
