# desprod4 (14): Escala Produtiva = massa salarial (sem adm publica) /
# numero de estabelecimentos (sem adm publica). Fonte mte_rais.
# A massa salarial vem do desprod3 (ja calculada em cache/DW); a contagem
# de estabelecimentos vem de rais_estabelecimento_ com filtro CNAE != adm pub.
# Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

# massa salarial sem adm publica por municipio-ano (mesma query do desprod3)
massa <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, SUM(vl_remun_dezembro_nom) massa_salarial
            FROM rais_vinculo_", ano,
           " WHERE vinculo_ativo_31_12 = 1
              AND (cnae_2_0_classe < 84000 OR cnae_2_0_classe > 84999)
              GROUP BY municipio"))
  a$ano <- ano
  a
}))

# estabelecimentos sem adm publica por municipio-ano
# CNAE granularity: ate 2023 = 5 digitos (classe 84000-84999 = adm pub);
# 2024+ = 7 digitos (subclasse 8400000-8499999 = adm pub)
anos_estab <- sort(as.numeric(gsub("\\D", "", grep("^rais_estabelecimento_[0-9]+$",
  DBI::dbGetQuery(rais, "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='public' AND table_name ~ '^rais_estabelecimento_[0-9]+$'")$table_name,
  value = TRUE))))

estab <- data.table::rbindlist(lapply(anos_estab, \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_estabelecimentos
            FROM rais_estabelecimento_", ano,
           " WHERE (trunc(cnae_2_0_classe / CASE
                      WHEN cnae_2_0_classe > 99999 THEN 100 ELSE 1 END) < 84000
                 OR trunc(cnae_2_0_classe / CASE
                      WHEN cnae_2_0_classe > 99999 THEN 100 ELSE 1 END) > 84999)
              GROUP BY municipio"))
  a$ano <- ano
  a
}))
DBI::dbDisconnect(rais)

readr::write_csv(estab, "coleta/cache/desprod4_aedi/estabelec_sem_admpu_mun.csv")

# desprod4 = massa / estabelecimentos
d4 <- massa |>
  dplyr::inner_join(estab, by = c("local", "ano")) |>
  dplyr::filter(qtd_estabelecimentos > 0) |>
  dplyr::transmute(local,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   desprod4_aedi = massa_salarial / qtd_estabelecimentos)

cat("desprod4:", nrow(d4), "municipios-ano |",
    "anos:", paste(range(as.numeric(format(unique(d4$periodo), "%Y"))), collapse="-"), "\n")

AEDi:::gravar_serie_dw("desprod4",
  data.frame(local = d4$local, periodo = d4$periodo, valor = d4$desprod4_aedi))
