# indicadores_transf_renuncias.R -----------------------------------------
# Fase D (roadmap_gastos_tributarios.md, D4): indicadores DERIVADOS de
# transferências e gastos tributários como mdata proprios no star schema
# (data_values + mdata + mdata_exts + mdata_timetable), a la AEDi.
#
# Cinco mdata (ids 107-111, ver specs abaixo):
#   107 transf_uniao_pc_real    Transferências da União per capita (R$ de
#                               {base}, IGP-M) — transfpb/pc da referencia
#   108 transf_uniao_pib        Transferências / PIB municipal (%)
#   109 transf_uniao_massa_sal  Transferências / massa salarial (%)
#   110 renuncia_fiscal_pc_real Gastos tributários federais per capita
#                               (R$ de {base}, IGP-M)
#   111 renuncia_fiscal_pib     Gastos tributários federais / PIB municipal (%)
#
# Insumos: TODOS lidos do aedidb LOCAL (127.0.0.1):
#   - transf_uniao_municipio (D2; copiada do remoto 2026-09-17, 2014-2025)
#   - gasto_tributario_municipio grupo 'total' (D3/D3b, 2022-2025)
#   - data_values mdata 66 (população IBGE-TCU, refdate 07-01) e 89 (massa
#     salarial RAIS vl_remun_dezembro_nom_agr, refdate 12-31)
#   - PIB municipal: SIDRA v3 tabela 5938 variavel 37 (mil R$, publicado
#     ate 2021) + camada de EXTRAPOLAÇÃO explicita para >=2022 (metodo da
#     referencia dev/transferencias_comparacoes.R): fator_municipio =
#     min(PIB/massa) em 2018-2021; PIB(ano>=2022) = max(massa)*fator*
#     1.02^(ano-2022), em R$ (mil R$ x 1e3).
#   - IGP-M (ipeadatar IGP_IGPMG, variação mensal): índice de dezembro,
#     rebasado para o último dezembro = 100 => R$ de {base} = último ano
#     fechado. Deflator real de transf_uniao_pc_real e
#     renuncia_fiscal_pc_real (renuncias 2024/2025 já são projeção NOMINAL
#     da DGT Q XXVI — deflacionar as coloca em R$ de {base}).
#
# Gravação: mdata_refdate = 12-31 do ano (convenção dominante do DW, 93
# mdata). Alvo por env D4_ALVO: "local" (padrão) grava no aedidb local;
# "remoto" grava no painelpndr 10.214.50.169 (creds do .Renviron — as
# mesmas env userdb/passwddbdev/hostdbdev/tdbname). Idempotente: se o
# mdata já existe (por orig_name), reaproveita o id e regrava os pontos
# (DELETE + INSERT). datasource "AEDi - estimação própria" é criado no
# alvo se não existir (local: id 19; remoto: max+1).
#
# Rodar a partir da raiz do pndr_dashboard (cache relativo). Cópia
# sincronizada em AEDi/coleta (com .ignore: indicador derivado, o
# agendamento do AEDi não deve rodá-lo — D2/D3 o alimentam).

suppressMessages({
  library(DBI); library(dplyr); library(tidyr); library(httr)
  library(jsonlite); library(stringi)
})

tiracento <- \(x) toupper(stri_trans_general(x, "latin-ascii"))

cache_dir <- "coleta/cache/indicadores_transf"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

loc <- dbConnect(RPostgres::Postgres(),
                 user = "aedi", password = "aEd1#man@gR",
                 host = "127.0.0.1", dbname = "aedidb")

alvo_nm <- Sys.getenv("D4_ALVO", "local")
alvo <- if (alvo_nm == "remoto") {
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("tdbname"), user = Sys.getenv("userdb"),
            password = Sys.getenv("passwddbdev"), host = Sys.getenv("hostdbdev"))
} else loc
cat(sprintf("alvo de gravação: %s\n", alvo_nm))

# ---- insumos do DW local ----------------------------------------------

lmap <- dbGetQuery(loc, "SELECT local_id, geoloc_id::int AS codmun7 FROM local WHERE local_id < 5571")

transf_tot <- dbGetQuery(loc, "
  SELECT ano, local_id, sum(valor) AS valor
    FROM transf_uniao_municipio GROUP BY 1, 2")
stopifnot(nrow(transf_tot) > 0)

gt_tot <- dbGetQuery(loc, "
  SELECT ano AS ano_gt, local_id, sum(valor) AS valor
    FROM gasto_tributario_municipio WHERE grupo = 'total' GROUP BY 1, 2")
stopifnot(nrow(gt_tot) > 0)

pop <- dbGetQuery(loc, "
  SELECT extract(year from refdate)::int AS ano, local_id, value AS pop
    FROM data_values WHERE mdata_id = 66")
massa <- dbGetQuery(loc, "
  SELECT extract(year from refdate)::int AS ano, local_id, value AS massa
    FROM data_values WHERE mdata_id = 89")
cat(sprintf("pop (mdata 66): %d pts %d-%d | massa (mdata 89): %d pts %d-%d\n",
            nrow(pop), min(pop$ano), max(pop$ano),
            nrow(massa), min(massa$ano), max(massa$ano)))

anos <- sort(unique(c(transf_tot$ano, gt_tot$ano_gt)))
cat("anos:", paste(anos, collapse = ", "), "\n")

# ---- PIB municipal 5938 var 37 (mil R$) + extrapolação >= 2022 --------

sidra_v3 <- \(tabela, variavel, periodo) {
  arq <- file.path(cache_dir, sprintf("sidra_%s_%s_%s.rds", tabela, variavel, periodo))
  if (file.exists(arq)) return(readRDS(arq))
  url <- sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/periodos/%s/variaveis/%s?localidades=N6[all]",
                tabela, periodo, variavel)
  r <- NULL
  for (i in 1:5) {
    r <- tryCatch(GET(url, user_agent("Mozilla/5.0"), timeout(120)), error = \(e) NULL)
    if (!is.null(r) && status_code(r) == 200) break
    cat("  SIDRA indisponivel (tentativa", i, "/5), aguardando 60s...\n"); Sys.sleep(60); r <- NULL
  }
  if (is.null(r)) stop("SIDRA fora do ar e sem cache: ", url)
  res <- fromJSON(content(r, "text"), simplifyVector = TRUE)
  rd <- res$resultados[[1]]
  d <- lapply(seq_len(nrow(rd)), \(j) {
    s <- rd$series[[j]]; sv <- s$serie
    if (is.data.frame(sv)) {
      data.frame(codmun7 = as.integer(s$localidade$id),
                 valor = suppressWarnings(as.numeric(sv[[1]])))
    } else {
      if (!is.list(sv)) sv <- list(sv)
      data.frame(codmun7 = as.integer(s$localidade$id),
                 valor = vapply(sv, \(x) suppressWarnings(as.numeric(x[[1]])),
                                numeric(1)))
    }
  }) |> bind_rows() |> filter(!is.na(valor))
  stopifnot(nrow(d) > 0)
  saveRDS(d, arq)
  d
}

ano_pub <- max(anos[anos <= 2021])   # ultimo publicado (PIB municipal)
pib_l <- bind_rows(lapply(2014:ano_pub, \(a)
  sidra_v3(5938, 37, a) |> mutate(ano = a))) |>
  rename(pib_mil = valor) |> mutate(codmun7 = as.integer(codmun7))
cat(sprintf("PIB 5938 var 37: %d munis x %d anos publicados (%d-%d)\n",
            n_distinct(pib_l$codmun7), n_distinct(pib_l$ano),
            min(pib_l$ano), max(pib_l$ano)))

# extrapolação (camada EXPLÍCITA, metodo da referencia):
base <- lmap |>
  left_join(pib_l, by = "codmun7") |>
  left_join(massa, by = c("local_id", "ano"))
fator_muni <- base |>
  filter(ano >= 2018, ano <= 2021, massa > 0, pib_mil > 0) |>
  group_by(local_id) |>
  summarise(fator = min(pib_mil * 1e3 / massa), .groups = "drop")
massa_max <- massa |> group_by(local_id) |> summarise(massa_max = max(massa), .groups = "drop")
n_extrap <- sum(anos >= 2022)
pib <- crossing(local_id = lmap$local_id, ano = anos) |>
  left_join(base |> select(local_id, ano, pib_mil), by = c("local_id", "ano")) |>
  left_join(fator_muni, by = "local_id") |>
  left_join(massa_max, by = "local_id") |>
  mutate(pib_rs = if_else(ano >= 2022,
            massa_max * fator * 1.02^(ano - 2022),
            pib_mil * 1e3)) |>
  filter(!is.na(pib_rs))
cat(sprintf("extrapolação PIB >=2022: %d munis com fator (%.1f%% de %d); %d anos extrapolados\n",
            nrow(fator_muni), 100 * nrow(fator_muni) / nrow(lmap), nrow(lmap), n_extrap))

# ---- IGP-M: índice de dezembro rebasado (R$ do último ano fechado) -----

arq_igpm <- file.path(cache_dir, "igpm_dez.rds")
if (file.exists(arq_igpm)) {
  igpm <- readRDS(arq_igpm)
} else {
  # ipeadata odata4 direto (ipeadatar da timeout aqui; mesmos dados):
  # IGP_IGPMG = variacao ANUAL (%) do IGP-M (36 obs, 1990-)
  url_igpm <- "http://www.ipeadata.gov.br/api/odata4/Metadados('IGP_IGPMG')/Valores"
  r <- NULL; vv <- NULL
  for (i in 1:5) {
    r <- tryCatch(GET(url_igpm, user_agent("Mozilla/5.0"), timeout(180)), error = \(e) NULL)
    ok <- !is.null(r) && status_code(r) == 200
    if (ok) vv <- tryCatch(fromJSON(content(r, "text"))$value, error = \(e) NULL)
    if (ok && NROW(vv) > 0) break
    cat("  ipeadata indisponivel (tentativa", i, "/5), aguardando 60s...\n"); Sys.sleep(60); r <- NULL; vv <- NULL
  }
  if (is.null(vv) || NROW(vv) == 0) stop("ipeadata fora do ar e sem cache: ", url_igpm)
  vv <- vv[order(substr(vv$VALDATA, 1, 4)), ]
  igpm <- data.frame(ano = as.integer(substr(vv$VALDATA, 1, 4)),
                     indice = 100 * cumprod(1 + as.numeric(vv$VALVALOR) / 100))
  igpm$indice <- 100 * igpm$indice / tail(igpm$indice, 1)
  saveRDS(igpm, arq_igpm)
}
base_ano <- max(igpm$ano)
cat(sprintf("IGP-M: dezembro, base %d = 100 (%d-%d)\n", base_ano, min(igpm$ano), base_ano))

# ---- série dos 5 indicadores ------------------------------------------

transf <- transf_tot |>
  left_join(pop, by = c("ano", "local_id")) |>
  left_join(massa, by = c("ano", "local_id")) |>
  left_join(pib |> select(local_id, ano, pib_rs), by = c("ano", "local_id")) |>
  left_join(igpm, by = "ano")

gt <- gt_tot |>
  rename(ano = ano_gt) |>
  left_join(pop, by = c("ano", "local_id")) |>
  left_join(pib |> select(local_id, ano, pib_rs), by = c("ano", "local_id")) |>
  left_join(igpm, by = "ano")

series <- list(
  list(107L, "transf_uniao_pc_real",
       sprintf("Transferências da União ao município per capita (R$ de %d, IGP-M)", base_ano),
       "Total anual transferido pela União ao município (todas as naturezas), deflacionado pelo IGP-M e dividido pela população IBGE-TCU.",
       "R$", "habitante",
       transf |> filter(pop > 0, !is.na(indice)) |>
         mutate(valor = valor * 100 / indice / pop) |> select(local_id, ano, valor)),
  list(108L, "transf_uniao_pib",
       "Transferências da União / PIB municipal (%)",
       "Total anual transferido pela União em relação ao PIB municipal; PIB 5938 (IBGE) e, a partir de 2022, extrapolação explícita (massa salarial RAIS x razão histórica mínima, cresc. 2% a.a.).",
       "%", NA_character_,
       transf |> filter(pib_rs > 0) |>
         mutate(valor = 100 * valor / pib_rs) |> select(local_id, ano, valor)),
  list(109L, "transf_uniao_massa_sal",
       "Transferências da União / massa salarial (%)",
       "Total anual transferido pela União em relação à massa salarial de dezembro (RAIS, vl_remun_dezembro_nom_agr).",
       "%", NA_character_,
       transf |> filter(massa > 0) |>
         mutate(valor = 100 * valor / massa) |> select(local_id, ano, valor)),
  list(110L, "renuncia_fiscal_pc_real",
       sprintf("Gastos tributários federais per capita (R$ de %d, IGP-M)", base_ano),
       "Municipalização dos gastos tributários federais (DGT/RFB bases efetivas; 2024-2025 projeção Q XXVI) per capita, deflacionada pelo IGP-M.",
       "R$", "habitante",
       gt |> filter(pop > 0, !is.na(indice)) |>
         mutate(valor = valor * 100 / indice / pop) |> select(local_id, ano, valor)),
  list(111L, "renuncia_fiscal_pib",
       "Gastos tributários federais / PIB municipal (%)",
       "Municipalização dos gastos tributários federais em relação ao PIB municipal (extrapolação explícita a partir de 2022).",
       "%", NA_character_,
       gt |> filter(pib_rs > 0) |>
         mutate(valor = 100 * valor / pib_rs) |> select(local_id, ano, valor))
)

# sanity: participação agregada das transferências no PIB (nacional, ~4-6%)
chk <- transf |> filter(!is.na(pib_rs)) |>
  group_by(ano) |> summarise(part = sum(valor) / sum(pib_rs), .groups = "drop")
cat("participação agregada transf/PIB por ano:\n"); print(as.data.frame(chk), row.names = FALSE)

# ---- gravação no alvo --------------------------------------------------

ds_id <- dbGetQuery(alvo, "
  SELECT datasource_id FROM datasource
   WHERE lower(datasource_name) LIKE '%aedi%'")$datasource_id[1]
if (is.na(ds_id)) {
  nxt <- dbGetQuery(alvo, "SELECT coalesce(max(datasource_id),0)+1 AS n FROM datasource")$n
  dbExecute(alvo, "INSERT INTO datasource (datasource_id, datasource_name)
                   VALUES ($1, 'AEDi - estimação própria')", params = list(nxt))
  ds_id <- nxt
}
cat(sprintf("datasource: %d\n", ds_id))

OBS <- paste("D4: indicador derivado (coleta/indicadores_transf_renuncias.R).",
             "Camadas: PIB municipal extrap. >=2022; IGP-M base",
             base_ano, "= 100; renúncias 2024-2025 = projeção DGT Q XXVI.")

for (s in series) {
  id <- s[[1]]; nome <- s[[2]]; dname <- s[[3]]; desc <- s[[4]]
  un <- s[[5]]; ud <- s[[6]]; dd <- s[[7]]
  stopifnot(nrow(dd) > 0, !anyNA(dd$valor), all(is.finite(dd$valor)))

  ja <- dbGetQuery(alvo, "SELECT mdata_id FROM mdata WHERE orig_name = $1",
                   params = list(nome))$mdata_id
  dbBegin(alvo)
  tryCatch({
    if (length(ja) == 0) {
      dbExecute(alvo, "INSERT INTO mdata (mdata_id, orig_name, data_name, data_desc)
                       VALUES ($1,$2,$3,$4)", params = list(id, nome, dname, desc))
      dbExecute(alvo, "
        INSERT INTO mdata_exts (mdata_id, data_class_id, data_freq_id,
                                dataunit_num, dataunit_den, data_type_id,
                                datasource_id, mdata_obs)
        VALUES ($1, 2, 9, $2, $3, 2, $4, $5)",
        params = list(id, un, ud, ds_id, OBS))
    } else {
      id <- ja
      dbExecute(alvo, "UPDATE mdata SET data_name=$2, data_desc=$3 WHERE mdata_id=$1",
                params = list(id, dname, desc))
    }
    dbExecute(alvo, "DELETE FROM data_values WHERE mdata_id = $1", params = list(id))
    dbAppendTable(alvo, "data_values",
                  data.frame(mdata_id = id,
                             local_id = dd$local_id,
                             refdate = as.Date(sprintf("%d-12-31", dd$ano)),
                             value = dd$valor))
    dbExecute(alvo, "DELETE FROM mdata_timetable WHERE mdata_id = $1", params = list(id))
    dbExecute(alvo, "INSERT INTO mdata_timetable (mdata_id, last_refdate, last_update)
                     VALUES ($1, $2, current_date)",
              params = list(id, as.Date(sprintf("%d-12-31", max(dd$ano)))))
    dbCommit(alvo)
    cat(sprintf("%-24s mdata %3d: %5d pts %d-%d (mediana %.4g %s)\n",
                nome, id, nrow(dd), min(dd$ano), max(dd$ano),
                median(dd$valor), un))
  }, error = \(e) { dbRollback(alvo); stop(nome, ": ", conditionMessage(e)) })
}

dbDisconnect(loc)
if (alvo_nm == "remoto") dbDisconnect(alvo)
cat("OK: indicadores D4 gravados\n")
