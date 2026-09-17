# objetivo4_3_municipio_dw.R ----------------------------------------------
# RESTAURACAO (2026-09-17) da serie MUNICIPAL do objetivo4_3 (mdata 54,
# Coeficiente de Diversificacao Economica) nos dois DWs (aedidb local e
# painelpndr remoto 10.214.50.169).
#
# CAUSA DA PERDA: coleta/objetivo4_3_diversificacao.R (padrao A5b, 2025)
# gravou o indicador apenas em nivel de UF; o append do gravar_serie_dw
# faz DELETE por refdate e removeu as linhas municipais de TODOS os anos
# (2013-2025) nos dois bancos (restaram 27 UFs/ano + 5 linhas espurias de
# Guajara, local_id 99). As matviews municipais objetivo4_20XX (e a camada
# objetivo4_3_2024 do GeoINTEGRA que as consulta) ficaram em branco.
#
# Este script recalcula MUNI + UF direto do mte_rais local (vinculos
# ativos 31/12, setor = cnae_2_0_classe/1000, mesma formula do
# objetivo4_3_aedi.R: metade da soma dos desvios absolutos entre a
# participacao setorial local e a nacional), confere a UF regenerada
# contra a ja gravada, faz backup do estado atual e REGRAVA a serie
# completa — muni+UF JUNTOS, para o append nao apagar as UFs de novo.
# Ao final CRIA (se faltar) e atualiza as matviews objetivo4_*.
#
# Rodar a partir da raiz do AEDi:
#   local:  Rscript coleta/objetivo4_3_municipio_dw.R
#   remoto: R_ENVIRON_USER=/dev/null env user=usr_cggi_admin \
#             password='anbhdbregrvdf@2024' host=10.214.50.169 \
#             dbname=painelpndr mte_rais=mte_rais \
#             pwdrais='aEd1#man@gRpublicrais' hostraispsql=127.0.0.1 \
#             Rscript coleta/objetivo4_3_municipio_dw.R
# (R_ENVIRON_USER=/dev/null impede o .Renviron do repo de sequestrar
# user/host/dbname — mesmo GOTCHA do D3 em roadmap_gastos_tributarios.md)

suppressMessages({library(DBI); library(dplyr)})
pkgload::load_all(".")

anos <- 2013:2025

rais <- dbConnect(RPostgreSQL::PostgreSQL(),
  dbname = Sys.getenv("mte_rais", "mte_rais"), user = "mte_rais",
  password = Sys.getenv("pwdrais"), host = Sys.getenv("hostraispsql", "127.0.0.1"))

vinculos_ano_setor <- \(ano) {
  a <- dbGetQuery(rais, sprintf(
    "SELECT municipio, trunc(cnae_2_0_classe/1000) setor, COUNT(*)::int qtd_vinc
     FROM rais_vinculo_%d
     WHERE vinculo_ativo_31_12 = 1 AND municipio BETWEEN 110001 AND 539999
     GROUP BY municipio, setor", ano))
  a$ano <- ano
  a
}
cat("Lendo rais_vinculo_", min(anos), "-", max(anos), " (agregacao SQL por municipio x setor)...\n", sep = "")
emprego <- data.table::rbindlist(lapply(anos, vinculos_ano_setor))
dbDisconnect(rais)
cat("Agregado:", nrow(emprego), "linhas municipio x setor x ano\n")

calc_div <- function(df, chave) {
  df |>
    group_by({{chave}}, ano, setor) |>
    summarise(vinc_setor = sum(qtd_vinc), .groups = "drop") |>
    group_by({{chave}}, ano) |>
    mutate(vinc_local = sum(vinc_setor)) |>
    ungroup() |>
    group_by(setor, ano) |>
    mutate(vinc_setor_br = sum(vinc_setor)) |>
    ungroup() |>
    group_by(ano) |>
    mutate(vinc_br = sum(vinc_setor)) |>
    ungroup() |>
    mutate(desvio = abs(vinc_setor / vinc_local - vinc_setor_br / vinc_br)) |>
    group_by({{chave}}, ano) |>
    summarise(valor = sum(desvio) / 2, .groups = "drop")
}

base <- emprego |> mutate(uf = trunc(municipio / 1e4))
serie_muni <- calc_div(base, municipio)
serie_uf   <- calc_div(base, uf)

cat("\n== validacoes ==\n")
cob <- serie_muni |> count(ano, name = "n_muni")
print(as.data.frame(cob))
stopifnot(all(cob$n_muni >= 5500))                 # cobertura municipal plena
stopifnot(all(count(serie_uf, ano)$n == 27))       # 27 UFs por ano
stopifnot(all(serie_muni$valor >= 0 & serie_muni$valor <= 1))

# conferencia INFORMATIVA: UF regenerada vs UF gravada (mesma fonte/formula
# do objetivo4_3_diversificacao.R). Nao pode ser stopifnot: no DW local os
# valores gravados podem estar contaminados por execucoes anteriores deste
# proprio script (append idempotente auto-cura na gravacao abaixo); so no
# remoto (estado original) a comparacao e contra os valores genuinamente
# originais (diff esperado <= ~3.25e-05).
con <- dbConnect(RPostgres::Postgres(),
  user = Sys.getenv("user", "aedi"), password = Sys.getenv("password", "aEd1#man@gR"),
  host = Sys.getenv("host", "127.0.0.1"), dbname = Sys.getenv("dbname", "aedidb"))
uf_gravada <- dbGetQuery(con, "SELECT l.geoloc_id::int uf, dv.refdate, dv.value
  FROM data_values dv JOIN local l ON dv.local_id = l.local_id
  WHERE dv.mdata_id = 54 AND l.geoloc_id BETWEEN 11 AND 53")
comp <- serie_uf |>
  transmute(uf, refdate = as.Date(paste0(ano, "-12-31")), regen = valor) |>
  inner_join(uf_gravada, by = c("uf", "refdate"))
if (nrow(comp)) {
  dif_max <- max(abs(comp$regen - comp$value))
  cat(sprintf("UF regenerada vs gravada: %d pontos, |diff| max = %.3g%s\n",
              nrow(comp), dif_max,
              if (dif_max < 1e-3) " (OK)" else
                " (ATENCAO: gravado pode estar contaminado por execucao anterior)"))
  # AVISO: diffs pequenos (~1e-5) sao esperados — o objetivo4_3_diversificacao.R
  # incluia vinculos de municipio "ignorado" (999999) nos totais nacionais;
  # este script os exclui (municipio BETWEEN 110001 AND 539999). A regenerada
  # e internamente consistente (muni e UF da mesma base).
} else {
  cat("sem UF gravada comparavel (estado contaminado por execucoes anteriores)\n")
}

# backup do estado atual (mdata 54 inteiro) antes de regavar
dir.create("coleta/cache/objetivo4_3_restore", recursive = TRUE, showWarnings = FALSE)
saveRDS(dbGetQuery(con, "SELECT * FROM data_values WHERE mdata_id = 54"),
        sprintf("coleta/cache/objetivo4_3_restore/pre_restore_%s_%s.rds",
                Sys.getenv("host", "127.0.0.1"), format(Sys.Date())))

# mapeamento local -> geoloc_id feito AQUI, nao no gravar_serie_dw: o lookup
# triplo dele e INDEXADO POR geoloc_id e cola em duas direcoes — codigos RAIS
# 6d batem com geoloc_ids de REGIOES IMEDIATAS (ex. 110001 Porto Velho ->
# 6431) e local_ids municipais 4d batem com geoloc_ids de MESORREGIOES
# (1101..5308). A unica chave segura e o geoloc_id COMPLETO: 7d para
# municipios (match pelo prefixo 6d do RAIS) e 2d (11..53) para UFs.
locm <- dbGetQuery(con, "SELECT local_id, geoloc_id FROM local WHERE local_id <= 5570")
locm$cod6 <- as.numeric(substr(as.character(locm$geoloc_id), 1, 6))
muni_df <- serie_muni |>
  transmute(municipio,
            local = locm$geoloc_id[match(municipio, locm$cod6)],
            periodo = as.Date(paste0(ano, "-12-31")), valor)
if (anyNA(muni_df$local)) {
  cat("AVISO: codigo(s) RAIS sem local_id municipal no DW (descartados):",
      paste(sort(unique(muni_df$municipio[is.na(muni_df$local)])), collapse = ", "),
      "\n")   # ex. 510183 (MT): existe no RAIS 2024/2025, nao existe no local
}
muni_df <- muni_df |> filter(!is.na(local))
uf_df <- serie_uf |>
  transmute(local = uf,
            periodo = as.Date(paste0(ano, "-12-31")), valor)
stopifnot(all(uf_df$local >= 11 & uf_df$local <= 53),
          all(nchar(as.character(muni_df$local)) == 7))
serie <- bind_rows(select(muni_df, local, periodo, valor), uf_df)
dbDisconnect(con)
cat("\nGravando", nrow(serie), "pontos (muni+UF) em",
    Sys.getenv("dbname", "aedidb"), "@", Sys.getenv("host", "127.0.0.1"), "...\n")
gravar_serie_dw("objetivo4_3", serie, modo = "append")

# matviews municipais objetivo4_* (base da camada objetivo4_3 do GeoINTEGRA):
# cria as que faltarem para os anos da serie e da refresh em todas. As
# definicoes DIFEREM entre os DWs (remoto: recortes_geograficos com colunas
# objetivo4_1/4_2/4_3/comp_objetivo4; local: estilo antigo objetivo1_1-3
# ancorado no mdata 52) — por isso a criacao copia a definicao de uma matview
# existente trocando TODOS os literais de ano (padrao "(AAAA)::", cobre
# ::numeric e ::double precision).
con <- dbConnect(RPostgres::Postgres(),
  user = Sys.getenv("user", "aedi"), password = Sys.getenv("password", "aEd1#man@gR"),
  host = Sys.getenv("host", "127.0.0.1"), dbname = Sys.getenv("dbname", "aedidb"))
mvs <- dbGetQuery(con, "SELECT matviewname FROM pg_matviews
                  WHERE matviewname ~ '^objetivo4_' ORDER BY 1")$matviewname
if (length(mvs)) {
  anos_mv <- as.integer(sub("objetivo4_", "", mvs))
  modelo <- dbGetQuery(con, sprintf(
    "SELECT definition FROM pg_matviews WHERE matviewname = '%s'", mvs[1]))$definition
  novos <- setdiff(anos, anos_mv)
  for (ano in novos) {
    d <- gsub("\\(20[0-9]{2}\\)::", sprintf("(%d)::", ano), modelo)
    dbExecute(con, sprintf("DROP MATERIALIZED VIEW IF EXISTS objetivo4_%d", ano))
    dbExecute(con, sprintf("CREATE MATERIALIZED VIEW objetivo4_%d AS %s", ano, d))
    cat("criada: objetivo4_", ano, "\n", sep = "")
  }
  for (mv in sprintf("objetivo4_%d", sort(c(anos_mv, novos)))) {
    dbExecute(con, sprintf("REFRESH MATERIALIZED VIEW %s", mv))
    cat("refresh:", mv, "\n")
  }
}

res <- dbGetQuery(con, "SELECT extract(year from refdate)::int ano,
    count(*) FILTER (WHERE local_id BETWEEN 1 AND 5570)::int n_muni,
    count(*) FILTER (WHERE local_id > 5570)::int n_outros,
    count(*)::int total
  FROM data_values WHERE mdata_id = 54 GROUP BY 1 ORDER BY 1")
cat("\n== estado final mdata 54 (", Sys.getenv("dbname", "aedidb"), ") ==\n", sep = "")
print(res)
stopifnot(all(res$n_outros == 27),             # so UFs fora do municipal
          all(res$n_muni %in% c(5569, 5570)))  # cobertura municipal (2015: 5569)
dbDisconnect(con)
cat("OK\n")
