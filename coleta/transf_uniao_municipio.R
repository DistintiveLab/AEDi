# transf_uniao_municipio.R ------------------------------------------------
# Fase D (roadmap_gastos_tributarios.md, D2): transferências da União para
# municípios como FATO no aedidb (não é data_values/mdata — granularidade
# município x ano x tipo x favorecido não cabe no star schema).
#
# Fonte: Portal da Transparência (CGU), séries mensais
#   https://dadosabertos-download.cgu.gov.br/PortalDaTransparencia/saida/transferencias/YYYYMM_Transferencias.zip
# (mesma distribuição que transfRgov::download_transferencias_uniao usa).
#
# O que faz:
#   1. Para cada mês fechado de 2014 até o último mês completo, baixa o ZIP e
#      agrega para municipio(SIAFI) x tipo_transferencia x grupo_favorecido,
#      guardando CSV pequeno em coleta/cache/transf_uniao_municipio/YYYYMM.csv
#      (idempotente: mês já em cache não é rebaixado).
#   2. Consolida os meses em ANO, traduz SIAFI->IBGE (tabela oficial via
#      transfRgov::municipios_siafi_ibge) e IBGE->local_id (tabela local do DW).
#   3. Upsert transacional (DELETE+INSERT por ano) na tabela
#      transf_uniao_municipio. Só carrega anos com os 12 meses em cache.
#
# Dimensões:
#   tipo_transferencia: rótulo original da fonte ("Constitucionais e
#     Royalties" | "Legais, Voluntárias e Específicas"), estável 2014-2025.
#   grupo_favorecido: publica (administração pública em qualquer esfera),
#     osc (entidades sem fins lucrativos), privada (empresas), outra
#     (agentes intermediários, organizações internacionais, pessoa física,
#     sem informação). Linhas SEM código SIAFI de município são descartadas
#     (são transferências a estados/exterior, fora do escopo municipal).
#
# Rodar a partir da raiz do repositório AEDi (cache relativo).
# Conexão via .Renviron do AEDi (produção: painelpndr@10.214.50.169); para
# piloto local: export tdbname=aedidb userdb=aedi passwddbdev=... hostdbdev=127.0.0.1

suppressMessages({
  library(DBI)
  library(dplyr)
  library(readr)
  library(janitor)
  library(httr)
})

ano_inicio <- as.integer(Sys.getenv("TRANSF_ANO_INICIO", "2014"))
cache_dir  <- "coleta/cache/transf_uniao_municipio"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

## 1. descobrir meses a processar (só meses FECHADOS) ----------------------
hoje       <- Sys.Date()
ultimo_fechado <- seq(as.Date(format(hoje, "%Y-%m-01")), by = "-1 month", length.out = 1)[1] - 1
ymes <- expand.grid(ano = ano_inicio:as.integer(format(ultimo_fechado, "%Y")),
                    mes = 1:12) |>
  transmute(ym = sprintf("%04d%02d", ano, mes)) |>
  filter(as.Date(paste0(ym, "01"), format = "%Y%m%d") <= ultimo_fechado) |>
  pull(ym)

## 2. baixar + agregar por mês (cache) --------------------------------------
recod_favorecido <- c(
  "Administração Pública"                                       = "publica",
  "Administração Pública Municipal"                             = "publica",
  "Administração Pública Estadual ou do Distrito Federal"       = "publica",
  "Administração Pública Federal"                               = "publica",
  "Entidades Sem Fins Lucrativos"                               = "osc",
  "Entidades Empresariais Privadas"                             = "privada"
)

espere_desbloqueio <- function(url) {
  # CloudFront bloqueia o IP por ~5 min quando recebe rajada: esperar até um
  # HEAD voltar 200 antes de tentar o download de novo (max ~15 min).
  for (i in 1:15) {
    cat(sprintf("    bloqueado, aguardando 60s (tentativa %d/15)\n", i)); Sys.sleep(60)
    r <- tryCatch(httr::HEAD(url, httr::timeout(30)), error = function(e) NULL)
    if (!is.null(r) && httr::status_code(r) == 200) return(TRUE)
  }
  FALSE
}

baixa_mes <- function(ym) {
  arq_cache <- file.path(cache_dir, paste0(ym, ".csv"))
  if (file.exists(arq_cache)) return(invisible(NULL))
  url <- paste0("https://dadosabertos-download.cgu.gov.br/PortalDaTransparencia/",
               "saida/transferencias/", ym, "_Transferencias.zip")
  zip_tmp <- tempfile(fileext = ".zip")
  # pacing entre meses para não derrubar o rate limit do CloudFront
  Sys.sleep(as.numeric(Sys.getenv("TRANSF_PAUSA_S", "8")))
  ok <- FALSE
  for (tentativa in 1:4) {
    ok <- tryCatch({ download.file(url, zip_tmp, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE,
                   warning = function(w) FALSE)
    if (ok && file.exists(zip_tmp) && file.size(zip_tmp) > 1e4) break
    ok <- FALSE; unlink(zip_tmp)
    if (tentativa < 4 && !espere_desbloqueio(url)) break
  }
  if (!ok) { cat(sprintf("  %s: FALHA download apos retries (%s)\n", ym, basename(url))); return(invisible(NULL)) }
  arqs <- unzip(zip_tmp, exdir = tempdir())
  csv <- arqs[grepl("\\.csv$", arqs, ignore.case = TRUE)][1]
  d <- read_delim(csv, delim = ";", quote = "\"",
                  locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
                  col_types = cols(.default = "c", `VALOR TRANSFERIDO` = col_double()),
                  show_col_types = FALSE) |>
    clean_names()
  unlink(c(zip_tmp, arqs))
  d <- d |>
    filter(!is.na(codigo_municipio_siafi),
           codigo_municipio_siafi != "", codigo_municipio_siafi != "-1",
           !is.na(valor_transferido)) |>
    mutate(grupo_favorecido = ifelse(tipo_favorecido %in% names(recod_favorecido),
                                     recod_favorecido[tipo_favorecido], "outra")) |>
    group_by(ano_mes = as.integer(ym), siafi = codigo_municipio_siafi,
             tipo_transferencia, grupo_favorecido) |>
    summarise(valor = sum(valor_transferido), .groups = "drop")
  write_csv(d, arq_cache)
  cat(sprintf("  %s: baixado, %d grupos agregados\n", ym, nrow(d)))
}

cat("Baixando/agregando meses (cache em ", cache_dir, ")\n", sep = "")
for (ym in ymes) baixa_mes(ym)

## 3. consolidar em ano x local_id ------------------------------------------
arqs <- list.files(cache_dir, pattern = "^\\d{6}\\.csv$", full.names = TRUE)
fato_mes <- lapply(arqs, read_csv, show_col_types = FALSE,
                   col_types = cols(ano_mes = col_integer(), siafi = col_character(),
                                    tipo_transferencia = col_character(),
                                    grupo_favorecido = col_character(),
                                    valor = col_double())) |>
  bind_rows()

map_siafi <- transfRgov::municipios_siafi_ibge |>
  transmute(siafi = sprintf("%04d", as.integer(codigo_municipio_siafi)),
            codmun = as.integer(codigo_ibge)) |>
  filter(codmun > 0)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("tdbname"), user = Sys.getenv("userdb"),
                 password = Sys.getenv("passwddbdev"), host = Sys.getenv("hostdbdev"))
local_dw <- dbGetQuery(con, "SELECT local_id, geoloc_id::int AS codmun FROM local WHERE local_id < 5571")

sem_mapa <- setdiff(unique(fato_mes$siafi), map_siafi$siafi)
if (length(sem_mapa)) {
  v_sm <- sum(fato_mes$valor[fato_mes$siafi %in% sem_mapa], na.rm = TRUE)
  cat(sprintf("AVISO: %d SIAFIs sem mapeamento IBGE (R$ %.1f mi, %.3f%% do total)\n",
              length(sem_mapa), v_sm / 1e6,
              100 * v_sm / sum(fato_mes$valor, na.rm = TRUE)))
}

fato_ano <- fato_mes |>
  filter(!siafi %in% sem_mapa) |>
  left_join(map_siafi, by = "siafi") |>
  left_join(local_dw, by = "codmun") |>
  filter(!is.na(local_id)) |>
  mutate(ano = ano_mes %/% 100L) |>
  group_by(ano, local_id, tipo_transferencia, grupo_favorecido) |>
  summarise(valor = sum(valor), .groups = "drop")

# só anos completos (12 meses em cache)
meses_por_ano <- fato_mes |> mutate(ano = ano_mes %/% 100L) |>
  distinct(ano, ano_mes) |> count(ano)
anos_completos <- meses_por_ano$ano[meses_por_ano$n == 12]
fato_ano <- filter(fato_ano, ano %in% anos_completos)
cat(sprintf("Anos completos a carregar: %s\n", paste(sort(anos_completos), collapse = ", ")))

## 4. upsert transacional ----------------------------------------------------
dbExecute(con, paste(
  "CREATE TABLE IF NOT EXISTS transf_uniao_municipio (",
  "  ano integer NOT NULL,",
  "  local_id integer NOT NULL,",
  "  tipo_transferencia text NOT NULL,",
  "  grupo_favorecido text NOT NULL,",
  "  valor numeric NOT NULL,",
  "  PRIMARY KEY (ano, local_id, tipo_transferencia, grupo_favorecido))"))
dbBegin(con)
res <- tryCatch({
  dbExecute(con, sprintf("DELETE FROM transf_uniao_municipio WHERE ano IN (%s)",
                         paste(sort(unique(fato_ano$ano)), collapse = ",")))
  dbWriteTable(con, "transf_uniao_municipio", as.data.frame(fato_ano),
               append = TRUE, row.names = FALSE)
  TRUE
}, error = function(e) { dbRollback(con); stop(conditionMessage(e)) })
if (!isTRUE(res)) stop("falha na carga")
dbCommit(con)

chk <- dbGetQuery(con, paste("SELECT ano, count(*) n_mun, round(sum(valor)/1e9,2) bi",
                             "FROM transf_uniao_municipio GROUP BY ano ORDER BY ano"))
print(chk)
dbDisconnect(con)
cat("transf_uniao_municipio: carga concluida\n")
