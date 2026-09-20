# cnpj_indice_raiz_municipio.R ---------------------------------------------
# D3b (roadmap_gastos_tributarios.md): indice CNPJ raiz (8 digitos) ->
# municipio (codigo SIAFI) com peso = numero de estabelecimentos ATIVOS
# ("02") da raiz no municipio. Alimenta a chave do grupo "demais_tributos"
# em gastos_tributarios_municipio.R (renuncia PJ por estabelecimentos,
# metodo da referencia Produto-5).
#
# Base: espelho HuggingFace `fluowai/datacorp-cnpj-data` (cc-by-4.0,
# snapshot 2026-03), 10 parquets cnpj_prefix_XX_YY.parquet baixados em
# coleta/cache/bases_cnpj/datacorp/ (via baixa_shards). O portal oficial
# arquivos.receitafederal.gov.br esta bloqueado por IP (connection reset em
# todos os caminhos; testado UA, headers, IPv4 forcado, WebDAV) —
# substituicao documentada no header do script principal e no roadmap.
#
# Fidelidade ao metodo de referencia (pndr_relatorios/2025-08-Produto-5/
# dataprep/gastos_tributarios_dados_abertos_rfb_cnpj.R) com UMA substituicao:
#   - o espelho NAO tem `data_inicio` -> o filtro temporal da referencia
#     (data_inicio < ano+1) vira "snapshot atual para qualquer ano";
#   - dedup OBRIGATORIO por (cnpj_base, ordem, dv, sit_cadastral, mun_code,
#     uf): o espelho tem linhas duplicadas (shard 90_99: 800.627 brutas ->
#     456.951 unicas);
#   - filtro: situacao_cadastral == "02" (ativa), uf != EX/BR;
#   - mun_code do espelho ja e codigo SIAFI (casa com siafi_id da base
#     kelvins).
#
# Saida: coleta/cache/bases_cnpj/indice_raiz_municipio.parquet
#        (cnpj_base, mun_code, uf, n_estabs) + relatorio de cobertura
#        contra o agregado PJ da RFB (valor por raiz, colunas por ano).
# Rodar a partir da raiz do repositorio (pndr_dashboard).

suppressMessages({ library(arrow); library(dplyr); library(readxl); library(stringi) })

dir_datacorp <- "coleta/cache/bases_cnpj/datacorp"
arq_indice   <- "coleta/cache/bases_cnpj/indice_raiz_municipio.parquet"
arq_agregado <- "coleta/cache/renuncias/agregado-2015-a-2024-irpj-csll-pis-imp-cofins-imp-ipi-imp-e-ii-5.xlsx"
stopifnot(file.exists(arq_agregado))

shards <- sort(list.files(dir_datacorp, pattern = "^cnpj_prefix_[0-9]{2}_[0-9]{2}\\.parquet$"))
stopifnot(length(shards) == 10L)

## 1. indice raiz x municipio (peso = nº de estabelecimentos ativos) -------
indice <- lapply(shards, \(sh) {
  # filtro (situacao/uf) pushed-down no arrow: coleta so as ~15% ativas
  d <- open_dataset(file.path(dir_datacorp, sh)) |>
    select(cnpj_base, ordem, dv, sit_cadastral, mun_code, uf) |>
    filter(sit_cadastral == "02", !uf %in% c("EX", "BR"),
           !is.na(mun_code), mun_code != 0) |>
    collect()
  n_ativas <- nrow(d)
  out <- d |>
    distinct() |>
    count(cnpj_base, mun_code, uf, name = "n_estabs")
  rm(d); gc(verbose = FALSE)
  cat(sprintf("%s: %d ativas -> %d combos (raiz x mun)\n",
              sh, n_ativas, nrow(out)))
  out
}) |> bind_rows() |>
  count(cnpj_base, mun_code, uf, wt = n_estabs, name = "n_estabs")

cat(sprintf("\nINDICE: %d combos (raiz x mun), %d raizes distintas, %d municipios SIAFI, %.1f mi estabs ativos\n",
            nrow(indice), n_distinct(indice$cnpj_base), n_distinct(indice$mun_code),
            sum(indice$n_estabs) / 1e6))

## 2. cobertura vs agregado PJ (valor por raiz, por ano) -------------------
tiracento <- \(x) toupper(stri_trans_general(x, "latin-ascii"))
ag <- read_xlsx(arq_agregado, skip = 5)
nms <- tiracento(gsub("\\s+", " ", names(ag)))
names(ag) <- nms
col_raiz <- nms[grepl("CNPJ RAIZ", nms)][1]
stopifnot(!is.na(col_raiz))
anos_col <- setNames(
  vapply(nms, \(x) if (grepl("^\\*?(20[0-9]{2})\\b", x))
    as.integer(sub("^\\*?(20[0-9]{2}).*$", "\\1", x)) else NA_integer_,
    integer(1)), nms)
ag_roots <- ag |>
  transmute(cnpj_base = trimws(as.character(.data[[col_raiz]])),
            !!!setNames(lapply(names(anos_col)[!is.na(anos_col)],
                               \(cnm) suppressWarnings(as.numeric(ag[[cnm]]))),
                        paste0("v", anos_col[!is.na(anos_col)]))) |>
  filter(grepl("^[0-9]{8}$", cnpj_base))
cat(sprintf("AGREGADO: %d raizes validas de 8 digitos\n", nrow(ag_roots)))

idx_roots <- distinct(indice, cnpj_base)
cov <- lapply(sort(unique(anos_col[!is.na(anos_col)])), \(aa) {
  cnm <- paste0("v", aa)
  v <- ag_roots[[cnm]]
  ok <- !is.na(v) & ag_roots$cnpj_base %in% idx_roots$cnpj_base
  data.frame(ano = aa, n_raizes = sum(ok),
             perc_valor = sum(v[ok], na.rm = TRUE) / sum(v, na.rm = TRUE))
}) |> (\(x) do.call(rbind, x))()
cat("\nCOBERTURA do indice sobre o agregado (raizes ativas localizadas):\n")
print(cov, row.names = FALSE, digits = 4)
# snapshot ATUAL: anos antigos perdem raizes baixadas (2015-2017 ~86%); o
# metodo absorve proporcionalmente. Exigencia firme so nos anos em uso (2022+).
stopifnot(all(cov$perc_valor[cov$ano >= 2022] > 0.90))

write_parquet(indice, arq_indice)
cat(sprintf("\nOK: %s (%.1f MB)\n", arq_indice, file.size(arq_indice) / 1e6))
