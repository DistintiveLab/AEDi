# infra4 (19) 2025: despesas pagas por funcao (15 Urbanismo + 18.543) / popmun.
# Replica o metodo do infra4_aedi_siconfi.R (DCA-Anexo I-E) para o exercicio
# 2025 e faz append do refdate 2025-12-31. SEM 0-fill, para manter a
# cobertura historica da serie no DW (2024: ~4.290 munis).

suppressMessages({library(data.table); library(dplyr); library(stringr)})

dir <- "coleta/cache/infra4_aedi_s"
fs25 <- list.files(dir, pattern = "mun_.*_2025_annex_dca_i_e\\.csv$",
                   full.names = TRUE)
cat("arquivos 2025:", length(fs25), "\n")

calc_2025 <- \(f) {
  d <- tryCatch(fread(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- janitor::clean_names(d)
  d <- d[coluna == "Despesas Pagas" &
           (str_detect(conta, "^15 -") | str_detect(conta, "^18\\.543 -")),
         .(codmun = as.numeric(cod_ibge), valor, populacao)]
  if (nrow(d) == 0) return(NULL)
  d[, .(value = sum(valor, na.rm = TRUE),
        populacao = suppressWarnings(max(populacao, na.rm = TRUE))),
    by = codmun]
}

tudo <- rbindlist(lapply(fs25, calc_2025), fill = TRUE)
tudo <- tudo[!is.na(codmun) & !is.na(value)]
cat("infra4 2025: munis com despesa:", nrow(tudo),
    "| soma total (R$ bi):", round(sum(tudo$value) / 1e9, 2), "\n")

# popmun 2025 do DW (datasus_popmun = populacao absoluta, refdate 07-01),
# por geoloc_id 7d. NAO usar objetivo1_3 (é proporcao, nao absoluto).
pkgload::load_all("/home/wlvdbaj/pRojetos/AEDi", export_all = FALSE)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, paste0(
  "SELECT l.geoloc_id, d.value AS pop_dw FROM data_values d ",
  "JOIN mdata m USING (mdata_id) JOIN local l USING (local_id) ",
  "WHERE m.orig_name = 'datasus_popmun' AND d.refdate = DATE '2025-07-01'"))
DBI::dbDisconnect(con)
cat("popmun 2025-07-01:", nrow(pop), "locais | BH:",
    pop$pop_dw[pop$geoloc_id == 3106200], "\n")
stopifnot(nrow(pop) > 5000, pop$pop_dw[pop$geoloc_id == 3106200] > 1e6)

serie <- tudo |>
  inner_join(pop, by = c("codmun" = "geoloc_id")) |>
  transmute(local = codmun, periodo = as.Date("2025-12-31"),
            valor = value / pop_dw)

# fallback: 63 munis sem datasus_popmun 2025 usam a populacao do proprio DCA
faltam <- tudo |> anti_join(pop, by = c("codmun" = "geoloc_id")) |>
  filter(is.finite(populacao), populacao > 0) |>
  transmute(local = codmun, periodo = as.Date("2025-12-31"),
            valor = value / populacao)
cat("fallback populacao DCA:", nrow(faltam), "munis\n")
serie <- bind_rows(serie, faltam)
serie <- serie[!duplicated(serie$local), ]
cat("infra4 2025 serie:", nrow(serie),
    "| media per capita:", round(mean(serie$valor), 1), "\n")

AEDi::gravar_serie_dw("infra4", serie, modo = "append")
cat("DW atualizado (append): infra4 refdate 2025-12-31\n")
