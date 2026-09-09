# infra3 (18) 2025: internacoes DRSAI/10k hab. Processa os .dbc do SIH-RD
# 2025 (ja copiados em coleta/cache/infra3_aedi2/2025/ — 27 UFs x 12 meses)
# com a mesma metodologia do infra3_aedi.R. Append do refdate 2025-12-31.

suppressMessages({library(read.dbc); library(data.table); library(parallel)})
aih_interesse <- c("A09","A25","B15","A90","A95","B55","B74","B50","B57","A27",
                   "B65","Z135","H543","H10","B08","B36","B820","B839")
padrao <- paste0("^(", paste0(aih_interesse, collapse="|"), ")")

le_sih_drsai <- \(x) {
  d <- read.dbc::read.dbc(x) |>
    dplyr::select(ANO_CMPT, MUNIC_MOV, DIAG_PRINC) |>
    dplyr::filter(grepl(padrao, DIAG_PRINC)) |>
    dplyr::group_by(ANO_CMPT, MUNIC_MOV) |>
    dplyr::summarise(internacoes = dplyr::n(), .groups = "drop")
}

arqs <- list.files("coleta/cache/infra3_aedi2/2025", pattern = "\\.dbc$",
                   full.names = TRUE)
cat("processando", length(arqs), "arquivos .dbc (SIH-RD 2025)...\n")
res <- lapply(arqs, le_sih_drsai)

inf3 <- data.table::rbindlist(res) |>
  dplyr::group_by(ANO_CMPT, MUNIC_MOV) |>
  dplyr::summarise(internacoes = sum(internacoes, na.rm = TRUE), .groups = "drop")
cat("municipios com internacoes DRSAI:", nrow(inf3), "| total:",
    sum(inf3$internacoes), "\n")

# populacao de julho 2025 (ja no DW)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) local, d.value pop
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun' AND d.refdate = DATE '2025-07-01'")
DBI::dbDisconnect(con)

serie <- inf3 |>
  dplyr::mutate(local = as.numeric(as.character(MUNIC_MOV))) |>
  dplyr::left_join(pop, by = "local") |>
  dplyr::filter(!is.na(pop), pop > 0) |>
  dplyr::transmute(local,
                   periodo = as.Date("2025-12-31"),
                   valor = 10000 * ifelse(is.na(internacoes), 0, internacoes) / pop)
cat("infra3 2025:", nrow(serie), "municipios | media:", round(mean(serie$valor), 1), "\n")

AEDi:::gravar_serie_dw("infra3",
  data.frame(local = serie$local, periodo = serie$periodo, valor = serie$valor),
  modo = "append")
