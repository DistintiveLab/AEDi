# dessoc3 (23): Taxa de Distorcao Idade-Serie — Ensino Fundamental, Total.
# Edicao 2025: TDI_2025_MUNICIPIOS.zip (INEP, indicadores educacionais 2025;
# planilha com cabecalho na linha 9: NU_ANO_CENSO..FUN_CAT_0 = Total Fundamental).
# Append do refdate 2025-12-31. Padrao A5b.

td <- "coleta/cache/tdi_2025"
if (!dir.exists(td)) {
  tf <- tempfile(fileext = ".zip")
  h <- curl::new_handle(ssl_verifypeer = 0, ssl_verifyhost = 0,
                        http_version = 1, followlocation = 1,
                        useragent = "Mozilla/5.0 (X11; Linux x86_64) Firefox/130.0")
  curl::curl_download("https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2025/TDI_2025_MUNICIPIOS.zip",
                      tf, handle = h)
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  unzip(tf, exdir = td)
}
f <- list.files(td, pattern = "xlsx$", recursive = TRUE, full.names = TRUE)[1]

d <- readxl::read_xlsx(f, skip = 8)   # linha 9 = nomes das colunas
cat("linhas:", nrow(d), "| cols:", ncol(d), "\n")
tot <- d |>
  dplyr::filter(NO_CATEGORIA == "Total", NO_DEPENDENCIA == "Total",
                !is.na(CO_MUNICIPIO)) |>
  dplyr::transmute(local = trunc(as.numeric(CO_MUNICIPIO) / 10),
                   valor = suppressWarnings(as.numeric(FUN_CAT_0))) |>
  dplyr::filter(!is.na(local), !is.na(valor))
cat("dessoc3 2025:", nrow(tot), "municipios | media:", round(mean(tot$valor), 1), "\n")
stopifnot(nrow(tot) > 5000)

AEDi:::gravar_serie_dw("dessoc3",
  data.frame(local = tot$local, periodo = as.Date("2025-12-31"),
             valor = tot$valor),
  modo = "append")
