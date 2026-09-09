# infra1 (16): Percentual de atendimento urbano de agua (SINISA — antigo SNIS).
# Fonte: SINISA_Resultados_Ref2024.zip -> AGUA_Indicadores_Base Municipal
# (coluna 12 = "Atendimento da populacao urbana com rede de abastecimento").
# O DW tem 2022-2023 do SNIS (cache); este script acrescenta 2024.
# Padrao A5b (append).

td <- "coleta/cache/sinisa_agua"
if (!file.exists(file.path(td, "sinisa_ref2024.zip"))) {
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  h <- curl::new_handle(ssl_verifypeer = 0, followlocation = 1,
                        useragent = "Mozilla/5.0 (X11; Linux x86_64) Firefox/130.0")
  curl::curl_download(
    "https://www.gov.br/cidades/pt-br/acesso-a-informacao/acoes-e-programas/saneamento/sinisa/resultados-sinisa/SINISA_Resultados_Ref2024.zip",
    file.path(td, "sinisa_ref2024.zip"), handle = h)
  unzip(file.path(td, "sinisa_ref2024.zip"), exdir = td)
}
f <- list.files(td, pattern = "AGUA_Indicadores.*Municipal.*Retifica.*xlsx$",
                recursive = TRUE, full.names = TRUE)[1]

# cabecalho em 3 niveis: linha 9 (grupo), 10 (nome), 11 (unidade); dados a partir da 12
d <- readxl::read_xlsx(f, skip = 10, col_names = FALSE)
h2 <- as.character(suppressWarnings(
  readxl::read_xlsx(f, skip = 9, n_max = 1, col_names = FALSE))[1,])
cat("cols:", ncol(d), "| cabecalho h2[11:13]:", paste(h2[11:13], collapse=" | "), "\n")

x <- d |>
  dplyr::transmute(codmun7 = suppressWarnings(as.numeric(...1)),
                   urbano = suppressWarnings(as.numeric(...12))) |>
  dplyr::filter(!is.na(codmun7), !is.na(urbano))
cat("infra1 2024 SINISA:", nrow(x), "municipios | media:", round(mean(x$urbano), 1), "%\n")
stopifnot(nrow(x) > 5000, mean(x$urbano) > 80, mean(x$urbano) < 100)

AEDi:::gravar_serie_dw("infra1",
  data.frame(local = x$codmun7, periodo = as.Date("2024-12-31"),
             valor = x$urbano),
  modo = "append")
