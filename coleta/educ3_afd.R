# educ3 (3): Adequacao da Formacao Docente — % de docentes do Ensino
# Fundamental com formacao adequada (Grupo 1: licenciatura com conteudo
# compativel). Fonte: INEP/AFD 2024 (mesmo caminho do TDI).
# Recalculo completo (replace). Padrao A5b.

# f: prioriza cache local; baixa apenas se ausente
f <- list.files("coleta/cache", pattern = "AFD.*\\.xlsx$",
                recursive = TRUE, full.names = TRUE)[1]
if (is.na(f)) {
  td <- "coleta/cache/afd_2024"
  if (!dir.exists(td)) {
    tf <- tempfile(fileext = ".zip")
    h <- curl::new_handle(ssl_verifypeer = 0, ssl_verifyhost = 0,
                          http_version = 1, followlocation = 1,
                          useragent = "Mozilla/5.0 (X11; Linux x86_64) Firefox/130.0")
    curl::curl_download("https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2024/AFD_2024_MUNICIPIOS.zip",
                        tf, handle = h)
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    unzip(tf, exdir = td)
  }
  f <- list.files(td, pattern = "xlsx$", recursive = TRUE, full.names = TRUE)[1]
}
cat("fonte:", basename(f), "\n")

# estrutura multi-nivel: linhas 7-9 cabecalho, linha 10 nomes, dados 11+.
# Nomes fixos (confirmados na inspecao) para evitar problemas de skip/encoding.
nms <- c("NU_ANO_CENSO","NO_REGIAO","SG_UF","CO_MUNICIPIO","NO_MUNICIPIO",
         "NO_CATEGORIA","NO_DEPENDENCIA",
         paste0("ED_INF_CAT_",1:5), paste0("FUN_CAT_",1:5),
         paste0("FUN_AI_CAT_",1:5), paste0("FUN_AF_CAT_",1:5),
         paste0("MED_CAT_",1:5), paste0("EJA_FUN_CAT_",1:5),
         paste0("EJA_MED_CAT_",1:5))
d <- readxl::read_xlsx(f, skip = 10, col_names = nms, guess_max = 50000)
# remove linhas de cabecalho residual (NA no codigo)
d <- d[!is.na(suppressWarnings(as.numeric(d$CO_MUNICIPIO))), ]
cat("linhas:", nrow(d), "| cols:", ncol(d), "\n")

serie <- d |>
  dplyr::filter(NO_CATEGORIA == "Total", NO_DEPENDENCIA == "Total",
                !is.na(CO_MUNICIPIO)) |>
  dplyr::transmute(local = trunc(as.numeric(CO_MUNICIPIO) / 10),
                   ano = as.numeric(NU_ANO_CENSO),
                   valor = suppressWarnings(as.numeric(FUN_CAT_1))) |>
  dplyr::filter(!is.na(local), !is.na(valor))
cat("educ3 AFD:", nrow(serie), "municipios | ano:", unique(serie$ano),
    "| media:", round(mean(serie$valor), 1), "%\n")
stopifnot(nrow(serie) > 5000)

AEDi:::gravar_serie_dw("educ3",
  data.frame(local = serie$local,
             periodo = as.Date(paste0(serie$ano, "-12-31")),
             valor = serie$valor))
