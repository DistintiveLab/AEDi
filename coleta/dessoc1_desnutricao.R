# dessoc1 (21): % internacoes por desnutricao sobre o total de internacoes.
# Fonte: DATASUS SIH (AIH) via datasus::sih_nibr_mun (TABNET, HTTP — funciona
# desta maquina, sem FTP). Serie completa 2013-2025. Padrao A5b.

suppressMessages(library(datasus))
Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

periodos <- stringr::str_to_title(
  format.Date(seq.Date(as.Date("2013-01-01"), as.Date("2025-12-01"), by = "month"),
              "%b/%Y"))

cat("buscando internacoes por desnutricao...\n")
desnutricao <- tryCatch(
  datasus::sih_nibr_mun(periodo = periodos, coluna = "Ano processamento",
                        categoria_cid10 = "Desnutrição"),
  error = function(e) { cat("ERRO:", conditionMessage(e), "\n"); NULL })
if (is.null(desnutricao)) stop("falha ao obter desnutricao")

cat("buscando internacoes totais...\n")
total <- tryCatch(
  datasus::sih_nibr_mun(periodo = periodos, coluna = "Ano processamento"),
  error = function(e) { cat("ERRO:", conditionMessage(e), "\n"); NULL })
if (is.null(total)) stop("falha ao obter total")

parse_mun <- \(d) {
  d |>
    tidyr::separate_wider_delim(Município, delim = " ",
                                names = c("cd_mun","nm_mun"),
                                too_many = "merge", too_few = "align_end") |>
    dplyr::mutate(cd_mun = as.numeric(cd_mun))
}

desnutricao <- parse_mun(desnutricao)
total <- parse_mun(total)
desnutricao[is.na(desnutricao)] <- 0
total[is.na(total)] <- 0

serie <- desnutricao |>
  dplyr::select(-"Total") |>
  tidyr::pivot_longer(-1:-2, names_to = "ano", values_to = "desnut") |>
  dplyr::left_join(total |> dplyr::select(-"Total") |>
             tidyr::pivot_longer(-1:-2, names_to = "ano", values_to = "tot")) |>
  dplyr::filter(!is.na(cd_mun), tot > 0) |>
  dplyr::transmute(local = cd_mun,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = 100 * desnut / tot)

cat("dessoc1:", nrow(serie), "municipios-ano | anos:",
    paste(range(as.numeric(format(unique(serie$periodo), "%Y"))), collapse="-"), "\n")

readr::write_csv(serie, "coleta/cache/dessoc1_aedi/dessoc1_aedi_2013_2025.csv")
AEDi:::gravar_serie_dw("dessoc1",
  data.frame(local = serie$local, periodo = serie$periodo, valor = serie$valor))
