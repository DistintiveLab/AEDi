# sust4 (29): emissoes liquidas de GEE (CO2e GWP-AR5) da agropecuaria e
# processos industriais, por municipio. Fonte: SEEG v13.0 (serie 1970-2024).
# Recalculo completo (replace). Padrao A5b.

suppressMessages({library(data.table); library(dplyr); library(tidyr)})

f <- "coleta/cache/sust4/Dados-municipais-resumido-CO2e-GWP-AR5-13.0.xlsx"
cat("lendo", f, "(~247MB, pode demorar)...\n")
lemissoes <- readxl::read_xlsx(f, sheet = "Dados") |>
  janitor::clean_names() |>
  dplyr::select(emissao_remocao_bunker, setor_de_emissao,
                id_territorio, municipio, matches("^x?(19|20)[0-9]{2}$"))

cat("linhas:", nrow(lemissoes), "| cols:", ncol(lemissoes), "\n")

cat("linhas:", nrow(lemissoes), "| cols:", ncol(lemissoes), "\n")
lemissoes$id_territorio <- as.numeric(lemissoes$id_territorio)

lemissoes_r <- lemissoes |>
  dplyr::filter(setor_de_emissao %in% c("Agropecuária", "Processos Industriais") &
                  emissao_remocao_bunker %in% c("Emissão", "Remoção") &
                  id_territorio > 1e7 & !is.na(id_territorio)) |>
  tidyr::pivot_longer(cols = -c(emissao_remocao_bunker, setor_de_emissao,
                                id_territorio, municipio),
                      names_to = "ano", values_to = "value") |>
  dplyr::group_by(id_territorio, municipio, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(ano = gsub("^x", "", ano)) |>
  dplyr::filter(!grepl("NA", municipio), !is.na(id_territorio))

cat("municipios:", length(unique(lemissoes_r$id_territorio)),
    "| anos:", paste(range(as.numeric(lemissoes_r$ano)), collapse="-"), "\n")

saveRDS(lemissoes_r, "coleta/cache/sust4_aedi/emissoes_agro_industria_v13.rds")

# serie no DW: id_territorio - 1e7 = geoloc_id (IBGE 7 digitos); valor em Mt
s4 <- lemissoes_r |>
  dplyr::transmute(local = id_territorio - 1e7,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = value / 1e6)

AEDi:::gravar_serie_dw("sust4",
  data.frame(local = s4$local, periodo = s4$periodo, valor = s4$valor))
