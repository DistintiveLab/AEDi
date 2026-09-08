# desprod1 (11): indice de complexidade economica (ECI) — fonte DataViva
# (coleta/cache/dataviva_atualizacao/dataviva_atualizacao.xlsx; colunas por
# ano 2019-2023). Recalculo completo (replace) do que a fonte cobre. Padrao A5b.

eci <- readxl::read_xlsx("coleta/cache/dataviva_atualizacao/dataviva_atualizacao.xlsx") |>
  dplyr::rename(codmun = `ID IBGE Municípios`) |>
  tidyr::pivot_longer(-codmun, names_to = "ano", values_to = "valor") |>
  dplyr::filter(!is.na(valor)) |>
  dplyr::transmute(local = trunc(as.numeric(codmun)/10),
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = as.numeric(valor))

AEDi:::gravar_serie_dw("desprod1",
  data.frame(local = eci$local, periodo = eci$periodo, valor = eci$valor))
