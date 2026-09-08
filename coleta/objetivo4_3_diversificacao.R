# objetivo4_3 (54): coeficiente de diversificacao economica por UF
# (metade da soma absoluta dos desvios setoriais vs estrutura nacional).
# Fonte direta mte_rais. Padrao A5b. (Formula do indicadores_agregado_uf.R.)

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

vinculos_ano_setor <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio, trunc(cnae_2_0_classe/1000) setor, COUNT(*) qtd_vinc
            FROM rais_vinculo_", ano,
           " WHERE vinculo_ativo_31_12 = 1 GROUP BY municipio, setor"))
  a$ano <- ano
  a
}
emprego_por_cnae_mun <- data.table::rbindlist(
  lapply(AEDi:::anos_rais(rais), vinculos_ano_setor))
DBI::dbDisconnect(rais)

o43 <- emprego_por_cnae_mun |>
  dplyr::mutate(uf = trunc(municipio/10000), setor = trunc(setor)) |>
  dplyr::group_by(uf, ano, setor) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(uf, ano) |>
  dplyr::mutate(vinc_munic = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::group_by(setor, ano) |>
  dplyr::mutate(vinc_setor_br = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::group_by(ano) |>
  dplyr::mutate(vinc_br = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(value = abs((vinc_setor/vinc_munic) - (vinc_setor_br/vinc_br))) |>
  dplyr::group_by(uf, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)/2, .groups = "drop") |>
  dplyr::transmute(local = uf,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = value)

AEDi:::gravar_serie_dw("objetivo4_3",
  data.frame(local = o43$local, periodo = o43$periodo, valor = o43$valor))
