# infra1 (16), infra3 (18) e infra4 (19): carrega as series completas ja
# computadas em cache (replace). Padrao A5b.
#   infra1: SNIS % atendimento urbano de agua (cache ate 2023)
#   infra3: internacoes DRSAI/10k hab, base SIH-RD/AIH (cache ate 2024)
#   infra4: despesas SICONFI habilitacao/areas degradadas per capita
#           (cache ate 2024)

i1 <- readr::read_csv("coleta/cache/infra1_aedi/infra1_aedi.csv", show_col_types = FALSE)
AEDi:::gravar_serie_dw("infra1",
  data.frame(local = i1$geoloc_id, periodo = as.Date(i1$refdate), valor = i1$infra1))

i3 <- readr::read_csv("coleta/cache/infra3_aedi2/infra3_aedi_2013_2024.csv",
                      show_col_types = FALSE)
AEDi:::gravar_serie_dw("infra3",
  data.frame(local = i3$local, periodo = as.Date(i3$refdate), valor = i3$infra3_aedi))

i4 <- readr::read_csv("coleta/cache/infra4_aedi_s/infra4_aedi_2015_2024.csv",
                      show_col_types = FALSE)
AEDi:::gravar_serie_dw("infra4",
  data.frame(local = i4$codmun, periodo = as.Date(paste0(i4$ano, "-12-31")),
             valor = i4$infra4_aedi))
