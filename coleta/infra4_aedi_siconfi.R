# ##infra4 siconfi despesa habitação
#
# files <- list.files(path = "coleta/cache/infra4_aedi_s/",
#                     full.names = TRUE,
#                     recursive = TRUE,
#                     pattern = ".csv$")
#
#
# reading_data <- function(x){
#   data <- data.table::fread(x) |>
#     dplyr::filter(coluna == "Despesas Pagas") |>
#     dplyr::filter(stringr::str_detect(conta, "^15 -") | stringr::str_detect(conta, "^18.543 -")) |>
#     dplyr::rename(codmun = cod_ibge,
#                   value = valor) |>
#     dplyr::group_by(codmun,ano=exercicio) |>
#     dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
#     dplyr::mutate(
#       variavel = "infra4_aedi")
# }
#
#
# infra4_aedi <- purrr::map_dfr(.x = files, .f = reading_data)
#
# saveRDS(infra4_aedi,'coleta/cache/infra4_aedi_s/infra4_aedi.rds')

infra4_aedi <- readRDS("coleta/cache/infra4_aedi_s/infra4_aedi.rds")

infra4_aedi <- popmunicipal|>dplyr::transmute(ano=lubridate::year(refdate),local,populacao)|>
  dplyr::filter(ano>2014)|>
  dplyr::left_join(infra4_aedi|>dplyr::mutate(local=trunc(codmun/10)))|>
  dplyr::mutate(infra4_aedi=ifelse(is.na(value),0,value)/populacao)
#Conferência
# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))

infra4_orig <-
  dbGetQuery(mdr,
             "select refdate,local_id,value infra4_base from
             data_values a left join mdata b on a.mdata_id = b.mdata_id where
              orig_name LIKE 'infra4%'")

infra4_compara <-
  infra4_orig|>
  dplyr::left_join(locgeoloc)|>
  dplyr::left_join(infra4_aedi|>dplyr::rename(geoloc_id=codmun)|>dplyr::mutate(refdate=as.Date(paste0(ano,"-12-31"))))

cor(infra4_compara$infra4_base,infra4_compara$infra4_aedi,use='complete.obs')
#0.9997605

summary(infra4_compara|>dplyr::select(refdate,local_id,infra4_base,infra4_aedi))

readr::write_csv(infra4_aedi,"coleta/cache/infra4_aedi_s/infra4_aedi_2015_2024.csv")
