##infra4 siconfi despesa habitação

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

infra4_aedi <- readRDS('coleta/cache/infra4_aedi_s/infra4_aedi.rds')

popmundatasus <- dbGetQuery(con,
                     "select geoloc_id codmun,extract('year' from refdate) ano, value populacao from
                     data_values a left join mdata b on a.mdata_id = b.mdata_id left join
                     local c on a.local_id = c.local_id where extract('year' from refdate) > 2014 and orig_name LIKE 'datasus_pop%'")

infra4_aedi <- infra4_aedi|>dplyr::filter(codmun!= 53,ano<2024)|>
  dplyr::left_join(popmundatasus)

infra4_aedi <- infra4_aedi|>
  dplyr::mutate(infra4_aedi_pc=value/populacao)

mdr <- dbConnect(RPostgreSQL::PostgreSQL(),
dbname=Sys.getenv("tdbname"),
user=Sys.getenv("userdb"),
password=Sys.getenv("passwddbdev"),
host=Sys.getenv("hostdbdev"))


#Conferência
infra4_orig <- dbGetQuery(mdr,
                               "select refdate,a.local_id,geoloc_id codmun,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id left join
                               local c on a.local_id = c.local_id where
                               orig_name like 'infra4%'")


infra4_aedi <-
  infra4_aedi|>
  dplyr::transmute(codmun,refdate=as.Date(paste0(ano,"-12-31")),
                   infra4_aedi_pc)


infra4_compara <-
  infra4_orig|>
  dplyr::left_join(infra4_aedi)
cor(infra4_compara$value,infra4_compara$infra4_aedi_pc,use='complete.obs')
# [1] 0.9997605
