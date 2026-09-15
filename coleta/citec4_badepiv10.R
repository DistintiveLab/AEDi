library(tidyverse)
##badepi patentes
#https://www.gov.br/inpi/pt-br/inpi-data/dados-e-series-temporais/badepi
badepilink <- "https://inpidrive.inpi.gov.br/index.php/s/TK6P8UThwYySyJs/download"
badepilink2 <- "https://www.gov.br/inpi/pt-br/inpi-data/dados-e-series-temporais/indicadores_pi_2023_badepiv-10_atualizado-em-12-08-24.zip"
cacheind <- "coleta/cache/badepi_patentes_mun/"

f <- tempfile(fileext = "zip")
download.file(badepilink2,f)
unzip(f,exdir = cacheind)

#Página 2 de https://portais.univasf.edu.br/nit/nucleo-de-inovacao-tecnologica/documentos/novo-codigo-de-numeracao-dos-pedidos-de-patente_nit.pdf
# 10 a 12 - PI
# 20 a 22 - MU
depositos <-
#  data.table::fread(paste0(cacheind,"badepiv10_ptn_deposito.csv"))|>
  readxl::read_xlsx(paste0(cacheind,"badepiv10_ptn_deposito.xlsx"))|>
  dplyr::mutate(
                CD_NATUREZ_PEDIDO = substr(NO_PEDIDO,1,2)) |>
  dplyr::filter(CD_NATUREZ_PEDIDO %in% c("PI", "MU",10:12,20:22)) |>
  dplyr::select(ANO, NO_PEDIDO) |>
  janitor::clean_names()

inventores <- data.table::fread(paste0(cacheind,"badepiv10_ptn_inventor.csv"),
                    colClasses = c(NO_CNPJ_CPF = "character",
                                   CD_IBGE_CIDADE = "character")) |>
  dplyr::filter(!(CD_IBGE_CIDADE %in% c("", "  ", "XX","0000000"))) |>
  dplyr::select(NO_PEDIDO, CD_IBGE_CIDADE) |>
  dplyr::mutate(across(NO_PEDIDO,trimws))|>
  dplyr::distinct() |>
  janitor::clean_names()|>
  dplyr::left_join(depositos) |>
  dplyr::group_by(ano, cd_ibge_cidade) |>
  dplyr::summarise(n_dep_patentes = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(cd_ibge_cidade = stringr::str_sub(cd_ibge_cidade, 1, 6),
         ano = as.character(ano))




citec4_tentativo <- popmunicipal|>
  dplyr::filter(lubridate::year(refdate)>1999 & lubridate::year(refdate)<2024)|>
  dplyr::left_join(inventores|>dplyr::transmute(
    refdate=as.Date(paste0(ano,"-07-01")),
    local=as.numeric(cd_ibge_cidade),
    n_dep_patentes))|>
  dplyr::mutate(across(contains("n_dep_p"),\(x){ifelse(is.na(x),0,x)}),
                #citec4cidade=n_dep_patentes_pimu*1e5/populacao,
                citec4inventor=n_dep_patentes*1e5/populacao,
                #citec4t3= ifelse(n_dep_patentes==0,
                 #                n_dep_patentes_pimu,n_dep_patentes)*1e5/populacao,
                #citec4t4=1e5*ifelse(n_dep_patentes<n_dep_patentes_ca,0,n_dep_patentes-n_dep_patentes_ca)/populacao
                )




patentes_cidade_pi <- readxl::read_excel(paste0(cacheind,"Indicadores_PTN_2023.xlsx"),
                                      sheet = "Deposito_PI_Cidade",skip=7)


nm_inicio <- c('cd_ibge_cidade','nome_cidade')
names(patentes_cidade_pi)[1:2] <- nm_inicio
patentes_cidade_pi <- patentes_cidade_pi[!is.na(patentes_cidade_pi$cd_ibge_cidade),]

patentes_cidade_pi <-
  patentes_cidade_pi|>
  tidyr::pivot_longer(-1:-2,names_to="ano",values_to="n_dep_patentes_pi")|>
  dplyr::transmute(ano,cd_ibge_cidade=substr(cd_ibge_cidade,1,6),n_dep_patentes_pi)


patentes_cidade_mu <- readxl::read_excel(paste0(cacheind,"Indicadores_PTN_2023.xlsx"),
                                         sheet = "Deposito_MU_Cidade",skip=7)

names(patentes_cidade_mu)[1:2] <- nm_inicio
patentes_cidade_mu <- patentes_cidade_mu[!is.na(patentes_cidade_mu$cd_ibge_cidade),]


patentes_cidade_mu <-
  patentes_cidade_mu|>
  tidyr::pivot_longer(-1:-2,names_to="ano",values_to="n_dep_patentes_mu")|>
  dplyr::transmute(ano,cd_ibge_cidade=substr(cd_ibge_cidade,1,6),n_dep_patentes_mu)


patentes_cidade_ca <- readxl::read_excel(paste0(cacheind,"Indicadores_PTN_2023.xlsx"),
                                         sheet = "Deposito_CA_Cidade",skip=7)

names(patentes_cidade_ca)[1:2] <- nm_inicio

patentes_cidade_ca <-
  patentes_cidade_ca|>
  tidyr::pivot_longer(-1:-2,names_to="ano",values_to="n_dep_patentes_ca")|>
  dplyr::transmute(ano,cd_ibge_cidade=substr(cd_ibge_cidade,1,6),n_dep_patentes_ca)

patentes_cidade <- patentes_cidade_pi|>dplyr::full_join(patentes_cidade_mu)|>
  dplyr::full_join(patentes_cidade_ca)

patentes_cidade[is.na(patentes_cidade)] <- 0
patentes_cidade <- patentes_cidade|>
  dplyr::mutate(n_dep_patentes_pimu = n_dep_patentes_pi+n_dep_patentes_mu)

patentes_cidade <- patentes_cidade|>
  dplyr::left_join(inventores)

patentes_cidade[is.na(patentes_cidade)] <- 0

popmunicipal <- DBI::dbGetQuery(con,'select
                                refdate,geoloc_id,value from data_values a left join local b ON a.local_id = b.local_id where mdata_id = 66')

pop2024 <- datasus::ibge_popt2024br_mun()

pop2024 <- pop2024|>
  tidyr::separate_wider_delim(cols = Município,
                                names=c('local','nm_mun'),
                              delim=" ",too_few="align_end",
                              too_many = "merge")

popmunicipal <- popmunicipal|>
  dplyr::transmute(refdate,local=trunc(geoloc_id/10),
  populacao=value)|>
  dplyr::bind_rows(pop2024|>dplyr::filter(!is.na(local))|>
                     dplyr::transmute(refdate=as.Date('2024-07-01'),
                                      local=as.numeric(local),populacao=`População estimada`)

)


citec4_tentativo <- popmunicipal|>
  dplyr::filter(lubridate::year(refdate)>1999)|>
  dplyr::left_join(patentes_cidade|>dplyr::mutate(
    refdate=as.Date(paste0(ano,"-07-01")),
    local=as.numeric(cd_ibge_cidade)))|>
  dplyr::mutate(across(contains("n_dep_p"),\(x){ifelse(is.na(x),0,x)}),
    citec4cidade=n_dep_patentes_pimu*1e5/populacao,
         citec4inventor=n_dep_patentes*1e5/populacao,
         citec4t3= ifelse(n_dep_patentes==0,
                          n_dep_patentes_pimu,n_dep_patentes)*1e5/populacao,
    citec4t4=1e5*ifelse(n_dep_patentes<n_dep_patentes_ca,0,n_dep_patentes-n_dep_patentes_ca)/populacao)

citec4_aedi_final <-
  citec4_tentativo|>dplyr::select(-contains("n_dep_"))|>
  dplyr::mutate(across(refdate,\(x){
    lubridate::month(x) <- 12
    lubridate::day(x) <- 31
    x
  }))|>
  dplyr::left_join(locgeoloc)|>
  dplyr::transmute(refdate,local_id,citec4=citec4inventor)|>
  dplyr::filter(!is.na(local_id))

citec4_orig <- DBI::dbGetQuery(mdr,"select
                                refdate,local_id,value citec4_base from data_values a
                          left join mdata b on a.mdata_id = b.mdata_id where
                          orig_name LIKE 'citec4%'")

citec4_comp <- citec4_orig|>
  dplyr::left_join(
    #,-ano,-cd_ibge_cidade
 citec4_aedi_final
  )

cor(citec4_comp[citec4_comp$refdate<'2020-12-31',]$citec4_base,
    citec4_comp[citec4_comp$refdate<'2020-12-31',]$citec4,use='complete.obs')
#0.909506
#0.8550284
# citec4_2021 :
#   Valores de citec4_2021 foram substituídos por citec4_2020
#
# citec4_2022 :
#   Valores de citec4_2022 foram substituídos por citec4_2021
summary(citec4_comp[citec4_comp$refdate<'2020-12-31',c("refdate","local_id","citec4_base","citec4inventor")])

readr::write_csv(citec4_aedi_final,'coleta/cache/citec4_aedi/citec4_aedi.csv')









#
# cor(citec4_comp$citec4_base,citec4_comp$citec4cidade,use='complete.obs')
# #0.4944773
#
# cor(citec4_comp$citec4_base,citec4_comp$citec4inventor,use='complete.obs')
# #0.6181432
# #0.6918338



#
# cor(citec4_comp$citec4_base,citec4_comp$citec4t3,use='complete.obs')
# #0.4712306
#
# cor(citec4_comp$citec4_base,citec4_comp$citec4t4,use='complete.obs')
# #0.6239294
#
# summary(citec4_comp|>dplyr::transmute(refdate,local_id,citec4_base,citec4_inventormun_exc_ca=citec4t4))
