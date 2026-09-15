#infra2

#link <- "https://www.anatel.gov.br/dadosabertos/paineis_de_dados/acessos/acessos_banda_larga_fixa.zip"

link_eq <- "https://www.anatel.gov.br/dadosabertos/paineis_de_dados/acessos/velocidade_contratada_scm.zip"
f <- tempfile(fileext = "zip")
download.file(link,f)
unzip(f,exdir = 'coleta/cache/infra2_aedi/')

blfixa_2022 <- data.table::fread('coleta/cache/infra2_aedi/Acessos_Banda_Larga_Fixa_2022_Colunas.csv')


  blfixa_2022[,acessos2022 := rowSums(.SD,na.rm = T),.SDcols=names(blfixa_2022)[14:25]]

  blfixa_2022[,]


#  blfixa_20192020 <- data.table::fread('coleta/cache/infra2_aedi/Acessos_Banda_Larga_Fixa_2019_2020_Colunas.csv')

  le_acessos_internet <-  \(x) data.table::fread(x,
                                          select = c("Tipo","Tipo de Produto","Tipo de Pessoa","Faixa de Velocidade","Ano","Código IBGE Município","Acessos","Velocidade"))


  arqs_2013_2024 <- list.files('coleta/cache/infra2_aedi/',pattern=".*(202[01234]|201[468]).csv",full.names=T)


  basecalc <- data.table::rbindlist(lapply(arqs_2013_2024,le_acessos_internet),fill = TRUE, use.names = TRUE)

  #unique(basecalc$`Faixa de Velocidade`)

  # media_fx_vel <-
  #   data.table::data.table(
  #     faixa_de_velocidade =c(
  #       "2Mbps a 12Mbps",
  #       "512kbps a 2Mbps",
  #       "0Kbps a 512Kbps",
  #       "12Mbps a 34Mbps",
  #       "> 34Mbps"
  #     ),
  #      vel = c(6,1.25,0.5,23,50))

  basecalc <- basecalc|>janitor::clean_names()

#  basecalc <-  media_fx_vel[basecalc, on = ("faixa_de_velocidade" = "faixa_de_velocidade")]

  ###AJUSTA INFORMAÇÃO FALTANTE POR SUPOSTOS - VE
  basecalc[,`:=` (
    #velocidade=ifelse(is.na(velocidade),vel,velocidade),
                 tipo_de_produto= ifelse(is.na(tipo_de_produto),'INTERNET',tipo_de_produto),
                 tipo_de_pessoa=ifelse(is.na(tipo_de_pessoa),'Pessoa Física',tipo_de_pessoa))]


  basecalc <- basecalc[tipo_de_produto == 'INTERNET' & tipo_de_pessoa == 'Pessoa Física',]
#
  ###AJUSTA INFORMAÇÃO VELOCIDADE DE INTERNET ACIMA 1Gbps = 1Gbps (1-2% com valores absurdos)
#  basecalc[,velocidade:=ifelse(velocidade>1000,1000,velocidade)]

  ###AJUSTA INFORMACAO DA MEDIA DA FAIXA DE VELOCIDADE MAIOR DE ACORDO COM 2021-204
  #: VAR 70 ANO  muito constante 2021 2024!
  #: 2017 -

#  media_geral <- weighted.mean(basecalc$velocidade,w=basecalc$acessos)

  #basecalc[,velocidade_alta:= velocidade>=media_geral]

  basecalc[,velocidade_alta:= faixa_de_velocidade=="> 34Mbps"]
  #write_rds(basecalc,'coleta/cache/infra2_aedi/basecalc2017_2024.rds')

  infra2_aedi <- basecalc[,.(total_acessos=sum(acessos)),by= .(ano,codigo_ibge_municipio,velocidade_alta)]

  infra2_aedi <- data.table::dcast(infra2_aedi,ano+codigo_ibge_municipio ~ velocidade_alta,value.var = 'total_acessos',fill = 0)

  names(infra2_aedi)[3:4] <- c('normal','velocidade_alta')

  infra2_aedi <- infra2_aedi[,infra2_aedi := 100*velocidade_alta/(normal+velocidade_alta)][,.(ano,codigo_ibge_municipio,infra2_aedi)]

  infra2_aedi[,ano:=as.Date(paste0(ano,"-12-31"))]

  infra2_orig <- DBI::dbGetQuery(mdr,"select refdate,geoloc_id, value from data_values a left join mdata b on a.mdata_id = b. mdata_id left join local c on a.local_id = c.local_id where orig_name LIKE 'infra2%'")

  names(infra2_aedi)[1:2] <- names(infra2_orig)[1:2]

  readr::write_csv(infra2_aedi,'coleta/cache/infra2_aedi/infra2_aedi.csv')

  infra2_compara <- infra2_aedi[infra2_orig, on = list(refdate,geoloc_id)]

  infra2_compara <- infra2_compara[,.(refdate,cd_ibge=geoloc_id,infra2_novo=infra2_aedi,infra2_orig=value)]

#https://dados.anatel.gov.br/qap/tempcontent/5eb956ff-63fb-4dd9-9b9f-3fcf12861b0d/617e38aa-231a-4198-9189-a63385a748eb.xlsx?serverNodeId=f9bd7020-6b0e-4515-9513-1115658bfe0f

#https://informacoes.anatel.gov.br/paineis/acessos/velocidade-contratada-banda-larga-fixa

#Velocidade média por município apenas


#https://www.anatel.gov.br/dadosabertos/PDA/Acesso_bandalarga_100habitantes/Acesso_bandalarga_100habitantes.csv

infra2_100hab <- data.table::fread('coleta/cache/infra2_aedi/Densidade_Banda_Larga_Fixa.csv')
