#OBJETIVO 1_1

###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(
  con,
  "SELECT orig_name,data_freq_id,refdate,trunc(geoloc_id/1e5) geoloc_id,value from named_datavalues
  LEFT JOIN local on named_datavalues.local_id = local.local_id
  WHERE orig_name IN ('rais_vlr_rem_dez_s38','rais_vinculos_s38','rais_mediana_remmed_s38')")

medianaano <-
  dbdbase |>dplyr::filter(orig_name=='rais_mediana_remmed_s38')|>
  dplyr::distinct()|>
  tidyr::pivot_wider(
    names_from=orig_name,values_from=value
  )


dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>
  dplyr::filter(orig_name!='rais_mediana_remmed_s38')|>
  dplyr::group_by(orig_name,geoloc_id,refdate,data_freq_id)|>
  dplyr::summarize_all(somasna)|>
  tidyr::pivot_wider(names_from='orig_name',values_from = 'value')|>
  dplyr::left_join(medianaano)

dbdbase <- dbdbase|>
  dplyr::ungroup()|>
  dplyr::mutate(rais_media_remdez_s38 =
                  rais_vlr_rem_dez_s38/rais_vinculos_s38)
###Cria indicador
objetivo1_1_uf <- dbdbase |>
  dplyr::rename(setNames(c('rais_media_remdez_s38','rais_mediana_remmed_s38'), c('a','b'))) |>
  dplyr::transmute(objetivo1_1 = a - b,refdate,geoloc_id)


ufgeoloc <-
  DBI::dbGetQuery(
    con,
    'select local_id, local_name,geoloc_id from local where geoloc_id > 9 AND geoloc_id < 100'
)


objetivo1_1_uf <-
  objetivo1_1_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(-geoloc_id,-local_name)

##
objetivo1_1_uf$mdata_id <- recupmdata_id(serie = 'objetivo1_1',db = mdr)

objetivo1_1_uf <-
  objetivo1_1_uf|>
  dplyr::rename(value=objetivo1_1)
#OBJETIVO 1_2

##obtem ide iniciais e finais por uf com replica

le_ideb_ufs <- \(referencia = 1) {
  url_ufs <- (educabR::metainep|>dplyr::filter(grepl("Ideb",assunto,fixed = F))|>
  dplyr::filter(grepl('ufs',tab_url)))$tab_url

f <- tempfile(fileext = "zip")
download.file(url_ufs,f,method="curl")

availf <- unzip(f, list = T)

unzip(f, files = availf[grepl("xlsx", availf$Name), ]$Name,
      junkpaths = T, exdir = dirname(f))

caminho_arquivo <- paste0(dirname(f), "/", basename(availf[grepl("xlsx",
                                                                 availf$Name), ]$Name[1]))

cabecalho <- readxl::read_excel(caminho_arquivo, sheet = referencia,
                                range = readxl::cell_rows(7:10), col_names = FALSE)


colunas <- apply(cabecalho, 2, function(col) {
  paste(na.omit(col), collapse = "_")
})

dados <- readxl::read_excel(caminho_arquivo, sheet = referencia, skip = 10,
                            col_names = colunas,na = "-")



normalizar_nomes <- function(nomes) {
  gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(tolower(iconv(nomes,
                                                                                           to = "ASCII//TRANSLIT")), pattern = " de ", replacement = "_"),
                                                                        pattern = "\\s+", replacement = "_"), pattern = "_+",
                                                                   replacement = "_"), pattern = "^_|_$", replacement = ""),
                                                         pattern = "_\\d$", replacement = ""), pattern = "_si$",
                                                    replacement = ""), pattern = "_vl_", replacement = "_vl-"),
                                          pattern = "a_([pm])", replacement = "a-\\1"), pattern = "o_a",
                                     replacement = "o-a"), pattern = "o_(\\do)", replacement = "o-\\1"),
                           pattern = "a_(\\do)", replacement = "a-\\1"), pattern = "_\\(.\\)_",
                      replacement = "_"), pattern = "r_r", replacement = "r-r"),
            pattern = "^vl_", replacement = "ideb_vl-"), pattern = "^\\d{4}",
       replacement = "meta-para-o-ideb")
}


colnames(dados) <- normalizar_nomes(colnames(dados))

colnames(dados) <- gsub(
  "^taxa_aprovacao_-_\\d+_","",
  colnames(dados)
)

colnames(dados) <- gsub(
  "^(ideb)_(\\d+)_([^_]+)_","\\1-\\2-\\3_",
  colnames(dados)
)

colnames(dados) <- gsub(
  "^(nota)_(saeb)_-_(\\d+)_(matematica)_","\\1-\\2-\\3-\\4_",
  colnames(dados)
)

colnames(dados) <-
  gsub(
    "n_x_p",
    "n.x.p",
    colnames(dados)
  )

colnames(dados) <-
  gsub(
    "metas_do-1o_ciclo_do_ideb6,7_(2007-2021)_2007",
    "meta-para-o-ideb",
    colnames(dados),
    fixed=TRUE
  )
dados <- dados[1:128,]

dados_long <- dplyr::filter(dplyr::select(dplyr::mutate(tidyr::pivot_longer(dados,
        cols = -(1:2), names_to = c("detalhe", "indicador", "ano"),
        names_sep = "_", values_to = "valor"), codigo_municipio = get(names(dados)[1]),
        nome_municipio = get(names(dados)[1]), ano = as.integer(ano),
        valor = as.numeric(valor), indicador = dplyr::case_when(indicador ==
            "vl-aprovacao" ~ "Taxa de Aprovação", grepl("indicador-rend",
            indicador) ~ "Indicador de Rendimento", indicador ==
            "vl-nota-media" ~ "Nota SAEB", grepl("nota", indicador) ~
            "Nota SAEB", indicador == "vl-observado" ~ "IDEB",
            indicador == "vl-projecao" ~ "IDEB", TRUE ~ indicador),
        detalhe = dplyr::case_when(detalhe == "1o-ao-5o-ano" ~
            "1ª à 5ª Série", detalhe == "1o" ~ "1ª Série",
            detalhe == "2o" ~ "2ª Série", detalhe == "3o" ~
                "3ª Série", detalhe == "4o" ~ "4ª Série",
            detalhe == "5o" ~ "5ª Série", detalhe == "6o-a-9o-ano" ~
                "6ª à 9ª Série", detalhe == "6o" ~ "6ª Série",
            detalhe == "7o" ~ "7ª Série", detalhe == "8o" ~
                "8ª Série", detalhe == "9o" ~ "9ª Série",
            detalhe == "matematica" ~ "Matemática", detalhe ==
                "lingua-portuguesa" ~ "Língua Portuguesa", detalhe ==
                "1a" ~ "1ª Série do Ensino Médio", detalhe ==
                "2a" ~ "2ª Série do Ensino Médio", detalhe ==
                "3a" ~ "3ª Série do Ensino Médio", detalhe ==
                "4a" ~ "4ª Série do Ensino Médio", detalhe ==
                "total" ~ "Ensino Médio (Total)", grepl("media",
                detalhe) ~ "Nota Média Padronidaza", grepl("meta",
                detalhe) ~ "Meta para o IDEB", grepl("ideb",
                detalhe) ~ "IDEB", grepl("rend", detalhe) ~ "Taxa de aprovação Média (Indicador de Rendimento)",
            TRUE ~ detalhe),rede = dplyr::case_when(
              grepl("Total",rede) ~ "Total",
              grepl("Privada",rede) ~ "Privada",
              grepl("Pública",rede) ~ "Pública",
              grepl("Estadual",rede) ~ "Estadual",
              T ~ NA
            )), "codigo_municipio", "nome_municipio",
        "rede", "ano", "indicador", "detalhe", "valor"), !is.na(codigo_municipio))

dados_long <- dados_long|>
  dplyr::filter(!(nome_municipio %in% c('Norte',"Sul","Centro-Oeste","Sudeste",'Nordeste')))


dados_long <- dados_long|>
  dplyr::bind_rows(dados_long|>dplyr::mutate(ano=ano+1))|>
  dplyr::arrange(codigo_municipio,ano)

return(dados_long)
}

iniciaisuf <- dados_long|>dplyr::filter(detalhe=='IDEB',rede=="Pública")

finaissuf <- le_ideb_ufs(2)|>dplyr::filter(detalhe=='IDEB',rede=="Pública")


idebmediapufs <- iniciaisuf|>dplyr::rename(iniciais=valor)|>
  dplyr::left_join(finaissuf)|>
  dplyr::transmute(
    orig_name = "ideb_media_fundamental_redep",
    refdate=as.Date(paste0(ano,'-12-31')),
    local_name = gsub("M. G.","Mato Grosso",gsub(
      "R. G.","Rio Grande",nome_municipio)),
    value=(iniciais+valor)/2
  )|>
  dplyr::left_join(ufgeoloc)

###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(
  con,
  "SELECT orig_name,data_freq_id,refdate,trunc(geoloc_id/1e5) geoloc_id,value from named_datavalues
  LEFT JOIN local on named_datavalues.local_id = local.local_id
  WHERE orig_name IN ('ideb_basico_rede_mediana')")

medianaano <-
  dbdbase |>dplyr::filter(orig_name=='ideb_basico_rede_mediana')|>
  dplyr::distinct()|>
  tidyr::pivot_wider(
    names_from=orig_name,values_from=value
  )


dbdbase <- idebmediapufs|>
  dplyr::mutate(data_freq_id=9)|>
  dplyr::group_by(geoloc_id,refdate,data_freq_id)|>
  tidyr::pivot_wider(names_from='orig_name',values_from = 'value')|>
  dplyr::left_join(medianaano)|>
  dplyr::ungroup()

###Cria indicador
objetivo1_2_uf <- dbdbase |>
  dplyr::rename(setNames(c('ideb_media_fundamental_redep','ideb_basico_rede_mediana'), c('a','b'))) |>
  dplyr::transmute(objetivo1_2 = a - b,refdate,geoloc_id)



objetivo1_2_uf <-
  objetivo1_2_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(-geoloc_id,-local_name)


objetivo1_2_uf$mdata_id <- recupmdata_id(serie = 'objetivo1_2',db = mdr)

objetivo1_2_uf <-
  objetivo1_2_uf|>
  dplyr::rename(value=objetivo1_2)
#OBJETIVO 1_3


medicosmun <- readRDS("coleta/cache/datasus_profsaude/medicos_ocupacoes_p_mun_2010_2024.rds")
###############REFAZER


medianaano <-
  medicosmun |>
  dplyr::mutate(Total=tidyr::replace_na(Total,1),medicopc = Total/value)|>
  dplyr::group_by(refdate)|>
  dplyr::summarize(mediananac=median(medicopc,na.rm=T))


medicospc_uf <-
  medicosmun|>
  dplyr::mutate(uf=trunc(geoloc_id/1e5))|>
  dplyr::group_by(refdate,uf)|>
  dplyr::summarize(across(c(Total,value),somasna))|>
  dplyr::mutate(medicopc=Total/value)

medicospc_uf <-
  medicospc_uf|>
  dplyr::left_join(medianaano)

medicospc_uf <-
  medicospc_uf|>
  dplyr::transmute(geoloc_id=uf,
                   refdate,objetivo1_3 = medicopc-mediananac)


objetivo1_3_uf <-
  medicospc_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(-geoloc_id,-local_name)

objetivo1_3_uf$mdata_id <- recupmdata_id(serie = 'objetivo1_3',db = mdr)

objetivo1_3_uf <-
  objetivo1_3_uf|>
  dplyr::rename(value=objetivo1_3)

lubridate::month(objetivo1_3_uf$refdate) <- 12
lubridate::day(objetivo1_3_uf$refdate) <- 31
#OBJETIVO 2_1

objetivo2_1_uf <- emprego_por_cnae_mun|>

  dplyr::mutate(
    uf = trunc(municipio/1e4),
    regiao = trunc(municipio/1e5),
    setor = trunc(setor/1000))|>
  dplyr::group_by(uf, ano, setor, regiao) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE)) |>
  dplyr::group_by(uf, ano) |>
  dplyr::mutate(vinc_uf = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(regiao, setor, ano) |>
  dplyr::mutate(vinc_setor_regiao = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(regiao, ano) |>
  dplyr::mutate(vinc_regiao = sum(vinc_setor, na.rm = TRUE),
                value = (vinc_setor/vinc_uf)*(1-(vinc_setor_regiao/vinc_regiao))*log(vinc_setor/vinc_uf)) |>
  dplyr::group_by(uf, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)*-1) |>
  dplyr::mutate(variavel = "objetivo2_1") |>
  dplyr::rename(geoloc_id = uf) |>
  dplyr::select(ano, geoloc_id, variavel, value) |>
  dplyr::ungroup()

objetivo2_1_uf <-
  objetivo2_1_uf|>
  dplyr::filter(geoloc_id!=99)

objetivo2_1_uf$mdata_id <- recupmdata_id(serie = 'objetivo2_1',db = mdr)

objetivo2_1_uf <-
  objetivo2_1_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(mdata_id,refdate=as.Date(paste0(ano,'-12-31')),
                   local_id,value)

###OBJETIVO 2_2


primazia_pop_agr_uf <- datasus_popmun|>
  dplyr::mutate(
    uf=trunc(geoloc_id/1e5),
    regiao=trunc(uf/10))|>
  dplyr::group_by(refdate,regiao,uf)|>
  dplyr::summarize(populacao=somasna(populacao))|>
  dplyr::mutate(popmaxreg=max(populacao),
                primaziapop=populacao/popmaxreg)


objetivo2_2_uf <-
  primazia_pop_agr_uf|>dplyr::rename(geoloc_id=uf)|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(refdate,local_id,value=primaziapop)

objetivo2_2_uf$mdata_id=recupmdata_id('objetivo2_2',mdr)


lubridate::month(objetivo2_2_uf$refdate) <- 12

lubridate::day(objetivo2_2_uf$refdate) <- 31
###OBJETIVO 2_3



###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(
  con,
  "SELECT orig_name,data_freq_id,refdate,trunc(geoloc_id/1e5) geoloc_id,value from named_datavalues
  LEFT JOIN local on named_datavalues.local_id = local.local_id
  WHERE orig_name ='massa_salarial_municipal'")


dbdbase <-
  dbdbase|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(across(value,somasna))|>
  dplyr::mutate(regiao=trunc(geoloc_id/10))|>
  dplyr::group_by(refdate,regiao)|>
  dplyr::mutate(maxmassaregiao=max(value),
                primaziaeco=value/maxmassaregiao)

objetivo2_3_uf <-
  dbdbase|>dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(refdate,local_id,value=primaziaeco)

objetivo2_3_uf$mdata_id <- recupmdata_id('objetivo2_3',mdr)

###Objetivo 3_1


###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('emprego_nivsuperior_municipal','emprego_formal_municipal')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)


dbdbase <-
  dbdbase|>
  dplyr::mutate(uf=trunc(codigo_ibge/1e5))|>
  dplyr::group_by(data_freq_id,refdate,uf,estado)|>
  dplyr::summarize(across(c(emprego_formal_municipal,emprego_nivsuperior_municipal),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(objetivo3_1 = 100*emprego_nivsuperior_municipal/emprego_formal_municipal)

objetivo3_1_uf <-
  dbdbase|>dplyr::ungroup()|>dplyr::rename(geoloc_id=uf)|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(refdate,local_id,value=objetivo3_1)



objetivo3_1_uf$mdata_id <- recupmdata_id('objetivo3_1',mdr)

##Objetivo 3_2




###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('massa_salarial_municipal','emprego_formal_municipal')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador

objetivo3_2_uf <- dbdbase |>dplyr::select(2:4,8,12)|>
  dplyr::mutate(geoloc_id=trunc(codigo_ibge/1e5))|>
  dplyr::group_by(refdate,geoloc_id,estado)|>
  dplyr::summarise_all(somasna)|>
  dplyr::ungroup()|>
  dplyr::rename(setNames(c('massa_salarial_municipal','emprego_formal_municipal'), c('a','b'))) |>
  dplyr::transmute(objetivo3_2 = a / b,refdate,geoloc_id)

objetivo3_2_uf <-
  objetivo3_2_uf|>
  dplyr::left_join(ufgeoloc)

objetivo3_2_uf <-
  objetivo3_2_uf|>
dplyr::transmute(refdate,local_id,value=objetivo3_2)

objetivo3_2_uf$mdata_id <- recupmdata_id('objetivo3_2',mdr)

##Objetivo 3_3

###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('datasus_popmun')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>
  dplyr::filter(refdate>'2011-12-31')|>
  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 1,unused_fn=dplyr::first)
###Cria indicador
objetivo3_3_uf <- dbdbase|>
  dplyr::filter(!is.na(codigo_ibge))|>
  dplyr::mutate(geoloc_id=trunc(codigo_ibge/1e5))|>
  dplyr::mutate(refdate=as.Date(paste0(lubridate::year(refdate),"-12-31")))|>
  dplyr::group_by(refdate,geoloc_id)|>dplyr::summarize(across(datasus_popmun,somasna))|>
  dplyr::ungroup()|>
  dplyr::arrange(geoloc_id,refdate)|>dplyr::mutate(datasus_popmun= datasus_popmun/dplyr::lag(datasus_popmun))|>dplyr::ungroup() |>
  dplyr::rename(setNames(c('datasus_popmun'), c('a'))) |>
  dplyr::transmute(objetivo3_3 = a,refdate,geoloc_id)|> dplyr::ungroup()

objetivo3_3_uf <-
  objetivo3_3_uf |>
  dplyr::left_join(ufgeoloc)

objetivo3_3_uf <-
  objetivo3_3_uf|>dplyr::filter(refdate>'2012-12-31')|>
  dplyr::transmute(refdate,local_id,value=objetivo3_3)

objetivo3_3_uf$mdata_id <- recupmdata_id('objetivo3_3',mdr)

##Objetivo 4_1

###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('emprego_agricola_sobre_total_mun','empregoagricola_prop_nacional')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)
###Cria indicador
objetivo4_1_uf <- dbdbase |>
  dplyr::mutate(geoloc_id=trunc(codigo_ibge/1e5))|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(across(contains('emprego'),somasna))|>
  dplyr::rename(setNames(c('emprego_agricola_sobre_total_mun','empregoagricola_prop_nacional'), c('a','b'))) |>
  dplyr::transmute(objetivo4_1 = a / b,refdate,geoloc_id)

objetivo4_1_uf <-
  objetivo4_1_uf |>
  dplyr::left_join(ufgeoloc)

objetivo4_1_uf <-
  objetivo4_1_uf |>
  dplyr::transmute(mdata_id=recupmdata_id("objetivo4_1",mdr),refdate,local_id,value=objetivo4_1)


##Objetivo 4_2

objetivo4_2_uf <-
  emprego_mineracao_municipal|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::group_by(ano,local_id)|>
  dplyr::summarize(
    propmin=weighted.mean(propmin,qtd_vinculos_agr),
    brmediamin=dplyr::first(brmediamin)
  )|>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('objetivo4_2',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=propmin/brmediamin
  )


##Objetivo 4_3
objetivo4_3_uf <- emprego_por_cnae_mun|>

  dplyr::mutate(
    regiao = trunc(municipio/1e5),
    uf = trunc(municipio/10000),
    setor = trunc(setor/1000))|>
  dplyr::group_by(uf, ano, setor, regiao) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE)) |>
  dplyr::group_by(uf, ano) |>
  dplyr::mutate(vinc_munic = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by( setor, ano) |>
  dplyr::mutate(vinc_setor_br = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(vinc_br = sum(vinc_setor, na.rm = TRUE),
                value = abs((vinc_setor/vinc_munic)-(vinc_setor_br/vinc_br))) |>
  dplyr::group_by(uf, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)/2) |>
  dplyr::mutate(variavel = "objetivo4_3") |>
  dplyr::rename(geoloc_id = uf) |>
  dplyr::select(ano, geoloc_id, variavel, value) |>
  dplyr::ungroup()


objetivo4_3_uf <-
  objetivo4_3_uf |>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(mdata_id=recupmdata_id("objetivo4_3",mdr),
                   refdate=as.Date(paste0(ano,'-12-31')),
                   local_id,value)

objetivo4_3_uf <- objetivo4_3_uf[!is.na(objetivo4_3_uf$local_id),]

## e1_1  desprod


## e1_2  desprod

desprod2_uf <-
  empregoformalmun|>
  dplyr::left_join(emprego_industria_municipal)|>
  dplyr::mutate(refdate=as.Date(paste0(ano,"-12-31")),
                geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(across(c(qtd_vinculos_agr,value),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(prop_emprego_industrial=100*qtd_vinculos_agr/value)


desprod2_uf <-
  desprod2_uf|>
  dplyr::left_join(ufgeoloc)

desprod2_uf <-
  desprod2_uf|>
  dplyr::transmute(mdata_id=recupmdata_id("desprod3",mdr),
                   refdate,
                   local_id,value=prop_emprego_industrial)


## e1_3  desprod

desprod3_uf <- salmedio_semadmpub_municipal|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize(across(c(massa_salarial,qtd_vinculos_agr),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(desprod3=massa_salarial/qtd_vinculos_agr)|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   geoloc_id,
                   desprod3)


desprod3_uf <-
  desprod3_uf|>
  dplyr::left_join(ufgeoloc)

desprod3_uf <-
  desprod3_uf|>
  dplyr::transmute(mdata_id=recupmdata_id("desprod3",mdr),
                   refdate,
                   local_id,value=desprod3)

desprod3_uf <- desprod3_uf[!is.na(desprod3_uf$local_id),]
## e1_4 desprod

desprod4_uf <- escala_prod|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize(across(c(massa_salarial,qtd_estabelecimentos),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(desprod4 = massa_salarial/qtd_estabelecimentos)


desprod4_uf <-
  desprod4_uf|>
  dplyr::left_join(ufgeoloc)

desprod4_uf <-
  desprod4_uf|>
  dplyr::transmute(mdata_id=recupmdata_id("desprod4",mdr),
                   refdate=as.Date(paste0(ano,'-12-31')),
                   local_id,value=desprod4)

desprod4_uf <- desprod4_uf[!is.na(desprod3_uf$local_id),]

## e2_1 citec



citec1_uf <-
  popmunicipal|>dplyr::transmute(
    ano=as.integer(substr(refdate,1,4)),
    municipio=local,
    geoloc_id=trunc(local/1e4),
    pop=populacao)|> dplyr::ungroup()|>
  dplyr::right_join(peqemp_biotecsaude_mun)|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize(across(c(estabelecimento,pop),somasna))|>
  dplyr::transmute(
    refdate=as.Date(paste0(ano,"-12-31")),
    geoloc_id,
    citec1=ifelse(is.na(estabelecimento),0,estabelecimento)*1e6/pop
  )|>
  dplyr::filter(!is.na(geoloc_id))

citec1_uf <-
  citec1_uf|>
  dplyr::left_join(ufgeoloc)

citec1_uf <-
  citec1_uf|>dplyr::ungroup()|>
  dplyr::transmute(mdata_id=recupmdata_id("citec1",mdr),
                   refdate,
                   local_id,value=citec1)


## e2_2 citec

citec2_uf <-
    popmunicipal|>dplyr::transmute(
    periodo=dezembriza(data.table::as.IDate(as.Date(refdate,tryFormats = "%Y-%m-%n"))),
    local,
    geoloc_id=trunc(local/1e4),
    pop=populacao)|> dplyr::ungroup()|>
  dplyr::right_join(empregos_citec_mun)|>
  dplyr::group_by(periodo,geoloc_id)|>
  dplyr::summarize(across(c(qtd_vinculos_agr,pop),somasna))|>
  dplyr::ungroup()|>
  dplyr::transmute(
    refdate=periodo,
    geoloc_id,
    citec2=ifelse(is.na(qtd_vinculos_agr),0,qtd_vinculos_agr)*1e6/pop
  )

citec2_uf <-
  citec2_uf|>
  dplyr::left_join(ufgeoloc)

citec2_uf <-
  citec2_uf|>
  dplyr::transmute(mdata_id=recupmdata_id("citec2",mdr),
                   refdate,
                   local_id,value=citec2)

citec2_uf <-
  citec2_uf[!(is.na(citec2_uf$local_id)),]
## e2_3 citec

citec3_uf <-

  datasus_popmun|>dplyr::mutate(ano=lubridate::year(refdate))|>
  dplyr::ungroup()|>dplyr::transmute(periodo=data.table::as.IDate(as.Date(paste0(ano,"-12-31"))),
                                     local,populacao)|>
  dplyr::left_join(empregos_cnae2citec_mun)|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(periodo,geoloc_id)|>
  dplyr::summarize(across(c(populacao,qtd_vinculos_agr),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(citec3 =qtd_vinculos_agr*1e6/populacao)

citec3_uf <-
  citec3_uf|>
  dplyr::left_join(ufgeoloc)

citec3_uf <-
  citec3_uf|>
  dplyr::filter(periodo>'2012-12-31')|>
  dplyr::transmute(mdata_id=recupmdata_id('citec3',mdr),refdate=periodo,local_id,value=citec3)


## e2_4 citec

citec4_uf <- popmunicipal|>
  dplyr::filter(lubridate::year(refdate)>1999)|>
  dplyr::left_join(inventores|>dplyr::mutate(
    refdate=as.Date(paste0(ano,"-07-01")),
    local=as.numeric(cd_ibge_cidade)))|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(across(c(populacao,n_dep_patentes),somasna))|>
  dplyr::mutate(across(contains("n_dep_p"),\(x){ifelse(is.na(x),0,x)}),
                citec4inventor=n_dep_patentes*1e5/populacao)

citec4_uf <- citec4_uf|>dplyr::filter(!is.na(local_id))|>dplyr::select(-local_name,-geoloc_id)|>dplyr::mutate(mdata_id=recupmdata_id("citec4",mdr))
## 2024 sem dados municipais

##Mas há dados por UF

linkufbadepi <- "https://www.gov.br/inpi/pt-br/central-de-conteudo/estatisticas/arquivos/estatisticas-preliminares/tabelas_completas_dez_2024.xls"

f <- tempfile(fileext = "xls")

download.file(linkufbadepi,f)


patuf24 <- readxl::read_xls(f,sheet='2.7_PTN_TIPO_UF ',skip=6)
## NA na coluna 2 = dado da macrorregião , retirar
names(patuf24)[2] <- 'nome_uf'
patuf24 <- patuf24|>dplyr::filter(!is.na(nome_uf))|>
  dplyr::left_join(ufgeoloc,by=c('nome_uf'='local_name'))|>
  dplyr::mutate(`PATENTE DE INVENÇÃO`=tidyr::replace_na(`PATENTE DE INVENÇÃO`,0),
                `MODELO DE UTILIDADE`=tidyr::replace_na(`MODELO DE UTILIDADE`,0))|>
  dplyr::transmute(geoloc_id,npatuf=`PATENTE DE INVENÇÃO`+`MODELO DE UTILIDADE`,
                   refdate=as.Date('2024-07-01'))

citec4_uf <-
  citec4_uf|>
  dplyr::left_join(patuf24)|>
  dplyr::mutate(citec4inventor=dplyr::case_when(
    lubridate::year(refdate)==2024 ~ npatuf*1e5/populacao,
    T ~ citec4inventor
  ))

citec4_uf <-
  citec4_uf|>dplyr::select(refdate,geoloc_id,value=citec4inventor)

citec4_uf <-
  citec4_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::filter(!is.na(local_name))

citec4_uf|>
  dplyr::filter(!is.na(local_id))|>
  dplyr::transmute(mdata_id=recupmdata_id('citec4',mdr),
                   refdate,local_id,value)

lubridate::month(citec4_uf$refdate) <- 12

## e3_1 educ

arqmicrocensos <- list.files(path = "coleta/cache/educ1_esgoto",
                             pattern = "microda.*.csv",
                             full.names = TRUE,
                             recursive = TRUE)

reading_data_uf <- function(x){
  data <-
    data.table::fread(
      x, sep = ";", dec = ",",
      select = c("NU_ANO_CENSO", "CO_MUNICIPIO", "IN_ESGOTO_REDE_PUBLICA"),
      fill=TRUE) |>
    dplyr::mutate(geoloc_id=trunc(`CO_MUNICIPIO`/1e5))|>
    dplyr::group_by(NU_ANO_CENSO, geoloc_id) |>
    dplyr::summarise(qtd_esgoto = sum(IN_ESGOTO_REDE_PUBLICA, na.rm = TRUE),
                     n = sum(!is.na(IN_ESGOTO_REDE_PUBLICA))) |>
    dplyr::transmute(NU_ANO_CENSO = NU_ANO_CENSO,
                     geoloc_id,
                     value = qtd_esgoto/n*100,
                     variavel = "educ1") |>
    dplyr::ungroup() |>
    dplyr::rename(ano = NU_ANO_CENSO)
  return(data)
}

educ1_uf <- purrr::map_dfr(.x = arqmicrocensos, .f = reading_data_uf) |>
  dplyr::select(ano, geoloc_id, variavel, value) |>
  dplyr::ungroup()

educ1_uf <-
  educ1_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id("educ1",mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value
  )

## e3_2 educ

reading_educ2_uf <- function(x){
  data <- data.table::fread(x, sep = ";", dec = ",", select = c("NU_ANO_CENSO", "CO_MUNICIPIO", "IN_INTERNET"),fill=TRUE) |>
    dplyr::mutate(geoloc_id=trunc(`CO_MUNICIPIO`/1e5))|>
    dplyr::group_by(NU_ANO_CENSO, geoloc_id) |>
    dplyr::summarise(qtd_internet = sum(IN_INTERNET, na.rm = TRUE),
                     n = sum(!is.na(IN_INTERNET))) |>
    dplyr::transmute(NU_ANO_CENSO = NU_ANO_CENSO,
                     geoloc_id,
                     value = qtd_internet/n*100,
                     variavel = "educ2") |>
    dplyr::ungroup() |>
    dplyr::rename(ano = NU_ANO_CENSO)
  return(data)
}

educ2_uf <- purrr::map_dfr(.x = arqmicrocensos, .f = reading_educ2_uf)

educ2_uf <-
  educ2_uf |>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id('educ2',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value
  )

## e3_3 educ

# ('Grupo 1' nas planilhas municipais do INEP), total (urbana e rural),
# de todas as dependências administrativas (categoria 'total' de dependência
# administrativa), por município e ano.

#afd

le_afduf <- educabR::le_afd
le_afduf <- edit(le_afduf)
le_afd_uf <- \(ano) {
  le_afduf(ano,regiao='uf',localizacoes = "total",
           dependencias = "total",
           niveis = "ensino_medio",
           subniveis = "total")|>
    dplyr::left_join(ufgeoloc,by=c('uf'='local_name'))|>
    dplyr::filter(!is.na(geoloc_id))|>
    dplyr::filter(indicador_afd=='grupo_1',
                  dependencia_administrativa=='total',
                  localizacao=='total',
                  nivel=='ensino_medio')|>
    dplyr::transmute(ano,
                     geoloc_id,
                     afd_uf=valor)
}

afd_ufs <- data.table::rbindlist(
  lapply(2013:2024,le_afd_uf)
)

# afd_ufs1316 <- data.table::rbindlist(
#   lapply(2013:2016,le_afd_uf)
# )
#
# afd_ufs <- data.table::rbindlist(list(afd_ufs,afd_ufs1316))

afd_ufs <-
  afd_ufs|>dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id("educ3",mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=afd_uf
  )

educ3_uf <- afd_ufs


## e3_4 educ

educ4_uf <- idebmediapufs

educ4_uf <-
  educ4_uf|>
  dplyr::transmute(
    mdata_id=recupmdata_id('educ4',mdr),
    refdate,local_id,value
  )

## e4_1 infra

infra1_uf <- readr::read_csv('coleta/cache/infra1_aedi/infra1_aedi.csv')

dezembriza <- \(x){
  lubridate::month(x) <- 12
  lubridate::day(x) <- 31
  x
}

infra1_uf <- infra1_uf|>
  dplyr::mutate(munic_mov=trunc(geoloc_id/10))|>
  dplyr::left_join(datasus_popmun|>dplyr::mutate(across(refdate,dezembriza)))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/1e5))|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(infra1_uf=weighted.mean(infra1,populacao,na.rm = TRUE))|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)

infra1_uf <-
  infra1_uf|>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('infra1',mdr),
    refdate,local_id,value=infra1_uf
  )

infra1_uf_pre2022 <-
  DBI::dbGetQuery(
    con,
    "SELECT refdate,geoloc_id,value from named_datavalues
  LEFT JOIN local on named_datavalues.local_id = local.local_id
  WHERE orig_name = 'infra1'")

infra1_uf_pre2022 <-
  infra1_uf_pre2022|>
  dplyr::mutate(munic_mov=trunc(geoloc_id/10))|>
  dplyr::left_join(datasus_popmun|>dplyr::mutate(across(refdate,dezembriza)))|>
  dplyr::mutate(geoloc_id=trunc(geoloc_id/1e5))|>
  dplyr::group_by(refdate,geoloc_id)|>
  dplyr::summarize(value=weighted.mean(value,populacao,na.rm = TRUE))|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)


infra1_uf <-
  infra1_uf_pre2022|>
  dplyr::transmute(mdata_id=infra1_uf$mdata_id[1],refdate,local_id,value)
  dplyr::bind_rows(infra1_uf)

## e4_2 infra

basecalc <- readRDS('coleta/cache/infra2_uf/basecalc2017_2024.rds')


infra2_uf <-  basecalc[,geoloc_id:=trunc(codigo_ibge_municipio/1e5)]

infra2_uf <- infra2_uf[,.(total_acessos=sum(acessos)),by= .(ano,geoloc_id,velocidade_alta)]

infra2_uf <- data.table::dcast(infra2_uf,ano+geoloc_id ~ velocidade_alta,value.var = 'total_acessos',fill = 0)

names(infra2_uf)[3:4] <- c('normal','velocidade_alta')

infra2_uf <- infra2_uf[,infra2_uf := 100*velocidade_alta/(normal+velocidade_alta)][,.(ano,geoloc_id,infra2_uf)]

infra2_uf[,ano:=as.Date(paste0(ano,"-12-31"))]

infra2_uf <-
  infra2_uf[,mdata_id:=recupmdata_id('infra2',mdr)]|>
  dplyr::left_join(ufgeoloc)

infra2_uf <-
  infra2_uf|>
  dplyr::select(mdata_id,refdate=ano,local_id,value=infra2_uf)

## e4_3  infra

infra3_uf <-
  infra3_aedi_b|>
  dplyr::mutate(geoloc_id=trunc(as.numeric(as.character(MUNIC_MOV))/1e4))|>
  dplyr::group_by(ANO_CMPT,geoloc_id)|>
  dplyr::summarize(across(internacoes,somasna))|>
  janitor::clean_names()|>
  dplyr::transmute(refdate=as.Date(paste0(as.character(ano_cmpt),"-12-31")),geoloc_id,internacoes)|>
  dplyr::left_join(popmunicipal|>dplyr::mutate(refdate=as.Date(paste0(lubridate::year(refdate),"-12-31")),geoloc_id=trunc(local/1e4))|>
                     dplyr::group_by(refdate,geoloc_id)|>dplyr::summarize(across(populacao,somasna)))|>
  dplyr::transmute(refdate,geoloc_id,infra3=1e4*internacoes/populacao)

infra3_uf <-
  infra3_uf|>
  dplyr::left_join(ufgeoloc)

infra3_uf <-
  infra3_uf|>dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('infra3',mdr),
    refdate,
    local_id,
    value=infra3
  )
## e4_4   infra

infra4_uf <-
  infra4_aedi|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::ungroup()|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize(across(c(value,populacao),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(infra4=value/populacao)


infra4_uf <-
  infra4_uf|>
  dplyr::left_join(ufgeoloc)

infra4_uf <-
  infra4_uf|>dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('infra4',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=infra4
  )

infra4_uf <- infra4_uf[!is.na(infra4_uf$local_id),]

## e5_1 dessoc

dessoc1_uf <- internacao_para_indicador|>
  dplyr::mutate(geoloc_id=trunc(cd_mun/1e4))|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize(across(contains("internacao"),somasna))|>
  dplyr::ungroup()|>
  dplyr::mutate(dessoc1=100*internacao_desnutricao/internacao_total)

dessoc1_uf <-
  dessoc1_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::filter(!is.na(local_name))

dessoc1_uf <-
  dessoc1_uf|>
  dplyr::transmute(
    mdata_id=recupmdata_id('dessoc1',mdr),
    refdate=as.Date(paste0(ano,"-12-31")),
    local_id,
    value=dessoc1
  )

## e5_2 dessoc

dessoc2_uf <- dessoc2|>
  dplyr::filter(grepl("^12",Referência))|>
  dplyr::transmute(refdate = as.Date(paste0("31/",Referência),tryFormats= "%d/%m/%Y"),
                   local=Código,
                   geoloc_id = trunc(Código/1e4),
                   pessoas_fam_ate_1sm = `Quantidade de pessoas cadastradas em famílias com renda total mensal até 1 salário mínimo`
  )|>
  dplyr::left_join(popmunicipal|>dplyr::transmute(refdate= as.Date(paste0(lubridate::year(refdate),'-12-31')) ,geoloc_id=trunc(local/1e4),local,populacao))|>
                     dplyr::group_by(refdate,geoloc_id)|>dplyr::summarize_all(somasna)

dessoc2_uf <- dessoc2_uf|>dplyr::ungroup()|>
  dplyr::mutate(dessoc2_uf=pessoas_fam_ate_1sm/populacao)

dessoc2_uf <-
  dessoc2_uf|>
  dplyr::left_join(ufgeoloc)

dessoc2_uf <-
  dessoc2_uf|>
  dplyr::filter(!is.na(local_name))

dessoc2_uf <-
  dessoc2_uf|>
  dplyr::transmute(
    mdata_id=recupmdata_id('dessoc2',mdr),
    refdate,
    local_id,
    value=dessoc2_uf
  )

## e5_3 dessoc

le_idadedist_uf <- educabR::le_idadeserie
le_idadedist_uf <- edit(le_idadedist_uf)

leuf_dis <- \(ano) {
  le_idadedist_uf(ano)|>
    dplyr::filter(rede == 'Total',detalhe %in%
                    c('Total_Ensino Fundamental_Total',
                      "Total_Taxa de Distorção Idade-Série - Ensino Fundamental_Total Fundamental" ))|>
    dplyr::left_join(ufgeoloc,by=c("unidade_geografica"="local_name"))|>
    dplyr::filter(!is.na(geoloc_id))|>
    dplyr::select(ano,geoloc_id,unidade_geografica,valor)
}

dessoc3_uf <-
  data.table::rbindlist(
    lapply(2017:2024,leuf_dis))


#dessoc3_uf16 <- leuf_dis(2016)
#dessoc3_uf15 <- leuf_dis(2015)
#dessoc3_uf14 <- leuf_dis(2014)

dessoc3_uf <-
  dessoc3_uf|>dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id('dessoc3',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=valor
  )
## e5_4 dessoc

dessoc4_uf <- salmedio_semadmpub_municipal_genero|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)|>
  dplyr::mutate(dessoc4=(massa_salarial_2/qtd_vinculos_agr_2)/(massa_salarial_1/qtd_vinculos_agr_1))

dessoc4_uf <-
  dessoc4_uf|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(ano,local_id,local_name,dessoc4)

dessoc4_uf <-
  dessoc4_uf|>dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('dessoc4',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=dessoc4
  )

dessoc4_uf <- dessoc4_uf[!is.na(dessoc4_uf$local_id),]
## e6_1 governativas

governativas1_uf <- dirigentesmunicipais|>
  dplyr::left_join(csuperior_dirigentesmunicipais|>dplyr::rename(qtd_vinculos_agr_csuperior=qtd_vinculos_agr,
                                                                 massa_salarial_sup=massa_salarial))|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>dplyr::select(-local)|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)

governativas1_uf <-
  governativas1_uf|>dplyr::ungroup()|>
  dplyr::mutate(governativas1=100*qtd_vinculos_agr_csuperior/
                  qtd_vinculos_agr
  )|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(ano,local_id,local_name,governativas1)

governativas1_uf <-
  governativas1_uf|>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('governativas1',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=governativas1
  )

## e6_2 governativas

governativas2_uf <- servidoresmunicipais|>
  dplyr::left_join(csuperior_servidoresmunicipais)|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>dplyr::select(-local)|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)

governativas2_uf <-
  governativas2_uf|>dplyr::ungroup()|>
  dplyr::mutate(governativas2=100*qtd_vinculos_agr_csuperior/
                  qtd_vinculos_agr
  )|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::select(ano,local_id,local_name,governativas2)

governativas2_uf <-
  governativas2_uf|>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('governativas2',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value=governativas2
  )


## e6_3 governativas
governativas3_uf <- servidoresmunicipais|>
  dplyr::left_join(csuperior_servidoresmunicipais)|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>dplyr::select(-local)|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(local=local_id,ano,governativas3=massa_salarial/qtd_vinculos_agr)

governativas3_uf <-
  governativas3_uf|>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('governativas3',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id=local,
    value=governativas3
  )

# e6_4 governativas

governativas4_uf <- readRDS("coleta/cache/governativas4_aedi/ifsm_ufs.rds")

governativas4_uf <-
  governativas4_uf|>
  dplyr::left_join(ufgeoloc,by=c('uf'='geoloc_id'))|>
  dplyr::transmute(
    mdata_id=recupmdata_id('governativas4',mdr),
    refdate=as.Date(paste0(ano,'-12-31')),
    local_id,
    value
  )


## e7_1 sust
sust1_uf <-
  popmunicipal|>
  dplyr::mutate(local,ano=lubridate::year(refdate))|>
  dplyr::select(-refdate)|>
  dplyr::left_join(empregos_reci_gresid)|>
  dplyr::mutate(geoloc_id=trunc(local/1e4))|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)|>
  dplyr::ungroup()|>
  dplyr::mutate(sust1_uf=1e6*qtd_vinculos_agr/populacao)|>
  dplyr::left_join(ufgeoloc)|>dplyr::filter(!is.na(local_id))|>
  dplyr::transmute(local_id,refdate=as.Date(paste0(ano,'-12-31')),sust1_uf)

sust1_uf <-
  sust1_uf |>
  dplyr::ungroup()|>
  dplyr::transmute(
    mdata_id=recupmdata_id('sust1',mdr),
    refdate,
    local_id,
    value=sust1_uf
  )

sust1_uf <- sust1_uf[!is.na(sust1_uf$local_id),]
## e7_2 sust

map_uf <-
   geobr::read_state(year=2020)|>
  dplyr::mutate(area_uf=sf::st_area(geom))|>sf::st_drop_geometry()

sust2_uf <- todos_desmatamento|>
  dplyr::mutate(geoloc_id=trunc(geocode_ibge/1e5))|>
  dplyr::group_by(year,geoloc_id)|>
  dplyr::summarise(area_desmatamento=sum(areakm,na.rm = TRUE)
  )|>
  dplyr::ungroup()|>
  dplyr::left_join(map_uf,by=c("geoloc_id"="code_state"))|>
  dplyr::mutate(prop_desmatamento = 100*area_desmatamento*1e6/as.vector(area_uf))

sust3_uf <- sust2_uf

sust2_uf <-
  sust2_uf|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id('sust2',mdr),
    refdate=as.Date(paste0(year,'-12-31')),
    local_id,
    value=prop_desmatamento
  )

## e7_3 sust

sust3_uf <- sust3_uf|>
  dplyr::mutate(prop_desmatamento=ifelse(prop_desmatamento>100,100,prop_desmatamento))|>
  dplyr::group_by(geoloc_id)|>
  dplyr::mutate(
    desmatamento_acumulado = ifelse(cumsum(prop_desmatamento)>100,100,cumsum(prop_desmatamento)))

sust3_uf <-
  sust3_uf|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id('sust3',mdr),
    refdate=as.Date(paste0(year,'-12-31')),
    local_id,
    value=desmatamento_acumulado
  )


## e7_4 sust



sust4_uf <- readRDS('coleta/cache/sust4_aedi/emissoes_agro_industria.rds')

sust4_uf <-
  sust4_uf|>
  dplyr::transmute(ano,value,geoloc_id=trunc(id_territorio/1e5)-100)|>
  dplyr::group_by(ano,geoloc_id)|>
  dplyr::summarize_all(somasna)|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),geoloc_id,value=value/1e6)

sust4_uf <-
  sust4_uf|>
  dplyr::ungroup()|>
  dplyr::left_join(ufgeoloc)|>
  dplyr::transmute(
    mdata_id=recupmdata_id('sust4',mdr),
    refdate,
    local_id,
    value
  )



indicadores_uf <- ls(pattern="^[^lr].*_uf$")


indicadores_uf_bd <- \(tabela) {
  print(paste0("atualizando tabela ",tabela))
  tabelai <- get(tabela,envir = .GlobalEnv)

  dbx::dbxUpsert(pndrupsert,'data_values',
                 tabelai,where_cols=c('mdata_id','refdate','local_id'))

}

lapply(indicadores_uf[9:length(indicadores_uf)],indicadores_uf_bd)
