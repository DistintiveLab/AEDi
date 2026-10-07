# Gênese do banco "aedidb": popula geoloc/local (municípios, UF, região,
# Brasil, meso/micro com hack +10000000, RGIM/RGINT 2020, estratos PNAD),
# datagroups (Eixos/Objetivos PNDR, Tipologia 2018, Faixa de Fronteira,
# Amazônia Legal, Semiárido, SUDENE, RGIM/RGINT) e carrega valores do
# Painel da PNDR (mdata/mdata_group/data_values, compostos via XLSX,
# metadados via indicsprev ODS).
#
# Cópia sanitizada de pndr_dashboard/data-raw/populate_initialdb.R — a
# cópia CANÔNICA permanece lá; diferencial daqui: credenciais via
# Sys.getenv() sem literal (repo público; senha vem de .Renviron/env).
#
# As cargas dependem de "dadostat/" (Manual_Painel.html,
# tipologia2018_ajustada.csv, Cálculo Painel de Indicadores/*.RDS/*.xlsx,
# indicsprev_v0/v1.ods), que só existe no pndr_dashboard: rodar da raiz
# desse projeto. Downloads externos: kelvins/Municipios-Brasileiros,
# geobr 2020, SUDENE 2021 (ODS), Faixa de Fronteira 2022 (SHP),
# estratos PNAD (painel.ibge.gov.br).

#' Importa previos PNDR
#'
#' Prepara dados já calculados do Painel da PNDR
#'
#' @export

#AEDi:::prepare_db()
pega_pndr_previo <- \(dbtype="sqlite"){
  #populate static tables
  if(dbtype!="pgsql") {
    con <- DBI::dbConnect(RSQLite::SQLite(),"dashboard_db.sqlite")
    ##GEOLOC
    muncodes <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv")

    geoloc <- brazilmaps::get_brmap(
      geo="City",
      geo.filter=list(City = sort(muncodes$codigo_ibge)),
      class = 'sf'
    )
    geoloc<- sf::st_transform(geoloc,crs = "+proj=longlat +datum=WGS84 +no_defs")

    geolocb <- as.data.frame(
      cbind(
        geoloc_id=geoloc$City,
        geometry=
          sf::st_as_text(sf::st_sfc(geoloc$geometry))))


    DBI::dbAppendTable(con,"geoloc",geolocb)

    ##local
    local <- data.frame(local_id = 1:nrow(geoloc),local_name=geoloc$nome,geoloc_id=geoloc$City)

    DBI::dbAppendTable(con,"local",local)
  } else {

    con <- DBI::dbConnect(RPostgres::Postgres(),
                          user = Sys.getenv("user", "aedi"),
                          password = Sys.getenv("password"),
                          host = Sys.getenv("host", "127.0.0.1"),
                          dbname = Sys.getenv("dbname", "aedidb"))

    muncodes <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv")

    #https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2023/DTB_2023.zip
    ## Pensar em como incorporar Boa esperança do Norte
    ## Atualizar shape de Sorriso, Ubiratã, indepdente do ano?
    ## Colocar shape novo de Boa Esperança do Norte
    ## Old microrregion and mesoregion IBGE (vigente até 2017)
    retwritegeo <- \(levelgeo="City") {
      geoloc <- brazilmaps::get_brmap(
        geo=levelgeo,
        class = 'sf'
      )
      geoloc<- sf::st_transform(geoloc,crs = "+proj=longlat +datum=WGS84 +no_defs")
      if(levelgeo=="Region"){
        geoloc <- geoloc|>sf::st_sf()|> dplyr::rename(nome="desc_rg")
      } else if(levelgeo=="Brazil"){
        geoloc <- sf::st_sf(Brazil=1+as.numeric(DBI::dbGetQuery(con,"select MAX(local_id) from local")),nome="Brasil",geometry=geoloc)
      }
      local <- geoloc|>dplyr::select(geoloc_id=tidyr::all_of(levelgeo),local_name=nome)|>
        dplyr::mutate(across(local_name,stringr::str_to_title),
                      across(local_name,\(x){gsub(" D([^ ]+) "," d\\1 ",x)}))|>
        sf::st_drop_geometry()
      geoloc <- geoloc|>dplyr::select(geoloc_id=tidyr::all_of(levelgeo),geometry)

      sf::st_write(geoloc,con,append=T)
      if (levelgeo=="Brazil"){
        DBI::dbAppendTable(con,"local",cbind(local_id=local$geoloc_id,local))
      } else {
        DBI::dbAppendTable(con,"local",local)
      }



    }
    retwritegeo()
    #OLD IBGE MIcrorregion
    retwritegeo("MicroRegion")
    #OLD IBGE Mesorregion
    retwritegeo("MesoRegion")
    {
      ##Mesoregion codes overlap intermediate region codes!!!.
      #Operate an arbitrary conversion without another ibge geocode overlap
      #15 - nchar(530010805060005) - census tracts
      #8 digit numbers are free as far as we could tell from ibge census_tract 2010 different codes
      #cetracts|>sf::st_drop_geometry()|> dplyr::mutate(across(contains("code"),as.numeric),
      #dezc=dplyr::if_any(dplyr::contains("code"), ~nchar(.,keepNA = F) == 8))|>dplyr::count(dezc)
      DBI::dbExecute(con,"BEGIN TRANSACTION")
      DBI::dbExecute(con,"ALTER TABLE local DROP CONSTRAINT fk_geoloc_geoloc_id;")
      DBI::dbExecute(con,"UPDATE geoloc SET geoloc_id=10000000+geoloc_id WHERE geoloc_id>1000 AND geoloc_id<10000;")

      DBI::dbExecute(con,"UPDATE local SET geoloc_id=10000000+geoloc_id WHERE geoloc_id>1000 AND geoloc_id<10000;")
      DBI::dbExecute(con,"ALTER TABLE local ADD CONSTRAINT fk_geoloc_geoloc_id
                   FOREIGN KEY (geoloc_id) REFERENCES geoloc(geoloc_id);")
      DBI::dbExecute(con,"END TRANSACTION")
    }


  }
  retwritegeo("State")

  retwritegeo("Region")
  ##New immediate and intermediate regions
  geobrcities <- subset(geobr::read_municipality(year=2020,simplified=T),!code_muni %in% (4300000+1:2))

  geobrimmediater <- geobr::read_immediate_region(year=2020,simplified=T)

  geobrintermr <- geobr::read_intermediate_region(year=2020)

  geobrstates <- geobr::read_state(year=2020)

  geobrcitiesm <- geobr::read_municipal_seat(year=2010)

  geobrsemiarid <- geobr::read_semiarid(year = 2022)

  geobramazonia_legal <- geobr::read_amazon(year=2012)

  #sudene
  tmp_file_sudene <- tempfile(fileext = ".ods")

  ibgesudenefile <- "http://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/area_atuacao_SUDENE/2021/SUDENE_2021.ods"
  download.file(ibgesudenefile,tmp_file_sudene)
  ibgesudene <- readODS::read_ods(tmp_file_sudene)

  ##FAIXA DE FRONTEIRA
  #system("mkdir /tmp/ffront")
  dir.create('/tmp/ffront',showWarnings = F)
  f <- "/tmp/ffront/ffront.zip"
  ffronurl <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/municipios_da_faixa_de_fronteira/2022/Sedes_Municipios_Faixa_de_Fronteira_Cidades_Gemeas_2022_shp.zip"
  download.file(ffronurl,f)

  system("unzip -o /tmp/ffront/ffront.zip -d /tmp/ffront")

  ffront <- sf::read_sf("/tmp/ffront/Sedes_Municipios_Faixa_de_Fronteira_Cidades_Gemeas_2022.shp")

  ffront <- sf::st_transform(ffront,crs = "+proj=longlat +datum=WGS84 +no_defs")


  ##FAIXA DE FRONTEIRA

  juntaspa <-
    sf::st_join(geobrcities,
                geobrimmediater|>dplyr::select(contains("immediate"))|>
                  sf::st_buffer(500),join = sf::st_within)
  juntaspa <-
    sf::st_join(juntaspa,
                geobrintermr|>dplyr::select(contains("intermediate"))|>
                  sf::st_buffer(500),join = sf::st_within)
  juntaspa <- sf::st_transform(juntaspa,crs = "+proj=longlat +datum=WGS84 +no_defs")

  geoloc<-geobrintermr|>
    dplyr::select(geoloc_id=code_intermediate,geometry=geom)|>
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  sf::st_write(geoloc,
               con,append=T)

  geoloc<-geobrimmediater|>
    dplyr::select(geoloc_id=code_immediate,geometry=geom)|>
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  sf::st_write(geoloc,
               con,append=T)

  newloc <-DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local;")$max

  local <- geobrintermr|>sf::st_drop_geometry() |>
    dplyr::mutate(local_id=(newloc+1):(newloc+nrow(geobrintermr)))|>
    dplyr::select(local_id,
                  geoloc_id=code_intermediate,
                  local_name=name_intermediate)

  DBI::dbAppendTable(con,"local",local)

  local <- geobrimmediater|>sf::st_drop_geometry() |>
    dplyr::mutate(
      local_id=(newloc+nrow(geobrintermr)+1):(newloc+nrow(geobrintermr)+nrow(geobrimmediater)))|>
    dplyr::select(local_id,
                  geoloc_id=code_immediate,
                  local_name=name_immediate)

  DBI::dbAppendTable(con,"local",local)

  ##Faixa de Fronteira - IN DATA GROUPS

  ##PNAD C EXTRACTS
  munextpnadc <- "https://painel.ibge.gov.br/saibamais/files/Municipios_por_Estratos.csv"
  ##CREATE LOCAL IDS FOR PNAD EXTRACTS WITH
  ##- TAKEN FROM EXAMPLE XLS -CODE = cod_estrato+uf+reg
  #https://painel.ibge.gov.br/saibamais/
  pnadcem <- "dadostat/Municipios_por_Estratos.csv"
  download.file(munextpnadc,pnadcem)
  extratosmun <- readr::read_csv2(pnadcem)
  extratosmun <- geobrcities|>dplyr::left_join(extratosmun,by=c("code_muni"="Código do Município"))
  extrlocs <-
    extratosmun|>
    dplyr::mutate(codmun=paste0(`Código do estrato`,substr(`Código do estrato`,1,2),substr(`Código do estrato`,1,1)))|>
    dplyr::group_by(codmun,`Nome abreviado do estrato`)|>
    dplyr::summarise()|>dplyr::ungroup()

  newloce <-DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local;")$max

  geoloc<-extrlocs|>
    dplyr::select(geoloc_id=codmun,geometry=geom)|>
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  sf::st_write(geoloc,
               con,append=T)

  locale <- extrlocs|>dplyr::ungroup()|>sf::st_drop_geometry() |>
    dplyr::mutate(
      local_id=(newloce+1):(newloce+nrow(extrlocs)))|>
    dplyr::select(local_id,
                  geoloc_id=codmun,
                  local_name=`Nome abreviado do estrato`)

  DBI::dbAppendTable(con,"local",locale)


  ##Brasil
  retwritegeo("Brazil")

  ##datagroup
  urlma <- "dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/Manual_Painel.html"
  manualhtml <- rvest::read_html(urlma)

  get_datagroup <- \(pathx="//*[@id='ind-municipais-desenvolvimento']/ul/li",grpbn="Eixo",
                     delimi=") ") {
    grupo <- gsub("\\\r\\\n"," ",rvest::html_elements(manualhtml,
                                                      xpath=pathx)|>rvest::html_text())
    grupo <- grupo|>
      tibble::as_tibble()|>
      tidyr::separate_wider_delim(value,names=c("datagroup_id","datagroup_desc"),delim=delimi)

    grupo$datagroup_id <- as.numeric(gsub("[^\\d]* ","",grupo$datagroup_id))
    grupo$datagroup_name <- paste(grpbn,grupo$datagroup_id)

    grupo
  }

  grupos <- get_datagroup()
  grupos <- rbind(grupos,
                  data.frame(
                    datagroup_id=8,
                    datagroup_desc="Indicadores de desenvolvimento por estratos da PNAD(não municipais)",
                    datagroup_name="Estratos PNAD"
                  ))

  pathxobj= "//*[@id='objetivos']/ul/li"

  grupos <- dplyr::bind_rows(grupos, get_datagroup(pathxobj,
                                                   grpbn="Objetivo",delimi=": ")|>
                               dplyr::mutate(datagroup_id=max(grupos$datagroup_id)+datagroup_id))

  ##main groups initially populated besides local - tipology
  grupos <- dplyr::bind_rows(
    grupos,
    data.frame(
      datagroup_id = nrow(grupos)+1:2,
      datagroup_name = c("Eixos","Objetivos"),
      datagroup_desc = c("Eixos da PNDR","Objetivos da PNDR")
    ))


  DBI::dbAppendTable(con,"datagroup",grupos)

  #Tipologia Sub-regional da Política Nacional de
  #Desenvolvimento Regional (PNDR)
  dgidtipologia <- DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) FROM datagroup;")$max+1

  DBI::dbAppendTable(
    con,"datagroup",
    data.frame(datagroup_id = dgidtipologia,datagroup_name="Tipologia PNDR 2018",
               datagroup_desc="Tipologia Sub-regional da Política Nacional de\\n
    Desenvolvimento Regional (PNDR)"))

  ##Local groups
  #Tipologia Sub-regional da Política Nacional de
  #Desenvolvimento Regional (PNDR)

  tip2018 <- readr::read_csv("dadostat/tipologia2018_ajustada.csv")

  localgroups <- unique(tip2018$`TIPOLOGIA SUB REGIONAL`)
  #Reodering  1 low-low 9- high-high
  localgroups <- localgroups[c(5,6,7,3,1,2,8,4,9)]
  localg <-   data.frame(
    datagroup_id = (dgidtipologia+1):(dgidtipologia+length(localgroups)),
    datagroup_name = localgroups,
    datagroup_desc = paste(localgroups,"na Tipologia da PNDR - 2018")
  )

  DBI::dbAppendTable(
    con,"datagroup",localg
  )


  ##FAIXA DE FRONTEIRA
  maxgrpid <- DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) FROM datagroup;")$max+1
  maxlocalid <- max(DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local"))
  ### INSERT DATA GROUP SITUAÇÃO QUANTO A FRONTEIRA
  ### INSERT DATA GROUP MUNICÍPIO/ZONA EM FAIXA DE FRONTEIRA
  ### INSERT DATA GROUP MUNICÍPIO/ZONA FORA DA FAIXA DE FRONTEIRA
  DBI::dbAppendTable(
    con,"datagroup",
    data.frame(datagroup_id = maxgrpid:(maxgrpid+2),
               datagroup_name=c("Faixa de Fronteira",
                                "em faixa de fronteira",
                                "fora da faixa de fronteira"),
               datagroup_desc=c("Situação Quanto à Faixa de Fronteira",
                                "Município/Área em Faixa de Fronteira",
                                "Município/Área Fora da Faixa de Fronteira"))
  )


  ffrontcdm <- (ffront|>sf::st_drop_geometry())$CD_MUN
  dgidff <- DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup where
                            datagroup_name LIKE '%em faixa%'")$datagroup_id
  local_group_ff <- geobrcities|>sf::st_drop_geometry()|>
    dplyr::transmute(local_id=1:nrow(geobrcities),
                     datagroup_id=
                       dplyr::case_when(code_muni %in% ffrontcdm ~ dgidff,
                                        T ~ dgidff + 1))

  DBI::dbAppendTable(con,"local_group",local_group_ff)


  regsff <- data.frame(
    datagroup_id = dgidff,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name =
                                   'Faixa de Fronteira'")$datagroup_id)


  DBI::dbAppendTable(con,"group_parent",regsff)

  ##AMAZONIA LEGAL
  maxgrpid <- DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) FROM datagroup;")$max+1
  maxlocalid <- max(DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local"))
  ### INSERT DATA GROUP PARTICIPACAÇÃO NA AMAZÔNIA LEGAL
  ### INSERT DATA GROUP MUNICÍPIO/ÁREA FAZ PARTE DA AMAZÔNIA LEGAL
  ### INSERT DATA GROUP MUNICÍPIO/ÁREA NÃO FAZ PARTE DA AMAZÔNIA LEGAL
  DBI::dbAppendTable(
    con,"datagroup",
    data.frame(datagroup_id = maxgrpid:(maxgrpid+2),
               datagroup_name=c("Participacação na Amazônia Legal",
                                "faz parte da Amazônia Legal",
                                "não faz parte da Amazônia Legal"),
               datagroup_desc=c("Participacação na Amazônia Legal",
                                "Município/Área faz parte da Amazônia Legal",
                                "Município/Área não faz parte da Amazônia Legal"))
  )


  amlegalcm <- sf::st_within(geobrcities,
                             sf::st_buffer(geobramazonia_legal,500),sparse=F)
  amlegalcm <- geobrcities|>sf::st_drop_geometry()|>
    dplyr::mutate(amzlegal=amlegalcm)

  dgidal <- DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup where
                            datagroup_name LIKE '%faz parte da Amaz%'")$datagroup_id
  local_group_al <- amlegalcm|>
    dplyr::transmute(local_id=1:nrow(amlegalcm),
                     datagroup_id=
                       dplyr::case_when(amzlegal ~ dgidal[1],
                                        T ~ dgidal[2]))

  DBI::dbAppendTable(con,"local_group",local_group_al)

  regsal <- data.frame(
    datagroup_id = dgidal,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name LIKE
                                   '%o na Amaz%'")$datagroup_id)


  DBI::dbAppendTable(con,"group_parent",regsal)


  ##SEMI ÁRIDO
  maxgrpid <- DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) FROM datagroup;")$max+1
  maxlocalid <- max(DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local"))
  ### INSERT DATA GROUP PARTICIPAÇÃO NO SEMI-ARIDO PARA FINS DE POLÍTICAS PÚBLICAS
  ### INSERT DATA GROUP MUNICÍPIO/ZONA NO SEMIÁRIDO
  ### INSERT DATA GROUP MUNICÍPIO/ZONA FORA DO SEMIÁRIDO
  DBI::dbAppendTable(
    con,"datagroup",
    data.frame(datagroup_id = maxgrpid:(maxgrpid+2),
               datagroup_name=c("Participação no Semiárido",
                                "no Semiárido",
                                "Fora do Semiárido"),
               datagroup_desc=c("Participação no Semiárido para Fins de Políticas Públicas",
                                "Município/Zona no Semiárido",
                                "Município/Zona Fora do Semiárido"))
  )


  semiacn <- (geobrsemiarid|>sf::st_drop_geometry())$code_muni

  dgidsa <- DBI::dbGetQuery(con,"SELECT
                            regexp_matches(datagroup_name,'(([^o] )|^)[dn]o Semi'),
                            datagroup_id
                            from datagroup")$datagroup_id

  regsemi <- data.frame(
    datagroup_id = dgidsa,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name =
                                   'Participação no Semiárido'")$datagroup_id
  )

  DBI::dbAppendTable(con,"group_parent",regsemi)

  local_group_sa <- geobrcities|>sf::st_drop_geometry()|>
    dplyr::transmute(local_id=1:nrow(geobrcities),
                     datagroup_id=
                       dplyr::case_when(code_muni %in% semiacn ~ dgidsa[1],
                                        T ~ dgidsa[2]))

  DBI::dbAppendTable(con,"local_group",local_group_sa)


  ##SUDENE
  maxgrpid <- DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) FROM datagroup;")$max+1
  maxlocalid <- max(DBI::dbGetQuery(con,"SELECT MAX(local_id) FROM local"))
  ### INSERT DATA GROUP PARTICIPAÇÃO NA SUDENE
  ### INSERT DATA GROUP MUNICÍPIO/ZONA NA SUDENE
  ### INSERT DATA GROUP MUNICÍPIO/ZONA FORA DA SUDENE
  DBI::dbAppendTable(
    con,"datagroup",
    data.frame(datagroup_id = maxgrpid:(maxgrpid+2),
               datagroup_name=c("Participação na SUDENE",
                                "na SUDENE",
                                "Fora da SUDENE"),
               datagroup_desc=c("Participação na SUDENE para Fins de Políticas Públicas",
                                "Município/Zona na SUDENE",
                                "Município/Zona Fora da SUDENE"))
  )


  ibgesudene <- ibgesudene$CD_MUN

  dgidsudene <- DBI::dbGetQuery(con,"SELECT
                            regexp_matches(datagroup_name,'(([^o] )|^)[dn]a SUDENE'),
                            datagroup_id
                            from datagroup")$datagroup_id

  regsudene <- data.frame(
    datagroup_id = dgidsudene,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name =
                                   'Participação na SUDENE'")$datagroup_id
  )

  DBI::dbAppendTable(con,"group_parent",regsudene)

  local_group_sudene <- geobrcities|>sf::st_drop_geometry()|>
    dplyr::transmute(local_id=1:nrow(geobrcities),
                     datagroup_id=
                       dplyr::case_when(code_muni %in% ibgesudene ~ dgidsudene[1],
                                        T ~ dgidsudene[2]))

  DBI::dbAppendTable(con,"local_group",local_group_sudene)


  ###DATA GROUPING FOR IMMEDIATE AND INTERMEDIATE REGION
  dgidmax<-DBI::dbGetQuery(con,"SELECT MAX(datagroup_id) from datagroup;")$max+1

  iminreg <- data.frame(
    datagroup_id= (dgidmax):(dgidmax+1),
    datagroup_name=c("Regiões Imediatas - IBGE 2020",
                     "Regiões Intermediárias - IBGE 2020"),
    datagroup_desc = c("Regiões Imediatas - IBGE 2020",
                       "Regiões Intermediárias - IBGE 2020")
  )
  DBI::dbAppendTable(con,"datagroup",iminreg)

  ufreg <- data.frame(
    datagroup_id=(dgidmax+2):(dgidmax+3),
    datagroup_name=c("UF","Região"),
    datagroup_desc=c("Unidades Federativas","Macrorregião brasileira")
  )

  DBI::dbAppendTable(con,"datagroup",ufreg)


  imrdrop <- geobrimmediater|>sf::st_drop_geometry()
  prim <- (dgidmax+4)
  imreg <- data.frame(
    datagroup_id= prim:(prim-1+nrow(imrdrop)),
    datagroup_name=imrdrop$name_immediate,
    datagroup_desc = paste0("Região Imediata de ",
                            imrdrop$name_immediate,", ",
                            imrdrop$name_state)
  )

  DBI::dbAppendTable(con,"datagroup",imreg)

  inrdrop <- geobrintermr|>sf::st_drop_geometry()
  prin <- (prim+nrow(imrdrop))

  inreg <- data.frame(
    datagroup_id= (prin):(prin-1+nrow(inrdrop)),
    datagroup_name=inrdrop$name_intermediate,
    datagroup_desc = paste0("Região Intermediária de ",
                            inrdrop$name_intermediate,", ",
                            inrdrop$name_state)
  )

  DBI::dbAppendTable(con,"datagroup",inreg)



  tabmunesp <- juntaspa|>sf::st_drop_geometry()

  harmoniza_im <- data.frame(
    datagroup_id = prim:(prim-1+nrow(imrdrop)),
    geoloc_id = imrdrop$code_immediate
  )

  harmoniza_in <- data.frame(
    datagroup_idin = prin:(prin-1+nrow(inrdrop)),
    geoloc_id = inrdrop$code_intermediate
  )

  tabmunesp <-
    tabmunesp|>
    dplyr::left_join(harmoniza_im, by = c("code_immediate"="geoloc_id"))

  tabmunesp <-
    tabmunesp|>
    dplyr::left_join(harmoniza_in, by = c("code_intermediate"="geoloc_id"))


  local <- DBI::dbGetQuery(con,"SELECT * from local;")

  #manual hack, interm region got nchar 7 also from IBGE
  localmun <- (local|>dplyr::filter(nchar(geoloc_id)==7, local_id<5580 | local_id>7086))$local_id

  regimgroup <- data.frame(
    local_id= localmun,
    datagroup_id= tabmunesp$datagroup_id
  )

  DBI::dbAppendTable(con,"local_group",regimgroup)

  regingroup <- data.frame(
    local_id= localmun,
    datagroup_id= tabmunesp$datagroup_idin
  )

  DBI::dbAppendTable(con,"local_group",regingroup)


  #group_parent

  regim <- data.frame(
    datagroup_id = imreg$datagroup_id,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name =
                                   'Regiões Imediatas - IBGE 2020'")$datagroup_id
  )

  DBI::dbAppendTable(con,"group_parent",regim)

  regin <- data.frame(
    datagroup_id = inreg$datagroup_id,
    datagroup_parentid = DBI::dbGetQuery(con,"SELECT datagroup_id from datagroup
                                   WHERE datagroup_name =
                                   'Regiões Intermediárias - IBGE 2020'")$datagroup_id
  )

  DBI::dbAppendTable(con,"group_parent",regin)



  ##Get Eixos and Objetivos parent and subgroups
  pgeo <- \(pgroup="Eixo") {
    data.frame(
      datagroup_id = grupos[grepl(paste0(pgroup," "),
                                  grupos$datagroup_name),]$datagroup_id,
      datagroup_parentid = grupos[grepl(paste0(pgroup,"s"),
                                        grupos$datagroup_name),]$datagroup_id
    )
  }
  pgroups <- data.table::rbindlist(lapply(c("Eixo","Objetivo"),pgeo))

  #add local_group parenting for typology
  pgroups <- dplyr::bind_rows(
    pgroups,
    data.frame(datagroup_id=(dgidtipologia+1):(dgidtipologia+length(localgroups)),
               datagroup_parentid=dgidtipologia)
  )

  DBI::dbAppendTable(con,"group_parent",pgroups)

  ##local_group

  ##Get local_id for all municipalities in tipology
  locids <- DBI::dbGetQuery(con,"SELECT local_id,geoloc_id from local WHERE local_id < 5571")
  tip2018 <- tip2018|>
    dplyr::left_join(locids,
                     by=c("code_muni"="geoloc_id"))

  lcgroup <- tip2018|>
    dplyr::left_join(localg,by=c("TIPOLOGIA SUB REGIONAL"="datagroup_name"))|>
    dplyr::select(local_id,datagroup_id)

  DBI::dbAppendTable(con,"local_group",lcgroup)
  ##Data values and mdata
  rdsbpath <- "dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/"
  alvos <- list.files(path=rdsbpath,pattern="*.RDS",full.names = T)
  lapply(alvos,\(x){
    assign(gsub(".*/([^/]+)\\.RDS$","\\1",x),readRDS(x),envir=.GlobalEnv)
  })

  ##fix pnad_c with extracts with different colname
  ##CREATE LOCAL IDS FOR PNAD EXTRACTS WITH
  ##- TAKEN FROM EXAMPLE XLS -CODE = cod_estrato+uf+reg
  `8_pnadc_estratos` <-
    `8_pnadc_estratos` |>dplyr::rename(codmun=cod_estrato)|>
    dplyr::select(2,1,3,4)|>
    dplyr::mutate(codmun=paste0(codmun,substr(codmun,1,2),substr(codmun,1,1)))




  nmarqind <- ls(pattern="\\d_ind|\\d_pnadc")
  lista_ind <- lapply(nmarqind,
                      \(x){(get(x))})
  names(lista_ind) <- nmarqind


  tiguais <-   all(
    sort(
      Reduce(
        intersect,
        lapply(lista_ind,names))) ==
      sort(names(lista_ind[[1]])))

  if (tiguais) {


    lista_ind <- data.table::rbindlist(lista_ind,use.names = T)




  }

  inds <- unique(lista_ind$variavel)


  ##fix compound indicators not found in rds files:
  calvos <- list.files(path=rdsbpath,pattern="*.xlsx",full.names = T)

  retrieve_comp <- \(x){
    x <- readxl::read_xlsx(x)

    if(!("code_muni6" %in% names(x))) {
      x <- dplyr::select(x,Cdgdest,contains("comp_"))
    } else {
      x <- dplyr::select(x,code_muni6,contains("comp_"))
    }
    x <- x |>
      tidyr::pivot_longer(-1,names_pattern = "(.*)_([^_]*$)",names_to=c("variavel","ano"),values_to="value")|>
      dplyr::mutate(across(variavel,\(x){gsub("_$","",x)}))
    x <- x|>dplyr::rename(codmun=1)
  }
  lista_compostos <- data.table::rbindlist(lapply(calvos,retrieve_comp))

  lista_compostos <- lista_compostos|>
    dplyr::mutate(ano=as.numeric(ano))


  ##MANUAL FIX  - filter out repeated adicionais == governativas
  compind <- data.frame("ind"=inds[!grepl('adicionais',inds)])|>tidyr::separate(1,into=c("var","id"),sep=-1,remove=T)|>
    dplyr::mutate(across(var,\(x){gsub("_$","",x)}))|>
    dplyr::mutate(across(id,as.numeric))|>
    dplyr::group_by(var)|>dplyr::filter(id==max(id))|>
    dplyr::mutate(id=id+1)

  ##manual hack for comp_pnadcs
  compind <- rbind(compind|>dplyr::mutate(var=ifelse(var=="pnadc","pnadc1",var)),
                   data.frame(
                     var=paste0("pnadc",2:7),
                     id=9:14
                   ))


  lista_compostos <- lista_compostos|>
    dplyr::mutate(var=gsub("comp_","",variavel))|>
    dplyr::left_join(compind)|>
    dplyr::transmute(codmun,ano,variavel=
                       dplyr::case_when(grepl("pnadc",variavel)~ paste0(gsub("\\d","",variavel),id),
                                        T ~ paste0(variavel,id)),
                     value)|>
    dplyr::mutate(variavel=gsub("(o\\d)\\d$","\\1",variavel))|>
    dplyr::mutate(variavel=gsub("([^d].)5$","\\1",variavel))|>
    dplyr::distinct()
  ##MANUAL FIX filter outr repeated adicionais == governativas
  lista_ind <- dplyr::bind_rows(lista_ind|>
                                  dplyr::filter(!grepl('adicionais',variavel)),
                                lista_compostos)

  inds <- unique(lista_ind$variavel)

  inds <- data.frame("mdata_id" = 1:length(inds),"orig_name"=inds)

  manual_indnpath <- "div.section.level4 h4"
  nomes_ind <-
    rvest::html_elements(manualhtml,css=manual_indnpath)|>
    rvest::html_text()

  ## Fix objectives numbering on manual - for pnad extracts
  nomes_ind[48:54] <- paste0("8.",1:7,") ",nomes_ind[48:54])

  ## Fix linebreakes
  nomes_ind <- gsub("\\\r(\\\n)*"," ",nomes_ind)


  nomes_ind <- data.frame(
    data_name = nomes_ind
  )    |>tidyr::separate(
    col="data_name",into=c("datagroup_id","orig_name_id","data_name"),
    sep="[.)]",convert=T, extra="merge"
  )

  ##Adjust group_id of objectives according to convention determined above

  nomes_ind[36:47,]$datagroup_id <- rep(9:12,each=3)

  ##adiciona compostos dos objetivos
  objcomp <- data.frame(datagroup_id = 9:12, orig_name_id = 4,
                        data_name = paste("Indicador Composto do Objetivo",1:4))

  objcomp <- rbind(objcomp,data.frame(datagroup_id = 8, orig_name_id = 8:14,
                                      data_name = paste("Indicador Composto/normalizado PNAD Eixo ",1:7)))

  nomes_ind <- rbind(nomes_ind,objcomp)|>
    dplyr::arrange(datagroup_id,orig_name_id)


  orig_names <- data.frame("orig_name"=sort(unique(lista_ind$variavel)))|>
    dplyr::mutate(orig_name_id = as.numeric(substr(orig_name,nchar(orig_name)-grepl("1[01234]$",orig_name),nchar(orig_name))),
                  datagroup_id = dplyr::case_when(
                    grepl("educ",orig_name) ~ 1,
                    grepl("citec",orig_name) ~ 2,
                    grepl("desprod",orig_name) ~ 3,
                    grepl("infra",orig_name) ~ 4,
                    grepl("dessoc",orig_name) ~ 5,
                    grepl("sust",orig_name) ~ 6,
                    grepl("governativas",orig_name) ~ 7,
                    grepl("pnadc",orig_name) ~ 8,
                    grepl("objetivo1",orig_name) ~ 9,
                    grepl("objetivo2",orig_name) ~ 10,
                    grepl("objetivo3",orig_name) ~ 11,
                    grepl("objetivo4",orig_name) ~ 12,
                    T ~ 13
                  ),
                  orig_name_id = ifelse(is.na(orig_name_id) & !grepl("pnad",orig_name),
                                        5,ifelse(
                                          is.na(orig_name_id) ,8,orig_name_id
                                        )),
                  orig_name_id = ifelse(grepl("comp_obj",orig_name),4,orig_name_id))|>
    dplyr::arrange(datagroup_id)|>
    dplyr::mutate(orig_name=gsub("NA$","",orig_name))

  nomes_ind <- nomes_ind|>dplyr::left_join(orig_names)

  #DEPRECATED
  # nomes_ind <- nomes_ind|>
  #   dplyr::bind_rows(
  #     data.frame(
  #       orig_name =
  #         orig_names[!orig_names$orig_name %in% nomes_ind$orig_name,]$orig_name,
  #     datagroup_id =
  #       orig_names[!orig_names$orig_name %in% nomes_ind$orig_name,]$datagroup_id,
  #     datagroup_id =
  #       orig_names[!orig_names$orig_name %in% nomes_ind$orig_name,]$orig_name_id,
  #     data_name =
  #       paste("Indicador composto do ",
  #             gsub("comp_","",orig_names[!orig_names$orig_name %in% nomes_ind$orig_name,]$orig_name))
  #     ))

  nomes_ind <- nomes_ind|>
    dplyr::filter(!grepl("comp_(pnadc|obj)",orig_name))|>
    dplyr::mutate(mdata_id=1:nrow(nomes_ind|>
                                    dplyr::filter(!grepl("comp_(pnadc|obj)",orig_name))),data_desc="")|>
    dplyr::bind_rows(
      nomes_ind|>
        dplyr::filter(grepl("comp_(pnadc|obj)",orig_name))|>
        dplyr::mutate(mdata_id=
                        (1+nrow(nomes_ind)-sum(grepl("comp_(pnadc|obj)",nomes_ind$orig_name))):nrow(nomes_ind),data_desc=""))|>
    dplyr::select(mdata_id,orig_name,data_name,data_desc)

  ## DEPRECATED
  # nomes_ind <- nomes_ind|>
  #   dplyr::mutate(data_name = ifelse(grepl("comp_pnad",orig_name),
  #                                    paste0("Índice Composto/Normalizado do indicador ",gsub("comp_","",orig_name)),
  #                                    data_name))

  DBI::dbAppendTable(con,"mdata",nomes_ind)

  ##mdata_group
  nomes_ind <- nomes_ind|>dplyr::left_join(orig_names)

  mdata_group <- nomes_ind|>dplyr::distinct(mdata_id,datagroup_id)

  DBI::dbAppendTable(con,"mdata_group",mdata_group)
  ##data_values
  lista_ind <- lista_ind|>dplyr::left_join(nomes_ind,by=c("variavel"="orig_name"))

  lista_ind$codmun <- as.numeric(lista_ind$codmun)


  lista_indt <-
    lista_ind |>
    # temporary
    dplyr::mutate(codmun=as.numeric(ifelse(nchar(codmun)==4,paste0(codmun,substr(codmun,1,2),substr(codmun,1,1)),
                                           codmun)))|>
    dplyr::left_join(local|>dplyr::filter(local_id<5571|local_id>=newloce)|>
                       dplyr::mutate(codmun=ifelse(local_id<6941,as.integer(substr(geoloc_id,1,6)),geoloc_id)),
                     by="codmun")|>
    dplyr::mutate(refdate=as.Date(paste0("31/12/",.data[["ano"]]),"%d/%m/%Y"))|>
    dplyr::select(mdata_id,local_id,refdate,value)


  DBI::dbAppendTable(con,"data_values",lista_indt)

  ### DIVERGÊNCIA ENTRE RDS E PLANILHAS - OBJETIVO1_2 -> REPETIR VALORES DE ANOS
  # PRÉVIOS ÍMPARES PARA ANOS PARES

  ##act 42 a 44
  repobj1_2 <- DBI::dbGetQuery(con,"select * from data_values WHERE mdata_id=44")

  repobj1_2$refdate <- repobj1_2$refdate+365+!((lubridate::year(repobj1_2$refdate)+1)%%4)

  DBI::dbAppendTable(con,"data_values",repobj1_2)


  ### META DADOS DE INDICADORES

  instituicoes <- readODS::read_ods("dadostat/indicsprev_v0.ods",sheet=4)

  DBI::dbAppendTable(con,"institution",instituicoes)

  officialerph <- readODS::read_ods("dadostat/indicsprev_v0.ods",sheet=5)

  DBI::dbAppendTable(con,"officialer",officialerph)

  datasources <- readODS::read_ods("dadostat/indicsprev_v0.ods",sheet=3)

  datasources <- datasources|>dplyr::select(datasource_id:datasource_delay)

  datasources <- datasources|>
    dplyr::mutate(across(datasource_lastupdate,lubridate::as_date))

  DBI::dbAppendTable(con,"datasource",datasources)

  ##updated for 2025-03-reorganization
  mdata_exts <-  readODS::read_ods("dadostat/indicsprev_v1.ods",sheet=2)

  mdata_exts <- mdata_exts|>dplyr::select(-orig_name)
  DBI::dbAppendTable(con,"mdata_exts",mdata_exts)

  DBI::dbDisconnect(con)


}
