###CRIAR/ EXTENDER APENAS CONSULTA - recortes geograficos


con <-
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname=Sys.getenv('tdbname'),
      user = Sys.getenv('userdb'),
      password = Sys.getenv('passwddbdev'),
      host=Sys.getenv('hostdbdev')),
    error = function(e) {
      warning("create_extend_geogroup_view: banco indisponivel (",
              conditionMessage(e), ") - con=NULL; ",
              "chame criar_recortes_geograficos() com o banco acessivel")
      NULL
    })
###Criar a VIEW so com municipios

#  consulta
criar_recortes_geograficos <- \() {
numero_municipios <- 5570

consulta_inicial <- paste('(SELECT geoloc.geoloc_id codigo_ibge,',
                          "ST_X(ST_Centroid(geoloc.geometry)) longitude,",
                          "ST_Y(ST_Centroid(geoloc.geometry)) latitude,",
                          "local.local_name município, ",
                          "geoloc.geometry FROM ",
                          "local LEFT JOIN geoloc ON ",
                          "local.geoloc_id = geoloc.geoloc_id WHERE",
                          'local_id < ',numero_municipios+1,") As viewbase")

previos_recortes_nmcol <- c(
  'faixa_de_fronteira',
  'participacao_semiarido',
  'regiao_intermediaria',
  'tipologia',
  'participacao_sudene',
  'regiao_imediata',
  'participacao_amazonia_legal')


adiciona_recorte <- \(novorecorte = 'regiao_imediata',baseq = consulta_inicial,viewbase='recortes_geograficos') {

  ###GET EXISTING PARENT GROUPS AS POSSIBLE BASES
  parent_grupos <- DBI::dbGetQuery(con,"select * from (select DISTINCT(datagroup_parentid) from group_parent) gp LEFT JOIN datagroup ON gp.datagroup_parentid = datagroup.datagroup_id")

  ###FOR COMPATIBILITY PURPOSES WITH PREVIOUS VERSION
  previos_nmcols <- c('eixos_pndr',
                      'faixa_de_fronteira',
                      'participacao_semiarido',
                      'regiao_intermediaria',
                      'tipologia',
                      'participacao_sudene',
                      'objetivos_pndr',
                      'regiao_imediata',
                      'participacao_amazonia_legal')

  if(length(previos_nmcols)==nrow(parent_grupos)){
    parent_grupos$nomecol <- previos_nmcols
  } else {
    parent_grupos$nomecol <- c(previos_nmcols,rep(NA_character_,nrow(parent_grupos)-length(previos_nmcols)))

    parent_grupos <- parent_grupos|>
      mutate(across(nomecol,
                    \(x)ifelse(is.na(x),
                               gsub("_de_","",janitor::make_clean_names(x)),
                               x)))
  }


  ###Identifica novo grupo
  novo_grupo <- (parent_grupos|>dplyr::filter(grepl(novorecorte,nomecol,ignore.case=T)))$datagroup_id

  if (length(novo_grupo) > 1) {
    warning("foi encontrado mais de um grupo-pai para o identificador fornecido. Vai ser utilizado o primeiro.")
  }

  ### check if column already exists
  newcolname <- parent_grupos[parent_grupos$datagroup_id==novo_grupo[1],]$nomecol


  ##Prepare data for JOIN

  recorte <-
    paste("(SELECT geoloc.geoloc_id codigo_ibge,",
          "datagroup.datagroup_name" ,newcolname,
          "FROM local LEFT JOIN geoloc ON",
          "local.geoloc_id = geoloc.geoloc_id",
          "LEFT JOIN local_group ON local.local_Id = local_group.local_id",
          'LEFT JOIN datagroup ON local_group.datagroup_id = datagroup.datagroup_id',
          "LEFT JOIN group_parent ON local_group.datagroup_id = group_parent.datagroup_id",
          "WHERE local.local_id <",numero_municipios+1,
          "AND group_parent.datagroup_parentid = ",
          novo_grupo,") As", newcolname)

  ###JOIN

  paste0(baseq,
         ' LEFT JOIN ',
         recorte,
         ' ON viewbase.codigo_ibge = ',
         newcolname,'.codigo_ibge')


}

centro_query <- Reduce(
  function(q, recorte) {
    adiciona_recorte(novorecorte = recorte, baseq = q)
  },
  previos_recortes_nmcol,
  init = consulta_inicial

)

### Estado e Região autorreferenciado via geoloc_id

uf_regiao <- paste0(" LEFT JOIN (SELECT geoloc_id,local_name uf FROM local WHERE geoloc_id<100 AND geoloc_id>9) ufs ON \n",
                    " SUBSTR(viewbase.codigo_ibge::text,1,2)::INTEGER = ufs.geoloc_id \n",
                    "LEFT JOIN \n",
                    "(SELECT geoloc_id,local_name regiao FROM local WHERE geoloc_id<10) regioes ON \n",
                    " SUBSTR(viewbase.codigo_ibge::text,1,1)::INTEGER = regioes.geoloc_id \n")

##inicio query:

colselect <- paste(
  "SELECT viewbase.codigo_ibge,",
  "viewbase.longitude, viewbase.latitude,",
  "viewbase.município,",
  'uf estado, regiao região,',
  paste0(previos_recortes_nmcol,collapse=", "),
  ', viewbase.geometry ',
  'FROM'
)


DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW recortes_geograficos AS ",paste(colselect,centro_query,uf_regiao)))
DBI::dbExecute(con,
               paste0("CREATE UNIQUE INDEX IF NOT EXISTS codibge_index ON
                      public.recortes_geograficos USING btree
                      (codigo_ibge ASC NULLS LAST) WITH (FILLFACTOR=90)
                      TABLESPACE pg_default;"))


}
