###CRIAR/ EXTENDER APENAS CONSULTA - recortes geograficos


# Conexao sob demanda: o `con` top-level anterior vazava para o namespace
# em load_all() e era encontrado por guards `if (!exists("con"))` de
# scripts A5b, que passavam a ler o banco remoto (tdbname) sem perceber
# (descoberto na cura do bug de cobertura municipal, 2026-09-22).
.con_recortes <- function() {
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv('tdbname'),
      user = Sys.getenv('userdb'),
      password = Sys.getenv('passwddbdev'),
      host = Sys.getenv('hostdbdev')),
    error = function(e) {
      warning("create_extend_geogroup_view: banco indisponivel (",
              conditionMessage(e), ") - retorne NULL; ",
              "chame criar_recortes_geograficos() com o banco acessivel")
      NULL
    })
}
###Criar a VIEW so com municipios

#  consulta
# `con` opcional: incorporar_municipio_ibge() regenera a matview na mesma
# conexao/transacao; sem conexao, cai no .con_recortes() via env (tdbname)
criar_recortes_geograficos <- \(con = NULL) {
con_propria <- is.null(con)
if (con_propria) con <- .con_recortes()
if (is.null(con)) return(invisible(NULL))
numero_municipios <- 5570
# municipios = bloco historico (1..5570) MAIS os incorporados com append
# apos o bloco PNAD (7088, 7089, ...; ver incorporar_municipio_ibge)
pnad_bloco_fim <- 7087
municipios_filtro <- sprintf("(local.local_id < %d OR local.local_id > %d)",
                             numero_municipios + 1, pnad_bloco_fim)

consulta_inicial <- paste('(SELECT geoloc.geoloc_id codigo_ibge,',
                          "ST_X(ST_Centroid(geoloc.geometry)) longitude,",
                          "ST_Y(ST_Centroid(geoloc.geometry)) latitude,",
                          "local.local_name município, ",
                          "geoloc.geometry FROM ",
                          "local LEFT JOIN geoloc ON ",
                          "local.geoloc_id = geoloc.geoloc_id WHERE",
                          municipios_filtro,") As viewbase")

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
          "WHERE", municipios_filtro,
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


# Matviews que dependem de recortes_geograficos (ex.: geonamed_datavalues):
# salvar definicao + indexes, dropar com CASCADE e recriar sobre a nova
# definicao — sem isso o CREATE falha por dependencia
.salvar_dependentes_recortes <- function(con) {
  # matviews dependem de recortes via regra de rewrite (pg_rewrite),
  # nao diretamente em pg_class; deptype 'n' exclui a regra interna da
  # propria recortes (deptype 'i')
  deps <- DBI::dbGetQuery(con, paste(
    "SELECT DISTINCT v.matviewname, pg_get_viewdef(c.oid, true) AS def",
    "FROM pg_depend d",
    "JOIN pg_rewrite r ON r.oid = d.objid",
    "AND d.classid = 'pg_rewrite'::regclass",
    "JOIN pg_class c ON c.oid = r.ev_class AND c.relkind = 'm'",
    "JOIN pg_matviews v ON v.matviewname = c.relname",
    "AND v.schemaname = c.relnamespace::regnamespace::text",
    "WHERE d.refclassid = 'pg_class'::regclass",
    "AND d.refobjid = 'recortes_geograficos'::regclass",
    "AND d.deptype = 'n'"))
  idx <- if (nrow(deps)) {
    DBI::dbGetQuery(con, sprintf(paste(
      "SELECT indexdef FROM pg_indexes",
      "WHERE schemaname = 'public' AND tablename IN (%s)"),
      paste(sprintf("'%s'", deps$matviewname), collapse = ", ")))
  } else data.frame(indexdef = character(0))
  list(deps = deps, idx = idx)
}

.recriar_dependentes_recortes <- function(con, salvos) {
  for (nm in salvos$deps$matviewname) {
    DBI::dbExecute(con, sprintf(
      "DROP MATERIALIZED VIEW IF EXISTS %s CASCADE", nm))
    DBI::dbExecute(con, sprintf(
      "CREATE MATERIALIZED VIEW %s AS %s", nm,
      salvos$deps$def[salvos$deps$matviewname == nm]))
  }
  for (d in salvos$idx$indexdef) DBI::dbExecute(con, d)
}

dependentes_salvos <- .salvar_dependentes_recortes(con)
DBI::dbExecute(con,"DROP MATERIALIZED VIEW IF EXISTS recortes_geograficos CASCADE")
DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW recortes_geograficos AS ",paste(colselect,centro_query,uf_regiao)))
DBI::dbExecute(con,
               paste0("CREATE UNIQUE INDEX IF NOT EXISTS codibge_index ON
                      public.recortes_geograficos USING btree
                      (codigo_ibge ASC NULLS LAST) WITH (FILLFACTOR=90)
                      TABLESPACE pg_default;"))
.recriar_dependentes_recortes(con, dependentes_salvos)

# geonamed_datavalues (definicao canonica de dbprepare.R) e central para os
# scripts de coleta: se nao foi capturada como dependente (ex.: regen apos
# queda que a derrubou), garante a existencia
DBI::dbExecute(con, paste(
  "CREATE MATERIALIZED VIEW IF NOT EXISTS geonamed_datavalues AS",
  "SELECT named_datavalues.*, recortes_geograficos.*",
  "FROM named_datavalues",
  "LEFT JOIN local ON named_datavalues.local_id = local.local_id",
  "LEFT JOIN recortes_geograficos",
  "ON local.geoloc_id = recortes_geograficos.codigo_ibge"))

if (con_propria) DBI::dbDisconnect(con)


}
