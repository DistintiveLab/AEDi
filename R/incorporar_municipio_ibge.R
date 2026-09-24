# Incorporacao de municipios criados apos a carga original do DW
#
# Municipios desmembrados aprovados pelo IBGE depois da carga historica
# (5570 codigos, local_id 1..5570) entram com APPEND apos o bloco PNAD
# (local_id 7088, 7089, ...) — nunca renumerar ids existentes: data_values
# referencia local_id e qualquer renumeracao quebraria o historico. A
# identificacao semantica de municipio em todo o ecossistema passa a ser:
# geoloc_id de 7 digitos E (local_id < 5571 OU local_id > 7087).

#' Baixa e le um JSON da API do IBGE com tratamento de erro
#' @keywords internal
.baixar_json_ibge <- function(url) {
  arq <- tempfile(fileext = ".json")
  ok <- tryCatch({
    utils::download.file(url, arq, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(arq) || file.size(arq) == 0) {
    stop("incorporar_municipio_ibge: falha ao baixar ", url)
  }
  jsonlite::fromJSON(arq, simplifyVector = TRUE)
}

#' Malha municipal (sf) de um codigo IBGE de 7 digitos
#'
#' Fonte 1: API v3 malhas (geojson; municipios recem-criados podem ainda
#' nao constar). Fonte 2: shapefile por UF da malha oficial no geoftp,
#' percorrendo anos do mais recente para tras ate encontrar o codigo.
#' @keywords internal
.baixar_malha_ibge <- function(geoloc7, uf_sigla) {
  url_geo <- paste0(
    "https://servicodados.ibge.gov.br/api/v3/malhas/municipios/", geoloc7,
    "?formato=application/vnd.geo+json&qualidade=intermediaria")
  arq <- tempfile(fileext = ".geojson")
  ok <- tryCatch({
    utils::download.file(url_geo, arq, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (ok && file.exists(arq) && file.size(arq) > 0) {
    g <- tryCatch(sf::st_read(arq, quiet = TRUE), error = function(e) NULL)
    if (!is.null(g) && nrow(g) > 0) {
      message("malha: API v3 malhas")
      return(g)
    }
  }
  for (ano in rev(2022:as.integer(format(Sys.Date(), "%Y")))) {
    url_shp <- sprintf(
      paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/",
             "malhas_territoriais/malhas_municipais/municipio_%d/UFs/%s/",
             "%s_Municipios_%d.zip"), ano, uf_sigla, uf_sigla, ano)
    arq <- tempfile(fileext = ".zip")
    pasta <- tempfile()
    ok <- tryCatch({
      utils::download.file(url_shp, arq, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok || !file.exists(arq) || file.size(arq) < 1000) next
    try(utils::unzip(arq, exdir = pasta), silent = TRUE)
    g <- tryCatch(sf::st_read(pasta, quiet = TRUE), error = function(e) NULL)
    if (is.null(g)) next
    cd <- as.character(g$CD_MUN)
    if (geoloc7 %in% cd) {
      message("malha: geoftp municipio_", ano)
      return(g[cd == geoloc7, ])
    }
  }
  stop("incorporar_municipio_ibge: malha de ", geoloc7,
       " nao encontrada na API v3 nem no geoftp (2022..)")
}

#' datagroup_id pelo nome dentro de uma familia (parent)
#' @keywords internal
.grupo_por_nome <- function(con, nome, parent_id) {
  if (is.null(nome) || is.na(nome) || length(nome) != 1 || !nzchar(nome))
    return(NA_integer_)
  out <- DBI::dbGetQuery(con, paste(
    "SELECT dg.datagroup_id FROM datagroup dg",
    "JOIN group_parent gp ON gp.datagroup_id = dg.datagroup_id",
    "WHERE lower(dg.datagroup_name) = lower($1)",
    "AND gp.datagroup_parentid = $2"),
    params = list(nome, parent_id))$datagroup_id
  if (length(out)) as.integer(out[1]) else NA_integer_
}

#' datagroup_id (familia/parent) pelo padrao ILIKE do nome — so grupos
#' que sao pai de outros grupos em group_parent (evita casar com o filho
#' de participacao, ex. "faz parte da Amazonia Legal")
#' @keywords internal
.familia_por_nome <- function(con, padrao) {
  out <- DBI::dbGetQuery(con, paste(
    "SELECT dg.datagroup_id FROM datagroup dg",
    "WHERE dg.datagroup_name ILIKE $1",
    "AND EXISTS (SELECT 1 FROM group_parent gp",
    "            WHERE gp.datagroup_parentid = dg.datagroup_id)",
    "LIMIT 1"),
    params = list(padrao))$datagroup_id
  if (length(out)) as.integer(out[1]) else NA_integer_
}

#' Incorpora um municipio criado depois da carga original do DW
#'
#' Baixa metadados e geometria da API do IBGE, insere `geoloc` (malha
#' simplificada) e `local` (local_id = max + 1, sempre apos o bloco PNAD,
#' 7088, 7089, ...), vincula os recortes geograficos (regiao imediata e
#' intermediaria resolvidas pelo nome do IBGE; participacoes em Amazonia
#' Legal, faixa de fronteira, semiarido e SUDENE copiadas de um municipio
#' de referencia da mesma regiao imediata) e regenera a matview
#' `recortes_geograficos`. A tipologia PNDR nao e copiada nem resolvida —
#' o IBGE ainda nao publica classificacao para municipios novos; passe o
#' id do grupo via `grupos` quando existir. DATASUS tambem costuma demorar
#' a publicar populacao de municipios novos: indicadores que dependem de
#' popmun ficam com o valor do municipio pendente ate la (scripts devem
#' tolerar NA; ver massa_salarial_municipal.R).
#'
#' @param geoloc_id codigo IBGE do municipio, 6 ou 7 digitos
#' @param municipio_referencia geoloc_id (7d) ou local_id de um municipio
#'   vizinho de onde copiar as participacoes (default: primeiro municipio
#'   da mesma regiao imediata do novo municipio)
#' @param grupos ids de datagroup adicionais a vincular (ex.: tipologia)
#' @param tolerancia dTolerance (graus) da simplificacao da malha;
#'   default 0.01, calibrado com a resolucao das geometrias existentes
#' @param con conexao DBI com o DW; default abre o aedidb local
#' @param regenerar_recortes regenerar a matview recortes_geograficos?
#' @return `local_id` do municipio incorporado, invisivel
#' @export
incorporar_municipio_ibge <- function(geoloc_id, municipio_referencia = NULL,
                                      grupos = NULL, tolerancia = 0.01,
                                      con = NULL,
                                      regenerar_recortes = TRUE) {
  .con_nossa <- is.null(con)
  if (.con_nossa) {
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      user = Sys.getenv("user", "aedi"),
      password = Sys.getenv("password", "aEd1#man@gR"),
      host = Sys.getenv("host", "127.0.0.1"),
      dbname = Sys.getenv("dbname", "aedidb"))
  }
  on.exit(if (.con_nossa) DBI::dbDisconnect(con), add = TRUE)

  geoloc_id <- suppressWarnings(as.numeric(geoloc_id)[1])
  if (is.na(geoloc_id)) stop("geoloc_id invalido")
  muni <- .baixar_json_ibge(paste0(
    "https://servicodados.ibge.gov.br/api/v1/localidades/municipios/",
    trunc(geoloc_id)))
  geoloc7 <- as.numeric(muni$id)
  nome <- as.character(muni$nome)
  rgi <- muni$`regiao-imediata`
  rgint <- if (is.list(rgi)) rgi$`regiao-intermediaria` else NULL
  uf_sigla <- if (is.list(rgint)) rgint$UF$sigla else NA_character_
  rgi_nome <- if (is.list(rgi)) as.character(rgi$nome) else NA_character_
  rgint_nome <- if (is.list(rgint)) as.character(rgint$nome) else NA_character_
  if (is.na(uf_sigla)) stop("resposta do IBGE sem regiao imediata/UF")

  ja <- DBI::dbGetQuery(con,
    "SELECT local_id FROM local WHERE geoloc_id = $1",
    params = list(geoloc7))
  if (nrow(ja)) {
    message("incorporar_municipio_ibge: ", nome, " (", geoloc7,
            ") ja existe no DW com local_id ", ja$local_id[1])
    if (regenerar_recortes) {
      criar_recortes_geograficos(con = con)
      DBI::dbExecute(con, "refresh materialized view named_datavalues;")
    }
    return(invisible(as.integer(ja$local_id[1])))
  }

  malha <- .baixar_malha_ibge(geoloc7, uf_sigla)
  s2_anterior <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(s2_anterior), add = TRUE)
  geometria <- sf::st_transform(sf::st_geometry(malha), 4326)[[1]]
  geometria <- sf::st_simplify(geometria, preserveTopology = TRUE,
                               dTolerance = tolerancia)
  wkt <- sf::st_as_text(geometria)

  # familias de recorte: regiao imediata/intermediaria (resolvidas pelo
  # nome publicado pelo IBGE) e participacoes (copiadas da referencia)
  parent_rgi <- .familia_por_nome(con, "Regi%es Imediatas%")
  parent_rgint <- .familia_por_nome(con, "Regi%es Intermedi%rias%")
  id_rgi <- if (!is.na(parent_rgi))
    .grupo_por_nome(con, rgi_nome, parent_rgi) else NA_integer_
  id_rgint <- if (!is.na(parent_rgint))
    .grupo_por_nome(con, rgint_nome, parent_rgint) else NA_integer_

  ref <- municipio_referencia
  if (is.null(ref) && !is.na(id_rgi)) {
    ref <- DBI::dbGetQuery(con, paste(
      "SELECT lg.local_id FROM local_group lg",
      "WHERE lg.datagroup_id = $1",
      "AND (lg.local_id < 5571 OR lg.local_id > 7087) LIMIT 1"),
      params = list(id_rgi))$local_id
    if (!length(ref)) ref <- NULL
  }
  if (is.null(ref)) {
    stop("informe municipio_referencia (sem regiao imediata resolvivel ",
         "no DW nao ha como copiar as participacoes)")
  }
  ref_num <- as.numeric(ref[1])
  # aceita geoloc_id 7d ou local_id como referencia
  ref_row <- DBI::dbGetQuery(con, paste(
    "SELECT local_id, geoloc_id, local_name FROM local",
    "WHERE local_id = $1 OR (geoloc_id = $1 AND length(geoloc_id::text) = 7)"),
    params = list(ref_num))
  if (!nrow(ref_row) ||
      (ref_row$local_id[1] >= 5571 && ref_row$local_id[1] <= 7087)) {
    stop("municipio_referencia nao encontrado ou nao e municipio")
  }
  ref_id <- as.integer(ref_row$local_id[1])
  message("referencia p/ participacoes: ", ref_row$local_name[1],
          " (local_id ", ref_id, ")")

  # participacoes copiadas da referencia pelo NOME do grupo (fronteira,
  # amazonia legal, semiarido, sudene): o grupo "fora da faixa de
  # fronteira" nao tem familia-pai em group_parent, entao copiar por
  # familia perderia esse recorte
  participacoes <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT lg.datagroup_id FROM local_group lg",
    "JOIN datagroup dg ON dg.datagroup_id = lg.datagroup_id",
    "WHERE lg.local_id = %d",
    "AND dg.datagroup_name ~* '(fronteira|amaz.nia legal|semi.rido|sudene)'"),
    ref_id))
  if (!is.null(grupos)) participacoes <- rbind(
    participacoes, data.frame(datagroup_id = as.integer(grupos)))

  novos_grupos <- unique(as.integer(stats::na.omit(c(
    id_rgi, id_rgint, participacoes$datagroup_id))))
  if (is.na(id_rgi)) warning("regiao imediata '", rgi_nome,
                             "' nao encontrada no DW - nao vinculada")
  if (is.na(id_rgint)) warning("regiao intermediaria '", rgint_nome,
                               "' nao encontrada no DW - nao vinculada")

  DBI::dbBegin(con)
  tryCatch({
    novo_id <- as.integer(
      DBI::dbGetQuery(con, "SELECT max(local_id) m FROM local")$m) + 1L
    DBI::dbExecute(con, paste(
      "INSERT INTO geoloc (geoloc_id, geometry)",
      "VALUES ($1, ST_GeomFromText($2, 4326))"),
      params = list(geoloc7, wkt))
    DBI::dbExecute(con, paste(
      "INSERT INTO local (local_id, geoloc_id, local_name)",
      "VALUES ($1, $2, $3)"), params = list(novo_id, geoloc7, nome))
    if (length(novos_grupos)) {
      DBI::dbExecute(con, sprintf(paste(
        "INSERT INTO local_group (local_id, datagroup_id) VALUES %s"),
        paste(sprintf("(%d, %d)", novo_id, novos_grupos),
              collapse = ", ")))
    }
    DBI::dbCommit(con)
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    stop("incorporar_municipio_ibge falhou: ", conditionMessage(e))
  })

  message("incorporado: ", nome, " (", geoloc7, ") local_id ", novo_id,
          "; grupos: ", paste(novos_grupos, collapse = ", "))

  if (regenerar_recortes) {
    criar_recortes_geograficos(con = con)
    DBI::dbExecute(con, "refresh materialized view named_datavalues;")
    DBI::dbExecute(con, "refresh materialized view geonamed_datavalues;")
  }
  invisible(novo_id)
}

