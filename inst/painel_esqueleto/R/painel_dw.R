# Conexao e leituras do DW de indicadores (aedidb) para o Painel.
#
# NAO usa tdbname/userdb/passwddbdev/hostdbdev de proposito: o .Renviron do
# pndr_dashboard aponta essas vars para o DW remoto e elas SOBRESCREVEM as
# variaveis de ambiente da sessao (verificado 2026-09-17, ver
# roadmap_gastos_tributarios.md secao D3). O painel usa o padrao do AEDi
# (user/password/host/dbname) com defaults locais, mudando de alvo via env
# quando necessario.

#' Conexao com o DW de indicadores
#' @keywords internal
painel_con <- function() {
  DBI::dbConnect(RPostgres::Postgres(),
                 user     = Sys.getenv("user", "aedi"),
                 password = Sys.getenv("password", "aEd1#man@gR"),
                 host     = Sys.getenv("host", "127.0.0.1"),
                 dbname   = Sys.getenv("dbname", "aedidb"))
}

#' Executa uma consulta abrindo e fechando conexao propria — usado so na
#' falha de cache, o que poupa os ~4s de handshake nas leituras comuns
#' @keywords internal
painel_com_con <- function(consulta) {
  con <- painel_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  consulta(con)
}

#' Metadados dos indicadores (mdata) com rotulo para selectize
#' @keywords internal
painel_mdata <- function(con) {
  md <- DBI::dbGetQuery(con, paste(
    "SELECT mdata_id, orig_name, data_name, data_desc FROM mdata",
    "ORDER BY orig_name"))
  nome <- ifelse(is.na(md$data_name), md$orig_name, md$data_name)
  md$rotulo <- paste0(nome, " (", md$orig_name, ")")
  md
}

#' Geometrias municipais do DW (local_id < 5571)
#' @keywords internal
painel_geo_mun <- function(con) {
  sf::st_read(con, query = paste(
    "SELECT l.local_id, l.local_name, g.geometry",
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE l.local_id < 5571"), quiet = TRUE)
}

#' Localidades do DW rotuladas por nome + id (ha nomes repetidos entre
#' granularidades: municipio, UF, aglomeracao, regiao, "exceto")
#' @keywords internal
painel_locais <- function(con) {
  loc <- DBI::dbGetQuery(con,
    "SELECT local_id, local_name FROM local ORDER BY local_name, local_id")
  setNames(loc$local_id,
           paste0(loc$local_name, " (id ", loc$local_id, ")"))
}

#' Valores de um indicador (todas as localidades e refdates)
#' @keywords internal
painel_valores <- function(con, mdata_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  if (is.na(mdata_id)) {
    return(data.frame(local_id = integer(0), refdate = as.Date(character(0)),
                      value = numeric(0)))
  }
  DBI::dbGetQuery(con, sprintf(
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id = %d",
    mdata_id))
}

#' Serie de um indicador em UMA localidade — usa o prefixo da chave
#' primaria (mdata_id, local_id) em vez de puxar todas as localidades
#' @keywords internal
painel_valores_local <- function(con, mdata_id, local_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(mdata_id) || is.na(local_id)) {
    return(data.frame(refdate = as.Date(character(0)), value = numeric(0)))
  }
  DBI::dbGetQuery(con, sprintf(paste(
    "SELECT refdate, value FROM data_values",
    "WHERE mdata_id = %d AND local_id = %d ORDER BY refdate"),
    mdata_id, local_id))
}

#' Anos com observacoes de um indicador (para os limites do slider do mapa)
#' @keywords internal
painel_anos <- function(con, mdata_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  if (is.na(mdata_id)) return(data.frame(ano = integer(0)))
  DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT date_part('year', refdate)::int AS ano FROM data_values WHERE mdata_id = %d ORDER BY 1",
    mdata_id))
}

#' Valor do ultimo refdate de cada localidade dentro de um ano: replica no
#' SQL (DISTINCT ON + faixa make_date, indices amigaveis; NULL e NaN caem no
#' <> 'NaN'::float8) o que antes puxava o indicador inteiro e agregava em R
#' @keywords internal
painel_valores_ano <- function(con, mdata_id, ano) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  ano <- suppressWarnings(as.integer(ano))[1]
  if (is.na(mdata_id) || is.na(ano)) {
    return(data.frame(local_id = integer(0), refdate = as.Date(character(0)),
                      value = numeric(0)))
  }
  DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT ON (local_id) local_id, refdate, value",
    "FROM data_values",
    "WHERE mdata_id = %d AND refdate >= make_date(%d, 1, 1)",
    "AND refdate < make_date(%d, 1, 1)",
    "AND value <> 'NaN'::float8",
    "ORDER BY local_id, refdate DESC"),
    mdata_id, ano, ano + 1L))
}

# Rotulos dos niveis territoriais do DW, pela largura do geoloc_id (IBGE):
# 1 grande regiao, 2 UF, 4 regiao geografica intermediaria (2017),
# 5 microrregiao (1990), 6 regiao geografica imediata (2017),
# 7 municipio e 8 mesorregiao (1990; sem dados no DW).
painel_niveis_rotulo <- c(
  "1" = "Região",
  "2" = "Unidade da Federação",
  "4" = "Região geográfica intermediária",
  "5" = "Microrregião",
  "6" = "Região geográfica imediata",
  "7" = "Município",
  "8" = "Mesorregião")

# Siglas por codigo de UF (para desambiguar nomes de municipios repetidos)
painel_uf_sigla <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP",
  "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES",
  "33" = "RJ", "35" = "SP", "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF")

#' Niveis territoriais disponiveis no DW: apenas os que possuem dados,
#' com quantidade de localidades distintas
#' @keywords internal
painel_niveis <- function(con) {
  q <- DBI::dbGetQuery(con, paste(
    "SELECT length(g.geoloc_id::text) AS nivel_id,",
    "count(DISTINCT v.local_id) AS n_locais",
    "FROM data_values v",
    "JOIN local l USING (local_id)",
    "JOIN geoloc g USING (geoloc_id)",
    "GROUP BY 1 ORDER BY 1"))
  q$rotulo <- unname(painel_niveis_rotulo[as.character(q$nivel_id)])
  q[!is.na(q$rotulo), ]
}

#' Nivel territorial de abertura default da aba Regiao: municipal quando
#' disponivel, senao UF
#' @keywords internal
painel_nivel_default <- function(niveis) {
  if (is.null(niveis) || !nrow(niveis)) return("2")
  alvo <- which(grepl("Munic", niveis$rotulo, fixed = TRUE))
  if (length(alvo)) as.character(niveis$nivel_id[alvo[1]]) else "2"
}

#' Localidades de um nivel territorial (pela largura do geoloc_id) que
#' possuem dados, rotuladas por nome (municipios ganham sigla da UF)
#' @keywords internal
painel_locais_nivel <- function(con, nivel_id) {
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(nivel_id)) return(integer(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT l.local_id, l.local_name,",
    "substring(g.geoloc_id::text, 1, 2) AS uf",
    "FROM data_values v",
    "JOIN local l USING (local_id)",
    "JOIN geoloc g USING (geoloc_id)",
    "WHERE length(g.geoloc_id::text) = %d",
    "ORDER BY l.local_name, l.local_id"),
    nivel_id))
  rotulo <- if (nivel_id == 7L) {
    sigla <- unname(painel_uf_sigla[q$uf])
    ifelse(is.na(sigla), q$local_name, paste0(q$local_name, " (", sigla, ")"))
  } else {
    q$local_name
  }
  setNames(as.integer(q$local_id), rotulo)
}

#' Localidade de um nivel com maior cobertura (pontos) de um indicador —
#' selecao default do seletor de localidade
#' @keywords internal
painel_local_top <- function(con, mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(NULL)
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT v.local_id FROM data_values v",
    "JOIN local l USING (local_id) JOIN geoloc g USING (geoloc_id)",
    "WHERE v.mdata_id = %d AND length(g.geoloc_id::text) = %d",
    "GROUP BY v.local_id ORDER BY count(*) DESC, v.local_id LIMIT 1"),
    mdata_id, nivel_id))
  if (nrow(q)) as.integer(q$local_id[1]) else NULL
}

#' sf vazio tipado (colunas code/label/geometry) para as leituras de
#' geometria do globo
#' @keywords internal
painel_geo_vazio <- function() {
  sf::st_sf(code = character(0), label = character(0),
            geometry = sf::st_sfc(crs = 4326))
}

#' Geometrias das Unidades da Federacao do DW (largura 2 do geoloc_id:
#' 26 UFs + DF), com code/label para o globo
#' @keywords internal
painel_geo_uf <- function(con) {
  geo <- sf::st_read(con, query = paste(
    "SELECT l.local_id, l.local_name, g.geometry",
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE length(g.geoloc_id::text) = 2",
    "ORDER BY l.local_id"), quiet = TRUE)
  geo$code <- as.character(geo$local_id)
  geo$label <- geo$local_name
  geo[, c("code", "label", "geometry")]
}

#' Geometrias de um nivel territorial do DW para o globo, simplificadas
#' no SQL (0.01 grau ~ 1 km) para o geojson ficar leve; niveis acima do
#' limite de feicoes (municipio: 5.7 mil poligonos) voltam vazio e o
#' chamador cai nas UFs como base do desenho
#' @keywords internal
painel_geo_nivel <- function(con, nivel_id, max_feicoes = 700L) {
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(nivel_id)) return(painel_geo_vazio())
  n_geo <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM geoloc WHERE length(geoloc_id::text) = %d",
    nivel_id))$n
  if (!length(n_geo) || is.na(n_geo) || n_geo > max_feicoes) {
    return(painel_geo_vazio())
  }
  geometria <- if (nivel_id <= 2L) "g.geometry" else
    "ST_SimplifyPreserveTopology(g.geometry, 0.01) AS geometry"
  geo <- sf::st_read(con, query = sprintf(paste(
    "SELECT l.local_id, l.local_name,", geometria,
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE length(g.geoloc_id::text) = %d",
    "ORDER BY l.local_id"),
    nivel_id), quiet = TRUE)
  geo$code <- as.character(geo$local_id)
  geo$label <- geo$local_name
  geo[, c("code", "label", "geometry")]
}

#' Geometria de uma localidade (destaque do globo quando a base e a das
#' UFs e a selecao nao esta entre as feicoes)
#' @keywords internal
painel_geo_local <- function(con, local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) return(painel_geo_vazio())
  geo <- sf::st_read(con, query = sprintf(paste(
    "SELECT l.local_id, l.local_name, g.geometry",
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE l.local_id = %d"),
    local_id), quiet = TRUE)
  geo$code <- as.character(geo$local_id)
  geo$label <- geo$local_name
  geo[, c("code", "label", "geometry")]
}

#' Geometria da UF que contem uma localidade (contexto do globo: o
#' estado aparece inteiro com a localidade em destaque)
#' @keywords internal
painel_geo_pai_uf <- function(con, local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) return(painel_geo_vazio())
  geo <- sf::st_read(con, query = sprintf(paste(
    "SELECT luf.local_id, luf.local_name, guf.geometry",
    "FROM local lf",
    "JOIN geoloc gf ON gf.geoloc_id = lf.geoloc_id",
    "JOIN geoloc guf ON length(guf.geoloc_id::text) = 2",
    "AND left(guf.geoloc_id::text, 2) = left(gf.geoloc_id::text, 2)",
    "JOIN local luf ON luf.geoloc_id = guf.geoloc_id",
    "WHERE lf.local_id = %d"),
    local_id), quiet = TRUE)
  geo$code <- as.character(geo$local_id)
  geo$label <- geo$local_name
  geo[, c("code", "label", "geometry")]
}

#' UFs que contem localidades de um nivel com dados de um indicador —
#' pintura do globo quando a base e a das UFs (nivel municipal)
#' @keywords internal
painel_ufs_com_dados <- function(con, mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(character(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT luf.local_id",
    "FROM data_values v",
    "JOIN local l ON l.local_id = v.local_id",
    "JOIN geoloc g ON g.geoloc_id = l.geoloc_id",
    "JOIN geoloc guf ON length(guf.geoloc_id::text) = 2",
    "AND left(guf.geoloc_id::text, 2) = left(g.geoloc_id::text, 2)",
    "JOIN local luf ON luf.geoloc_id = guf.geoloc_id",
    "WHERE v.mdata_id = %d AND length(g.geoloc_id::text) = %d"),
    mdata_id, nivel_id))
  as.character(q$local_id)
}

#' Localidade de um nivel com maior cobertura de um indicador DENTRO de
#' uma UF — destino do clique na UF quando o globo usa base estadual
#' @keywords internal
painel_local_top_uf <- function(con, mdata_id, nivel_id, uf_local_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  uf_local_id <- suppressWarnings(as.integer(uf_local_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id) || is.na(uf_local_id)) return(NULL)
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT v.local_id FROM data_values v",
    "JOIN local l ON l.local_id = v.local_id",
    "JOIN geoloc g ON g.geoloc_id = l.geoloc_id",
    "JOIN geoloc guf ON length(guf.geoloc_id::text) = 2",
    "AND left(guf.geoloc_id::text, 2) = left(g.geoloc_id::text, 2)",
    "JOIN local luf ON luf.geoloc_id = guf.geoloc_id",
    "WHERE v.mdata_id = %d AND length(g.geoloc_id::text) = %d",
    "AND luf.local_id = %d",
    "GROUP BY v.local_id ORDER BY count(*) DESC, v.local_id LIMIT 1"),
    mdata_id, nivel_id, uf_local_id))
  if (nrow(q)) as.integer(q$local_id[1]) else NULL
}

#' Localidades de um nivel territorial (largura do geoloc_id) com dados
#' para um indicador — disponibilidade para o globo de UFs
#' @keywords internal
painel_locais_com_dados <- function(con, mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(character(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT v.local_id",
    "FROM data_values v",
    "JOIN local l USING (local_id) JOIN geoloc g USING (geoloc_id)",
    "WHERE v.mdata_id = %d AND length(g.geoloc_id::text) = %d"),
    mdata_id, nivel_id))
  as.character(q$local_id)
}

#' Hierarquia de agrupamentos de indicadores: filhos diretos das raizes
#' "Eixos" e "Objetivos" com os indicadores vinculados (mdata_group) —
#' estrutura do resumo e do accordeon da aba Regiao
#' @keywords internal
painel_hierarquia <- function(con) {
  DBI::dbGetQuery(con, paste(
    "SELECT p.datagroup_parentid AS raiz_id, r.datagroup_name AS raiz_nome,",
    "g.datagroup_id, g.datagroup_name, mg.mdata_id",
    "FROM group_parent p",
    "JOIN datagroup r ON r.datagroup_id = p.datagroup_parentid",
    "AND r.datagroup_name IN ('Eixos', 'Objetivos')",
    "JOIN datagroup g ON g.datagroup_id = p.datagroup_id",
    "LEFT JOIN mdata_group mg ON mg.datagroup_id = g.datagroup_id",
    "ORDER BY 1, 3, mg.mdata_id"))
}

#' Indicadores compostos do catalogo (data_class_id = 4 no mdata_exts)
#' @keywords internal
painel_compostos <- function(con) {
  DBI::dbGetQuery(con, paste(
    "SELECT m.mdata_id, m.orig_name, m.data_name",
    "FROM mdata m",
    "JOIN mdata_exts e ON e.mdata_id = m.mdata_id AND e.data_class_id = 4",
    "ORDER BY m.orig_name"))
}

#' Valores de TODOS os indicadores em uma localidade — alimenta o resumo
#' e os mini-graficos da aba Regiao em uma unica leitura
#' @keywords internal
painel_valores_local_todos <- function(con, local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) {
    return(data.frame(mdata_id = integer(0), refdate = as.Date(character(0)),
                      value = numeric(0)))
  }
  DBI::dbGetQuery(con, sprintf(paste(
    "SELECT mdata_id, refdate, value FROM data_values",
    "WHERE local_id = %d ORDER BY mdata_id, refdate"),
    local_id))
}

#' GeoJSON (string) de um objeto sf para mensagens Shiny ao cliente
#' @keywords internal
painel_geojson <- function(x) {
  arquivo <- tempfile(fileext = ".geojson")
  on.exit(unlink(arquivo), add = TRUE)
  sf::st_write(x, arquivo, quiet = TRUE)
  paste(readLines(arquivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
}

#' Paleta de cores do mapa (port fiel do mypallet do labourvaluesdatapanel):
#' divergente centrada em 0 quando ha negativos, com rampas invertidas
#' conforme o argumento (checkbox "Inverter cores da escala")
#' @keywords internal
painel_paleta <- function(values, invertida = FALSE) {
  values[!is.finite(values)] <- NA_real_
  if (!any(is.finite(values))) {
    return(function(value) rep("transparent", length(value)))
  }
  qtt <- length(values)
  if (isTRUE(invertida)) {
    negative <- grDevices::colorRampPalette(c("#0000FF", "#C8C8FF"))(qtt)
    positive <- grDevices::colorRampPalette(c("#FFC8C8", "#FF0000"))(qtt)
  } else {
    negative <- grDevices::colorRampPalette(c("#FF0000", "#C8C8FF"))(qtt)
    positive <- grDevices::colorRampPalette(c("#C8C8FF", "#0000FF"))(qtt)
  }
  onlypositive <- grDevices::colorRampPalette(c("#FFC8C8", "#FF0000"))(qtt)

  suppressWarnings({
    maximum <- max(values, na.rm = TRUE) * 1.1
    minimum <- min(values, na.rm = TRUE) * 1.1
  })
  if (is.finite(minimum) && is.finite(maximum) &&
      minimum == 0 && maximum == 0) {
    minimum <- -1e-12
    maximum <- 1e-12
  }

  if (minimum < 0 && maximum > 0) {
    if (maximum > abs(minimum)) {
      dominio <- c(-maximum, maximum)
    } else {
      dominio <- c(minimum, -minimum)
    }
    cores <- c(negative, positive)
  } else if (maximum < 0) {
    dominio <- c(minimum, 0)
    cores <- negative
  } else {
    dominio <- c(0, maximum)
    cores <- onlypositive
  }

  if (is.infinite(maximum) || is.infinite(minimum)) {
    function(value) rep("transparent", length(value))
  } else {
    leaflet::colorNumeric(cores, domain = dominio, na.color = "transparent")
  }
}

#' Numeros no padrao pt-BR para rotulos de mapa e legenda
#' @keywords internal
painel_num <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE,
         trim = TRUE, digits = 6)
}

#' Delta de camadas do mapa (port do wlv_map_layer_delta): mantem o par
#' completo cor/tooltip de cada camada alterada, para o cliente fundir por id
#' @keywords internal
painel_delta_camadas <- function(anteriores, camadas) {
  mudou <- vapply(camadas, function(camada)
    !identical(anteriores[[camada$id]], camada), logical(1L))
  camadas[mudou]
}

# Acessores cacheados das leituras do DW (ver painel_cache.R): geometrias
# quase nao mudam (30 dias), catalogo e listas de localidades mudam por ETL
# (7 dias) e valores sao atualizados a cada carga (24h). IDs invalidos
# retornam vazio sem tocar no cache (a chave nunca leva NA).
painel_cache_ttl <- c(geo = 24 * 30, catalogo = 24 * 7, valores = 24)

#' mdata com rotulos, cacheado
#' @keywords internal
painel_mdata_cache <- function() {
  painel_cache_get(painel_cache_chave("mdata"), painel_cache_ttl[["catalogo"]],
                   function() painel_com_con(painel_mdata))
}

#' Niveis territoriais disponiveis, cacheado
#' @keywords internal
painel_niveis_cache <- function() {
  painel_cache_get(painel_cache_chave("niveis"), painel_cache_ttl[["catalogo"]],
                   function() painel_com_con(painel_niveis))
}

#' Hierarquia de agrupamentos (Eixos/Objetivos), cacheado
#' @keywords internal
painel_hierarquia_cache <- function() {
  painel_cache_get(painel_cache_chave("hierarquia"), painel_cache_ttl[["catalogo"]],
                   function() painel_com_con(painel_hierarquia))
}

#' Indicadores compostos do catalogo, cacheado
#' @keywords internal
painel_compostos_cache <- function() {
  painel_cache_get(painel_cache_chave("compostos"), painel_cache_ttl[["catalogo"]],
                   function() painel_com_con(painel_compostos))
}

#' Geometrias municipais, cacheado
#' @keywords internal
painel_geo_mun_cache <- function() {
  painel_cache_get(painel_cache_chave("geo_mun"), painel_cache_ttl[["geo"]],
                   function() painel_com_con(painel_geo_mun))
}

#' Geometrias das UFs, cacheado
#' @keywords internal
painel_geo_uf_cache <- function() {
  painel_cache_get(painel_cache_chave("geo_uf"), painel_cache_ttl[["geo"]],
                   function() painel_com_con(painel_geo_uf))
}

#' Geometrias de um nivel territorial, cacheado
#' @keywords internal
painel_geo_nivel_cache <- function(nivel_id) {
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(nivel_id)) return(painel_geo_vazio())
  painel_cache_get(
    painel_cache_chave(sprintf("geo_nivel_%d", nivel_id)),
    painel_cache_ttl[["geo"]],
    function() painel_com_con(function(con) painel_geo_nivel(con, nivel_id)))
}

#' Geometria de uma localidade, cacheada
#' @keywords internal
painel_geo_local_cache <- function(local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) return(painel_geo_vazio())
  painel_cache_get(
    painel_cache_chave(sprintf("geo_local_%d", local_id)),
    painel_cache_ttl[["geo"]],
    function() painel_com_con(function(con) painel_geo_local(con, local_id)))
}

#' Geometria da UF de uma localidade, cacheada
#' @keywords internal
painel_geo_pai_uf_cache <- function(local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) return(painel_geo_vazio())
  painel_cache_get(
    painel_cache_chave(sprintf("geo_pai_uf_%d", local_id)),
    painel_cache_ttl[["geo"]],
    function() painel_com_con(function(con) painel_geo_pai_uf(con, local_id)))
}

#' UFs com dados num nivel, cacheado
#' @keywords internal
painel_ufs_com_dados_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(character(0))
  painel_cache_get(
    painel_cache_chave(sprintf("ufs_com_dados_%d_%d", mdata_id, nivel_id)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con)
      painel_ufs_com_dados(con, mdata_id, nivel_id)))
}

#' Localidade com maior cobertura numa UF, cacheada
#' @keywords internal
painel_local_top_uf_cache <- function(mdata_id, nivel_id, uf_local_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  uf_local_id <- suppressWarnings(as.integer(uf_local_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id) || is.na(uf_local_id)) return(NULL)
  painel_cache_get(
    painel_cache_chave(sprintf("local_top_uf_%d_%d_%d",
                               mdata_id, nivel_id, uf_local_id)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con)
      painel_local_top_uf(con, mdata_id, nivel_id, uf_local_id)))
}

#' Localidades de um nivel territorial, cacheado
#' @keywords internal
painel_locais_nivel_cache <- function(nivel_id) {
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(nivel_id)) return(integer(0))
  painel_cache_get(
    painel_cache_chave(sprintf("locais_nivel_%d", nivel_id)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con) painel_locais_nivel(con, nivel_id)))
}

#' Localidade com maior cobertura de um indicador num nivel, cacheado
#' @keywords internal
painel_local_top_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(NULL)
  painel_cache_get(
    painel_cache_chave(sprintf("local_top_%d_%d", mdata_id, nivel_id)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con)
      painel_local_top(con, mdata_id, nivel_id)))
}

#' Localidades de um nivel com dados para um indicador, cacheado
#' @keywords internal
painel_locais_com_dados_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  nivel_id <- suppressWarnings(as.integer(nivel_id))[1]
  if (is.na(mdata_id) || is.na(nivel_id)) return(character(0))
  painel_cache_get(
    painel_cache_chave(sprintf("locais_com_dados_%d_%d", mdata_id, nivel_id)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con)
      painel_locais_com_dados(con, mdata_id, nivel_id)))
}

#' Serie de um indicador em uma localidade, cacheado
#' @keywords internal
painel_valores_local_cache <- function(mdata_id, local_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(mdata_id) || is.na(local_id)) {
    return(data.frame(refdate = as.Date(character(0)), value = numeric(0)))
  }
  painel_cache_get(
    painel_cache_chave(sprintf("valores_local_%d_%d", mdata_id, local_id)),
    painel_cache_ttl[["valores"]],
    function() painel_com_con(function(con)
      painel_valores_local(con, mdata_id, local_id)))
}

#' Valores de todos os indicadores em uma localidade, cacheado
#' @keywords internal
painel_valores_local_todos_cache <- function(local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) {
    return(data.frame(mdata_id = integer(0), refdate = as.Date(character(0)),
                      value = numeric(0)))
  }
  painel_cache_get(
    painel_cache_chave(sprintf("valores_local_todos_%d", local_id)),
    painel_cache_ttl[["valores"]],
    function() painel_com_con(function(con)
      painel_valores_local_todos(con, local_id)))
}

#' Anos com observacoes de um indicador, cacheado
#' @keywords internal
painel_anos_cache <- function(mdata_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  if (is.na(mdata_id)) return(data.frame(ano = integer(0)))
  painel_cache_get(
    painel_cache_chave(sprintf("anos_%d", mdata_id)),
    painel_cache_ttl[["valores"]],
    function() painel_com_con(function(con) painel_anos(con, mdata_id)))
}

#' Valores do ultimo refdate por localidade num ano, cacheado
#' @keywords internal
painel_valores_ano_cache <- function(mdata_id, ano) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  ano <- suppressWarnings(as.integer(ano))[1]
  if (is.na(mdata_id) || is.na(ano)) {
    return(data.frame(local_id = integer(0), refdate = as.Date(character(0)),
                      value = numeric(0)))
  }
  painel_cache_get(
    painel_cache_chave(sprintf("valores_ano_%d_%d", mdata_id, ano)),
    painel_cache_ttl[["valores"]],
    function() painel_com_con(function(con)
      painel_valores_ano(con, mdata_id, ano)))
}
