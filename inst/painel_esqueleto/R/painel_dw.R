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

#' Ranking vazio (localidade nao municipal, sem dado no ano ou sem par)
#' @keywords internal
painel_ranking_vazio <- function() {
  data.frame(rank_uf = NA_integer_, n_uf = NA_integer_,
             rank_br = NA_integer_, n_br = NA_integer_)
}

#' Posicao de uma localidade num indicador e ano: quantos municipios do
#' pais e da propria UF tem valor MAIOR naquele ano
#'
#' Os dois universos sao municipais (largura 7 do geoloc_id): o pais inteiro
#' e a UF de origem, pelo prefixo de 2 digitos do geoloc_id. A posicao conta
#' os valores estritamente maiores, entao empates dividem a mesma posicao; o
#' total (`n_uf`, `n_br`) e quantos municipios tem dado no ano para o
#' indicador. Vale a regra de ano de [painel_valores_ano()] (ultimo refdate
#' de cada localidade dentro do ano, `DISTINCT ON` + faixa `make_date`), e o
#' valor da propria localidade e o do mesmo ano.
#'
#' Contar de cima para baixo assume "maior e melhor", o sentido dos
#' indicadores compostos do catalogo do painel: o resumo nao guarda direcao
#' do indicador (a tabela mdata nao tem essa coluna).
#' @keywords internal
painel_ranking_local <- function(con, mdata_id, local_id, ano) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  local_id <- suppressWarnings(as.integer(local_id))[1]
  ano <- suppressWarnings(as.integer(ano))[1]
  if (is.na(mdata_id) || is.na(local_id) || is.na(ano)) {
    return(painel_ranking_vazio())
  }
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "WITH alvo AS (",
    "  SELECT left(g.geoloc_id::text, 2) AS uf, v.value",
    "  FROM local l",
    "  JOIN geoloc g USING (geoloc_id)",
    "  JOIN data_values v ON v.local_id = l.local_id",
    "  WHERE l.local_id = %d AND length(g.geoloc_id::text) = 7",
    "  AND v.mdata_id = %d",
    "  AND v.refdate >= make_date(%d, 1, 1) AND v.refdate < make_date(%d, 1, 1)",
    "  AND v.value <> 'NaN'::float8",
    "  ORDER BY v.refdate DESC LIMIT 1",
    "), vals AS (",
    "  SELECT DISTINCT ON (v.local_id) left(g.geoloc_id::text, 2) AS uf, v.value",
    "  FROM data_values v",
    "  JOIN local l USING (local_id)",
    "  JOIN geoloc g USING (geoloc_id)",
    "  WHERE v.mdata_id = %d",
    "  AND v.refdate >= make_date(%d, 1, 1) AND v.refdate < make_date(%d, 1, 1)",
    "  AND v.value <> 'NaN'::float8",
    "  AND length(g.geoloc_id::text) = 7",
    "  ORDER BY v.local_id, v.refdate DESC",
    ")",
    "SELECT",
    "  count(*) FILTER (WHERE v.value > a.value) + 1 AS rank_uf,",
    "  count(*) AS n_uf,",
    "  (SELECT count(*) + 1 FROM vals, alvo WHERE vals.value > alvo.value)",
    "  AS rank_br,",
    "  (SELECT count(*) FROM vals, alvo) AS n_br",
    "FROM vals v, alvo a WHERE v.uf = a.uf"),
    local_id, mdata_id, ano, ano + 1L,
    mdata_id, ano, ano + 1L))
  if (!NROW(q)) return(painel_ranking_vazio())
  r <- vapply(q, function(x) suppressWarnings(as.numeric(x))[1], numeric(1))
  # alvo vazio (localidade nao municipal ou sem dado no ano) devolve a
  # agregacao degenerada 1 / 0 / 1 / 0: sem universo, sem posicao
  if (!is.finite(r[["n_br"]]) || r[["n_br"]] < 1) return(painel_ranking_vazio())
  data.frame(rank_uf = r[["rank_uf"]], n_uf = r[["n_uf"]],
             rank_br = r[["rank_br"]], n_br = r[["n_br"]])
}

#' Anotacao de ranking de um cartao do resumo: "5o melhor na UF e 590o BR"
#'
#' `NULL` quando nao ha posicao a mostrar (universo de um so municipio,
#' localidade sem dado no ano ou UF sem par). `n` e o tamanho do universo.
#' @keywords internal
painel_ranking_texto <- function(rank_uf, n_uf, rank_br, n_br) {
  vale <- function(rank, n) {
    isTRUE(is.finite(rank) && is.finite(n) && rank >= 1 && n > 1)
  }
  partes <- character(0)
  if (vale(rank_uf, n_uf)) {
    partes <- c(partes, sprintf("%s\u00ba melhor na UF", painel_num(rank_uf)))
  }
  if (vale(rank_br, n_br)) {
    partes <- c(partes, sprintf("%s\u00ba BR", painel_num(rank_br)))
  }
  if (!length(partes)) return(NULL)
  paste(partes, collapse = " e ")
}

# Rotulos dos niveis territoriais do DW, pela largura do geoloc_id (IBGE):
# 1 grande regiao, 2 UF, 4 regiao geografica intermediaria (2017),
# 5 microrregiao (1990), 6 regiao geografica imediata (2017),
# 7 municipio e 8 mesorregiao (1990; sem dados no DW). A chave "7p" separa
# as regioes de interesse em PNAD Contínua, que usam codigos de 7 digitos
# (mesma largura do codigo IBGE de municipio) e vivem na faixa alta dos
# local_id (6941..7086) — publicadas so pelos indicadores pnadc*/comp_pnadc*
# (verificado 2026-09-22: nenhum indicador municipal publica nelas, e nenhum
# pnadc publica em municipio).
painel_niveis_rotulo <- c(
  "1" = "Região",
  "2" = "Unidade da Federação",
  "4" = "Região geográfica intermediária",
  "5" = "Microrregião",
  "6" = "Região geográfica imediata",
  "7" = "Município",
  "7p" = "Região de interesse PNAD",
  "8" = "Mesorregião")

# Fronteira entre os municipios (local_id 1..5570, Brasilia incluida) e as
# regioes de interesse em PNAD Contínua (local_id >= 5571) dentro da largura
# 7 do geoloc_id — mesma convencao ja adotada por painel_geo_mun()
painel_municipio_limite_id <- 5571L

#' Decodifica a chave de nivel territorial do painel
#'
#' A chave e a largura do geoloc_id como texto ("1".."8") ou "7p" para as
#' regioes de interesse em PNAD Contínua, que dividem a largura 7 com os
#' municipios. Devolve o nivel numerico, se e o subnivel PNAD e o fragmento
#' SQL que seleciona os locais do nivel (pressupoe aliases `l` e `g` no
#' chamador).
#' @keywords internal
painel_nivel_parse <- function(nivel_id) {
  chave <- suppressWarnings(trimws(as.character(nivel_id)[1]))
  if (is.na(chave)) chave <- ""
  pnad <- nzchar(chave) && tolower(chave) == "7p"
  nivel <- if (pnad) 7L else suppressWarnings(as.integer(chave))[1]
  if (!is.na(nivel) && nivel < 1L) nivel <- NA_integer_
  filtro <- if (is.na(nivel)) {
    "1 = 0"
  } else if (pnad) {
    sprintf("length(g.geoloc_id::text) = 7 AND l.local_id >= %d",
            painel_municipio_limite_id)
  } else if (identical(nivel, 7L)) {
    sprintf("length(g.geoloc_id::text) = 7 AND l.local_id < %d",
            painel_municipio_limite_id)
  } else {
    sprintf("length(g.geoloc_id::text) = %d", nivel)
  }
  list(nivel = nivel, pnad = pnad,
       chave = if (is.na(nivel)) NA_character_ else
         if (pnad) "7p" else as.character(nivel),
       filtro = filtro)
}

# Siglas por codigo de UF (para desambiguar nomes de municipios repetidos)
painel_uf_sigla <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP",
  "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES",
  "33" = "RJ", "35" = "SP", "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF")

#' Niveis territoriais disponiveis no DW: apenas os que possuem dados,
#' com quantidade de localidades distintas. A largura 7 aparece em duas
#' linhas — municipios ("7") e regioes de interesse em PNAD Contínua
#' ("7p") — para nao somar 5.570 + 146 numa unica entrada
#' @keywords internal
painel_niveis <- function(con) {
  q <- DBI::dbGetQuery(con, paste(
    "SELECT CASE WHEN length(g.geoloc_id::text) = 7",
    sprintf("AND l.local_id >= %d THEN '7p'", painel_municipio_limite_id),
    "ELSE length(g.geoloc_id::text)::text END AS nivel_id,",
    "count(DISTINCT v.local_id) AS n_locais",
    "FROM data_values v",
    "JOIN local l USING (local_id)",
    "JOIN geoloc g USING (geoloc_id)",
    "GROUP BY 1 ORDER BY 1"))
  q$rotulo <- unname(painel_niveis_rotulo[q$nivel_id])
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

#' Indicador de abertura do painel, escolhido pelo orig_name
#'
#' A variavel de ambiente `aedi_indicador` (ex.: `desprod1`) decide qual
#' indicador das abas Regiao, Mapa e Baixar nasce selecionado; vazia ou
#' desconhecida cai no primeiro do catalogo (`md`, de [painel_mdata()]).
#' Lida a cada chamada, ou seja, a sessao R precisa ser reiniciada para
#' enxergar uma mudanca (o server roda o `updateSelectizeInput` na abertura).
#' @keywords internal
painel_indicador_default <- function(md, padrao = Sys.getenv("aedi_indicador", "")) {
  if (is.null(md) || !NROW(md) || !"mdata_id" %in% names(md)) return(NA_integer_)
  alvo <- trimws(as.character(padrao)[1])
  if (length(alvo) && !is.na(alvo) && nzchar(alvo) && "orig_name" %in% names(md)) {
    i <- match(alvo, trimws(as.character(md$orig_name)))
    if (!is.na(i)) return(as.integer(md$mdata_id[i]))
  }
  as.integer(md$mdata_id[1])
}

#' Localidades de um nivel territorial (pela chave de [painel_nivel_parse()] )
#' que possuem dados, rotuladas por nome (municipios ganham sigla da UF;
#' regioes PNAD ja trazem o contexto no proprio nome)
#' @keywords internal
painel_locais_nivel <- function(con, nivel_id) {
  p <- painel_nivel_parse(nivel_id)
  if (is.na(p$nivel)) return(integer(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT l.local_id, l.local_name,",
    "substring(g.geoloc_id::text, 1, 2) AS uf",
    "FROM data_values v",
    "JOIN local l USING (local_id)",
    "JOIN geoloc g USING (geoloc_id)",
    "WHERE %s",
    "ORDER BY l.local_name, l.local_id"),
    p$filtro))
  rotulo <- if (identical(p$nivel, 7L) && !p$pnad) {
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
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(NULL)
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT v.local_id FROM data_values v",
    "JOIN local l USING (local_id) JOIN geoloc g USING (geoloc_id)",
    "WHERE v.mdata_id = %d AND %s",
    "GROUP BY v.local_id ORDER BY count(*) DESC, v.local_id LIMIT 1"),
    mdata_id, p$filtro))
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
#' limite de feicoes (municipio: 5.6 mil poligonos) voltam vazio e o
#' chamador cai nas UFs como base do desenho — as regioes PNAD (146
#' feicoes) sao desenhadas normalmente
#' @keywords internal
painel_geo_nivel <- function(con, nivel_id, max_feicoes = 700L) {
  p <- painel_nivel_parse(nivel_id)
  if (is.na(p$nivel)) return(painel_geo_vazio())
  n_geo <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT count(*) AS n FROM local l",
    "JOIN geoloc g USING (geoloc_id) WHERE %s"),
    p$filtro))$n
  if (!length(n_geo) || is.na(n_geo) || n_geo > max_feicoes) {
    return(painel_geo_vazio())
  }
  geometria <- if (p$nivel <= 2L) "g.geometry" else
    "ST_SimplifyPreserveTopology(g.geometry, 0.01) AS geometry"
  geo <- sf::st_read(con, query = sprintf(paste(
    "SELECT l.local_id, l.local_name,", geometria,
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE %s",
    "ORDER BY l.local_id"),
    p$filtro), quiet = TRUE)
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

#' Malha municipal de uma UF (prefixo de 2 digitos do geoloc_id),
#' simplificada no SQL (0.01 grau ~ 1 km) — so as bordas dos vizinhos
#' desenhadas no globo ao redor do municipio em destaque
#' @keywords internal
painel_geo_mun_uf <- function(con, uf) {
  uf <- as.character(uf)[1]
  if (is.na(uf) || !grepl("^[0-9]{2}$", uf)) return(painel_geo_vazio())
  geo <- sf::st_read(con, query = sprintf(paste(
    "SELECT l.local_id, l.local_name,",
    "ST_SimplifyPreserveTopology(g.geometry, 0.01) AS geometry",
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE l.local_id < 5571 AND left(g.geoloc_id::text, 2) = '%s'",
    "ORDER BY l.local_id"),
    uf), quiet = TRUE)
  geo$code <- as.character(geo$local_id)
  geo$label <- geo$local_name
  geo[, c("code", "label", "geometry")]
}

#' UFs que contem localidades de um nivel com dados de um indicador —
#' pintura do globo quando a base e a das UFs (nivel municipal)
#' @keywords internal
painel_ufs_com_dados <- function(con, mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(character(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT luf.local_id",
    "FROM data_values v",
    "JOIN local l ON l.local_id = v.local_id",
    "JOIN geoloc g ON g.geoloc_id = l.geoloc_id",
    "JOIN geoloc guf ON length(guf.geoloc_id::text) = 2",
    "AND left(guf.geoloc_id::text, 2) = left(g.geoloc_id::text, 2)",
    "JOIN local luf ON luf.geoloc_id = guf.geoloc_id",
    "WHERE v.mdata_id = %d AND %s"),
    mdata_id, p$filtro))
  as.character(q$local_id)
}

#' Localidade de um nivel com maior cobertura de um indicador DENTRO de
#' uma UF — destino do clique na UF quando o globo usa base estadual
#' @keywords internal
painel_local_top_uf <- function(con, mdata_id, nivel_id, uf_local_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  uf_local_id <- suppressWarnings(as.integer(uf_local_id))[1]
  if (is.na(mdata_id) || is.na(p$nivel) || is.na(uf_local_id)) return(NULL)
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT v.local_id FROM data_values v",
    "JOIN local l ON l.local_id = v.local_id",
    "JOIN geoloc g ON g.geoloc_id = l.geoloc_id",
    "JOIN geoloc guf ON length(guf.geoloc_id::text) = 2",
    "AND left(guf.geoloc_id::text, 2) = left(g.geoloc_id::text, 2)",
    "JOIN local luf ON luf.geoloc_id = guf.geoloc_id",
    "WHERE v.mdata_id = %d AND %s",
    "AND luf.local_id = %d",
    "GROUP BY v.local_id ORDER BY count(*) DESC, v.local_id LIMIT 1"),
    mdata_id, p$filtro, uf_local_id))
  if (nrow(q)) as.integer(q$local_id[1]) else NULL
}

#' Localidades de um nivel territorial (largura do geoloc_id) com dados
#' para um indicador — disponibilidade para o globo de UFs
#' @keywords internal
painel_locais_com_dados <- function(con, mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(character(0))
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT v.local_id",
    "FROM data_values v",
    "JOIN local l USING (local_id) JOIN geoloc g USING (geoloc_id)",
    "WHERE v.mdata_id = %d AND %s"),
    mdata_id, p$filtro))
  as.character(q$local_id)
}

# Familia de indicadores de cada eixo do catalogo: o prefixo de orig_name
# (convencao do pacote: orig_name == nome do script de coleta) identifica a
# familia do indicador, e o "comp_" na frente identifica o composto do eixo.
painel_familias_eixo <- c(
  educ = "Eixo 1", citec = "Eixo 2", desprod = "Eixo 3", infra = "Eixo 4",
  dessoc = "Eixo 5", sust = "Eixo 6", governativas = "Eixo 7")

#' Grupo de um indicador do catalogo (Eixo N, Objetivo N ou Estratos PNAD)
#' pela convencao de orig_name; NA para os indicadores de apoio
#'
#' O catalogo do DW e montado por convencao de nome (educ1..4 + comp_educ no
#' Eixo 1, objetivo2_1..3 + comp_objetivo2 no Objetivo 2, pnadc1..7 +
#' comp_pnadc8..14 nos Estratos PNAD): a tabela mdata_group existe, mas esta
#' populada so parcialmente (verificado 2026-09-22: 7 dos 35 indicadores dos
#' eixos), entao nao serve de fonte da hierarquia. Variacoes de trabalho
#' (`_via_aedi`, `_v0`) ficam fora do catalogo, que e o que o painel publica.
#' @keywords internal
painel_grupo_indicador <- function(orig_name) {
  x <- trimws(as.character(orig_name))
  x <- sub("^comp_", "", x)
  x[grepl("_v[0-9]+$", x)] <- NA_character_
  grupo <- rep(NA_character_, length(x))
  for (familia in names(painel_familias_eixo)) {
    alvo <- grepl(sprintf("^%s[0-9]*$", familia), x)
    grupo[alvo] <- unname(painel_familias_eixo[[familia]])
  }
  alvo <- grepl("^objetivo[0-9]+(_[0-9]+)?$", x)
  grupo[alvo] <- paste0("Objetivo ", sub("^objetivo([0-9]+).*$", "\\1", x[alvo]))
  grupo[grepl("^pnadc[0-9]+$", x)] <- "Estratos PNAD"
  grupo
}

#' Raiz de um grupo do catalogo (Eixos, Objetivos ou o proprio grupo)
#' @keywords internal
painel_grupo_raiz <- function(grupo) {
  ifelse(grepl("^Eixo", grupo), "Eixos",
         ifelse(grepl("^Objetivo", grupo), "Objetivos", grupo))
}

#' Ordem canonica dos grupos no seletor de indicadores do painel
#' @keywords internal
painel_grupos_selecao <- c(paste("Eixo", 1:7), paste("Objetivo", 1:4),
                           "Estratos PNAD")

#' Opcoes do seletor de indicadores do painel, agrupadas por eixo/objetivo
#'
#' Monta a lista aninhada que o `updateSelectizeInput(server = TRUE)`
#' transforma em optgroups, os titulos de grupo nao clicaveis do selectize:
#' "Eixo 1".."Eixo 7", "Objetivo 1".."Objetivo 4", "Estratos PNAD" e, por
#' fim, "Demais indicadores" (series de apoio e variantes de trabalho).
#' Dentro de cada eixo/objetivo o composto abre o grupo e os componentes
#' vem numerados ("Indicador N - Nome (orig_name)"); os Estratos PNAD
#' seguem o numero do estrato (pnadc1..7 e comp_pnadc8..14); os demais
#' mantem a ordem alfabetica do `orig_name` de [painel_mdata()].
#' @keywords internal
painel_opcoes_indicador <- function(md) {
  if (is.null(md) || !NROW(md)) return(list())
  x <- trimws(as.character(md$orig_name))
  grupo <- painel_grupo_indicador(x)
  grupo[is.na(grupo)] <- "Demais indicadores"
  numero <- rep(NA_integer_, length(x))
  numerado <- grepl("[0-9]+$", x)
  numero[numerado] <- as.integer(sub("^.*?([0-9]+)$", "\\1", x[numerado]))
  composto <- grepl("^comp_", x)
  nome <- if ("data_name" %in% names(md)) trimws(as.character(md$data_name)) else
    rep(NA_character_, length(x))
  rotulo <- paste0(ifelse(is.na(nome), x, nome), " (", x, ")")
  rotulo <- ifelse(is.na(numero) | composto, rotulo,
                   paste0("Indicador ", numero, " - ", rotulo))
  ordem <- intersect(painel_grupos_selecao, unique(grupo))
  ordem <- c(ordem,
             setdiff(unique(grupo), c(painel_grupos_selecao,
                                      "Demais indicadores")))
  if ("Demais indicadores" %in% grupo) ordem <- c(ordem, "Demais indicadores")
  respostas <- lapply(ordem, function(g) {
    i <- which(grupo == g)
    if (g == "Demais indicadores") {
      i <- i[order(x[i])]
    } else if (g == "Estratos PNAD") {
      i <- i[order(numero[i], x[i], na.last = TRUE)]
    } else {
      i <- i[order(as.integer(!composto[i]), numero[i], x[i],
                   na.last = TRUE)]
    }
    opcoes <- setNames(as.character(md$mdata_id[i]), rotulo[i])
    if (length(opcoes) == 1L) opcoes <- as.list(opcoes)
    opcoes
  })
  names(respostas) <- ordem
  respostas
}

#' Hierarquia do catalogo de indicadores: raiz (Eixos, Objetivos, Estratos
#' PNAD) > grupo (Eixo 1..7, Objetivo 1..4) > indicador, com a classe do
#' dado (data_class_id) — estrutura do resumo e do accordeon da aba Regiao
#'
#' Os ids e nomes de grupo vem da tabela datagroup do DW; o vinculo de cada
#' indicador ao grupo vem da convencao de orig_name (ver
#' [painel_grupo_indicador()]). Indicador sem grupo conhecido fica de fora.
#' @keywords internal
painel_hierarquia <- function(con) {
  md <- DBI::dbGetQuery(con, paste(
    "SELECT m.mdata_id, m.orig_name, m.data_name, e.data_class_id",
    "FROM mdata m LEFT JOIN mdata_exts e USING (mdata_id)",
    "ORDER BY m.mdata_id"))
  md$datagroup_name <- painel_grupo_indicador(md$orig_name)
  md <- md[!is.na(md$datagroup_name), ]
  grupos <- DBI::dbGetQuery(con,
    "SELECT datagroup_id, datagroup_name FROM datagroup")
  md$raiz_nome <- painel_grupo_raiz(md$datagroup_name)
  md$datagroup_id <- grupos$datagroup_id[match(md$datagroup_name,
                                               grupos$datagroup_name)]
  md$raiz_id <- grupos$datagroup_id[match(md$raiz_nome, grupos$datagroup_name)]
  md <- md[order(md$raiz_nome, md$datagroup_id, md$mdata_id),
           c("raiz_id", "raiz_nome", "datagroup_id", "datagroup_name",
             "mdata_id", "data_class_id", "orig_name", "data_name")]
  rownames(md) <- NULL
  md
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

#' Resumo por grupo do catalogo: um cartao por indicador composto com o
#' valor mais recente na localidade (rotulo, valor e ano) — conteudo do
#' "Resumo da regiao" na aba Regiao
#'
#' @param hierarquia tabela de [painel_hierarquia()]
#' @param compostos tabela de [painel_compostos()]
#' @param valores tabela de [painel_valores_local_todos()]
#' @keywords internal
painel_resumo_grupos <- function(hierarquia, compostos, valores) {
  vazio <- data.frame(grupo = character(0), raiz = character(0),
                      mdata_id = integer(0), rotulo = character(0),
                      valor = numeric(0), refdate = as.Date(character(0)))
  if (!NROW(hierarquia) || !NROW(compostos) || !NROW(valores)) return(vazio)
  h <- hierarquia[hierarquia$mdata_id %in% compostos$mdata_id, ]
  if (!nrow(h)) return(vazio)
  linhas <- lapply(seq_len(nrow(h)), function(i) {
    v <- valores[valores$mdata_id == h$mdata_id[i] &
                   is.finite(valores$value), ]
    if (!nrow(v)) return(NULL)
    ultimo <- which.max(v$refdate)
    nome <- h$data_name[i]
    if (is.na(nome) || !nzchar(nome)) nome <- h$orig_name[i]
    data.frame(grupo = h$datagroup_name[i], raiz = h$raiz_nome[i],
               mdata_id = h$mdata_id[i],
               rotulo = paste0(trimws(nome), " — ", h$datagroup_name[i]),
               valor = v$value[ultimo], refdate = v$refdate[ultimo])
  })
  linhas <- linhas[!vapply(linhas, is.null, logical(1))]
  if (!length(linhas)) return(vazio)
  resumo <- do.call(rbind, linhas)
  rownames(resumo) <- NULL
  resumo
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
  p <- painel_nivel_parse(nivel_id)
  if (is.na(p$nivel)) return(painel_geo_vazio())
  painel_cache_get(
    painel_cache_chave(sprintf("geo_nivel_%s", p$chave)),
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

#' Malha municipal da UF de uma localidade, cacheada POR UF (a malha
#' inteira entra no cache; o municipio em destaque sai dela fora do cache,
#' senao a entrada serviria outra selecao do mesmo estado)
#' @keywords internal
painel_geo_mun_uf_cache <- function(local_id) {
  local_id <- suppressWarnings(as.integer(local_id))[1]
  if (is.na(local_id)) return(painel_geo_vazio())
  uf <- painel_com_con(function(con) DBI::dbGetQuery(con, sprintf(paste(
    "SELECT left(g.geoloc_id::text, 2) AS uf",
    "FROM local l JOIN geoloc g USING (geoloc_id)",
    "WHERE l.local_id = %d"),
    local_id))$uf)
  uf <- as.character(uf)[1]
  if (is.na(uf) || !grepl("^[0-9]{2}$", uf)) return(painel_geo_vazio())
  malha <- painel_cache_get(
    painel_cache_chave(sprintf("geo_mun_uf_%s", uf)),
    painel_cache_ttl[["geo"]],
    function() painel_com_con(function(con) painel_geo_mun_uf(con, uf)))
  malha[malha$code != as.character(local_id), , drop = FALSE]
}

#' UFs com dados num nivel, cacheado
#' @keywords internal
painel_ufs_com_dados_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(character(0))
  painel_cache_get(
    painel_cache_chave(sprintf("ufs_com_dados_%d_%s", mdata_id, p$chave)),
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
  p <- painel_nivel_parse(nivel_id)
  if (is.na(p$nivel)) return(integer(0))
  painel_cache_get(
    painel_cache_chave(sprintf("locais_nivel_%s", p$chave)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con) painel_locais_nivel(con, nivel_id)))
}

#' Localidade com maior cobertura de um indicador num nivel, cacheado
#' @keywords internal
painel_local_top_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(NULL)
  painel_cache_get(
    painel_cache_chave(sprintf("local_top_%d_%s", mdata_id, p$chave)),
    painel_cache_ttl[["catalogo"]],
    function() painel_com_con(function(con)
      painel_local_top(con, mdata_id, nivel_id)))
}

#' Localidades de um nivel com dados para um indicador, cacheado
#' @keywords internal
painel_locais_com_dados_cache <- function(mdata_id, nivel_id) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  p <- painel_nivel_parse(nivel_id)
  if (is.na(mdata_id) || is.na(p$nivel)) return(character(0))
  painel_cache_get(
    painel_cache_chave(sprintf("locais_com_dados_%d_%s", mdata_id, p$chave)),
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

#' Ranking de uma localidade num indicador e ano, cacheado
#' @keywords internal
painel_ranking_local_cache <- function(mdata_id, local_id, ano) {
  mdata_id <- suppressWarnings(as.integer(mdata_id))[1]
  local_id <- suppressWarnings(as.integer(local_id))[1]
  ano <- suppressWarnings(as.integer(ano))[1]
  if (is.na(mdata_id) || is.na(local_id) || is.na(ano)) {
    return(painel_ranking_vazio())
  }
  painel_cache_get(
    painel_cache_chave(sprintf("ranking_%d_%d_%d", mdata_id, local_id, ano)),
    painel_cache_ttl[["valores"]],
    function() painel_com_con(function(con)
      painel_ranking_local(con, mdata_id, local_id, ano)))
}
