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

#' Metadados dos indicadores (mdata) com rotulo para selectize
#' @keywords internal
painel_mdata <- function(con) {
  md <- DBI::dbGetQuery(con, paste(
    "SELECT mdata_id, orig_name, data_name FROM mdata",
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
  DBI::dbGetQuery(con, sprintf(
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id = %d",
    as.integer(mdata_id)))
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

#' Localidades de um nivel territorial (pela largura do geoloc_id) que
#' possuem dados, rotuladas por nome (municipios ganham sigla da UF)
#' @keywords internal
painel_locais_nivel <- function(con, nivel_id) {
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT DISTINCT l.local_id, l.local_name,",
    "substring(g.geoloc_id::text, 1, 2) AS uf",
    "FROM data_values v",
    "JOIN local l USING (local_id)",
    "JOIN geoloc g USING (geoloc_id)",
    "WHERE length(g.geoloc_id::text) = %d",
    "ORDER BY l.local_name, l.local_id"),
    as.integer(nivel_id)))
  rotulo <- if (as.integer(nivel_id) == 7L) {
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
  q <- DBI::dbGetQuery(con, sprintf(paste(
    "SELECT v.local_id FROM data_values v",
    "JOIN local l USING (local_id) JOIN geoloc g USING (geoloc_id)",
    "WHERE v.mdata_id = %d AND length(g.geoloc_id::text) = %d",
    "GROUP BY v.local_id ORDER BY count(*) DESC, v.local_id LIMIT 1"),
    as.integer(mdata_id), as.integer(nivel_id)))
  if (nrow(q)) as.integer(q$local_id[1]) else NULL
}
