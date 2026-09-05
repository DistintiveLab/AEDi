# Gravacao de series no DW aedidb a partir dos scripts de coleta
# (padrao A5b, replicado de rais_vinculos_s38.R em 2026-09-03)
#
# Encapsula: conexao, metadados do mdata existente, mapeamento IBGE
# 6 digitos (RAIS) -> geoloc_id 7 digitos (DW), e recarga completa com
# db_datawrite(replace=TRUE) — transacional, mdata_id estavel.
#
#serie: data.frame com colunas `local` (IBGE 6 ou 7 digitos, ou local_id
#          do DW), `periodo` (Date) e `valor` (numeric).

# Anos disponiveis no mte_rais (tabelas rais_vinculo_YYYY)
#' @keywords internal
anos_rais <- function(con) {
  sort(as.numeric(gsub("\\D", "", grep("^rais_vinculo_[0-9]+$",
    DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables
                     WHERE table_schema='public' AND table_name ~ '^rais_vinculo_[0-9]+$'")$table_name,
    value = TRUE))))
}

#' Grava a serie de um indicador existente no DW
#'
#' @param orig_name nome do indicador em mdata (precisa existir; indicadores
#'   novos seguem pelo modulo/builder)
#' @param serie data.frame com `local`, `periodo`, `valor`
#' @param modo `"replace"` recalcula a serie completa (default, modelo A);
#'   `"append"` acrescenta/atualiza APENAS os refdates presentes em `serie`,
#'   preservando o historico (novo ponto de fonte que publica atrasado)
#' @return invisivel(TRUE) se gravou; FALSE se mdata ausente (com message)
#' @keywords internal
gravar_serie_dw <- function(orig_name, serie, modo = c("replace", "append")) {
  modo <- match.arg(modo)
  stopifnot(all(c("local", "periodo", "valor") %in% names(serie)))
  con_aedi <- DBI::dbConnect(
    RPostgres::Postgres(),
    user = Sys.getenv("user", "aedi"),
    password = Sys.getenv("password", "aEd1#man@gR"),
    host = Sys.getenv("host", "127.0.0.1"),
    dbname = Sys.getenv("dbname", "aedidb"))

  md <- DBI::dbGetQuery(con_aedi,
    "SELECT * FROM mdata WHERE orig_name = $1", params = list(orig_name))
  if (nrow(md) != 1) {
    DBI::dbDisconnect(con_aedi)
    message("gravar_serie_dw: mdata '", orig_name, "' ausente - nao gravado")
    return(invisible(FALSE))
  }
  exts <- DBI::dbGetQuery(con_aedi,
    "SELECT * FROM mdata_exts WHERE mdata_id = $1", params = list(md$mdata_id))
  metadf <- list(md[, c("orig_name", "data_name", "data_desc")],
                 exts[, setdiff(names(exts), "mdata_id")])

  locais <- DBI::dbGetQuery(con_aedi,
    "SELECT local_id, geoloc_id FROM local")

  # lookup triplo: geoloc_id completo (7d), prefixo IBGE da RAIS (6d) ou o
  # proprio local_id (agregados: Brasil, UF, regioes...)
  lookup <- c(
    setNames(locais$local_id, as.character(locais$geoloc_id)),
    setNames(locais$local_id[locais$local_id < 6000],
             as.numeric(substr(as.character(locais$geoloc_id[locais$local_id < 6000]), 1, 6))),
    setNames(locais$local_id, as.character(locais$local_id)))
  lookup <- lookup[!duplicated(names(lookup))]

  lid <- lookup[as.character(as.numeric(serie$local))]
  serie <- serie[!is.na(lid), ]
  lid <- lid[!is.na(lid)]
  datadf <- data.frame(local = as.numeric(lid),
                       periodo = serie$periodo,
                       valor = serie$valor)
  # defesa contra fan-out de joins: 1 ponto por (local, periodo)
  datadf <- datadf[!duplicated(datadf[, c("local", "periodo")]), ]

  if (modo == "append") {
    # remove apenas os refdates trazidos pela serie e reinsere (upsert por
    # refdate), preservando o historico anterior
    periodos <- sort(unique(datadf$periodo))
    DBI::dbBegin(con_aedi)
    tryCatch({
      in_dates <- paste(sprintf("DATE '%s'", format(periodos)), collapse = ", ")
      DBI::dbExecute(con_aedi, sprintf(
        "DELETE FROM data_values WHERE mdata_id = %d AND refdate IN (%s)",
        md$mdata_id, in_dates))
      DBI::dbAppendTable(con_aedi, "data_values",
        data.frame(mdata_id = md$mdata_id,
                   local_id = datadf$local,
                   refdate = datadf$periodo,
                   value = datadf$valor))
      DBI::dbExecute(con_aedi,
        "UPDATE mdata_timetable SET last_refdate = GREATEST(last_refdate, $2),
            last_update = current_date WHERE mdata_id = $1",
        params = list(md$mdata_id, max(periodos)))
      DBI::dbExecute(con_aedi, "refresh materialized view named_datavalues;")
      DBI::dbExecute(con_aedi, "refresh materialized view geonamed_datavalues;")
      DBI::dbCommit(con_aedi)
    }, error = function(e) {
      try(DBI::dbRollback(con_aedi), silent = TRUE)
      stop("gravar_serie_dw(append) falhou: ", conditionMessage(e))
    })
    DBI::dbDisconnect(con_aedi)
    message("DW atualizado (append): ", orig_name, " refdates ",
            paste(format(periodos), collapse = ", "), " (", nrow(datadf), " pontos)")
    return(invisible(TRUE))
  }

  AEDi:::db_datawrite(metadf, datadf, construct = NULL,
                      sanitize = FALSE, replace = TRUE)
  DBI::dbDisconnect(con_aedi)
  message("DW atualizado: ", orig_name, " ate ", max(serie$periodo),
          " (", nrow(serie), " pontos)")
  invisible(TRUE)
}
