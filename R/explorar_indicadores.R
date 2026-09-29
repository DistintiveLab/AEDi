#' Conexao de leitura para os modulos de exploracao: mesmas variaveis de
#' ambiente do app (user/password/host/dbname, defaults do controle).
#' Retorna NULL quando o banco nao responde (UI mostra aviso)
#' @keywords internal
explorar_con <- function() {
  tryCatch(controle_con(), error = function(e) NULL)
}

#' Dicionario do catalogo: mdata + classe, frequencia, unidades, fonte e
#' observacoes (joins fail-open: DW sem auxiliares segue com colunas NA)
#' @keywords internal
explorar_dicionario <- function(con) {
  d <- DBI::dbGetQuery(con, paste(
    "SELECT m.mdata_id, m.orig_name, m.data_name, m.data_desc,",
    "c.class_name AS classe, f.freq_name AS frequencia,",
    "e.dataunit_num AS unidade_num, e.dataunit_den AS unidade_den,",
    "s.datasource_name AS fonte, e.data_url AS fonte_url,",
    "e.mdata_obs AS observacoes",
    "FROM mdata m",
    "LEFT JOIN mdata_exts e ON e.mdata_id = m.mdata_id",
    "LEFT JOIN data_class c ON c.data_class_id = e.data_class_id",
    "LEFT JOIN data_freq f ON f.data_freq_id = e.data_freq_id",
    "LEFT JOIN datasource s ON s.datasource_id = e.datasource_id",
    "ORDER BY m.orig_name"))
  d$mdata_id <- as.integer(d$mdata_id)
  d
}

#' Nomes das localidades indexados pelo local_id (hover dos graficos)
#' @keywords internal
explorar_nomes_locais <- function(con) {
  loc <- DBI::dbGetQuery(con,
    "SELECT local_id, local_name FROM local ORDER BY local_id")
  setNames(loc$local_name, loc$local_id)
}

#' Ficha do indicador a partir da serie achatada (contrato de
#' `painel_valores_por_ano`): cobertura e dispersao por ano, com as
#' bandeiras da heuristica do roadmap (degenerada dp~0, cauda pesada
#' |CV| > 1,5). Serie vazia devolve NULL
#' @keywords internal
explorar_ficha <- function(serie) {
  if (!NROW(serie) || !all(c("local_id", "ano", "value") %in% names(serie)))
    return(NULL)
  finitos <- serie[is.finite(serie$value), ]
  if (!NROW(finitos)) return(NULL)
  por_ano <- finitos |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(
      n = dplyr::n(),
      min = min(.data$value),
      max = max(.data$value),
      media = mean(.data$value),
      dp = stats::sd(.data$value),
      mediana = stats::median(.data$value),
      iqr = stats::IQR(.data$value),
      .groups = "drop") |>
    dplyr::arrange(.data$ano) |>
    as.data.frame()
  por_ano$cv <- ifelse(por_ano$media != 0, por_ano$dp / por_ano$media,
                       NA_real_)
  ultimo <- por_ano[nrow(por_ano), ]
  list(
    por_ano = por_ano,
    ultimo_ano = ultimo$ano,
    degenerada = isTRUE(ultimo$dp < 1e-12),
    cauda_pesada = !is.na(ultimo$cv) && abs(ultimo$cv) > 1.5,
    anos = range(finitos$ano),
    n_locais = dplyr::n_distinct(finitos$local_id))
}

#' Pares completos de dois indicadores alinhados por (local_id, ano):
#' inner join das series achatadas mantendo apenas pares finitos
#' (remocao de NAs por pares, nao listwise)
#' @keywords internal
explorar_pares <- function(serie_x, serie_y) {
  vazio <- data.frame(local_id = integer(0), ano = integer(0),
                      x = numeric(0), y = numeric(0))
  if (!NROW(serie_x) || !NROW(serie_y)) return(vazio)
  x <- serie_x[, c("local_id", "ano", "value")]
  y <- serie_y[, c("local_id", "ano", "value")]
  names(x)[3] <- "x"
  names(y)[3] <- "y"
  p <- dplyr::inner_join(x, y, by = c("local_id", "ano"))
  p <- p[is.finite(p$x) & is.finite(p$y),
         c("local_id", "ano", "x", "y")]
  if (!NROW(p)) vazio else p
}

#' Correlacoes simples de um par (Pearson e Spearman) com n de pares;
#' series degeneradas (dp~0) ou com menos de 3 pares devolvem NA com
#' aviso explicativo
#' @keywords internal
explorar_cor <- function(pares) {
  res <- data.frame(n = NROW(pares), pearson = NA_real_,
                    spearman = NA_real_, aviso = NA_character_)
  if (res$n < 3) {
    res$aviso <- "menos de 3 pares completos"
    return(res)
  }
  if (stats::sd(pares$x) < 1e-12 || stats::sd(pares$y) < 1e-12) {
    res$aviso <- "série degenerada (dp \u2248 0): correlação indefinida"
    return(res)
  }
  res$pearson <- stats::cor(pares$x, pares$y)
  res$spearman <- stats::cor(pares$x, pares$y, method = "spearman")
  res
}
