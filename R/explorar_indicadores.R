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

#' Limiar do badge visual de "n insuficiente" (nada e escondido: o n
#' aparece sempre junto ao r)
explorar_n_min <- 30L

#' Texto do badge de n de pares: NA quando o n basta; abaixo do minimo
#' matematico (3) a correlacao nem existe
#' @keywords internal
explorar_badge_n <- function(n, minimo = explorar_n_min) {
  if (length(n) != 1L || is.na(n)) return(NA_character_)
  if (n < 3L)
    return(sprintf("n = %d: sem correlação (mínimo matemático de 3 pares)",
                   as.integer(n)))
  if (n < minimo)
    return(sprintf("n insuficiente: %d < %d", as.integer(n),
                   as.integer(minimo)))
  NA_character_
}

#' Reprojeção dos pares completos conforme a leitura de painel: pooled
#' (todas as observações), between (média por localidade) ou within
#' (desvio da média da própria localidade; localidade com um único par
#' sai, desvio indefinido)
#' @keywords internal
explorar_leituras <- function(pares, leitura = "pooled") {
  if (is.null(pares) || !NROW(pares)) return(pares)
  leitura <- if (identical(leitura, "between") || identical(leitura, "within"))
    leitura else "pooled"
  if (identical(leitura, "pooled")) return(pares)
  saida <- do.call(rbind, lapply(split(seq_len(nrow(pares)), pares$local_id),
    function(i) {
      if (identical(leitura, "between"))
        data.frame(local_id = pares$local_id[i[1L]], ano = NA_integer_,
                   x = mean(pares$x[i]), y = mean(pares$y[i]))
      else if (length(i) >= 2L)
        data.frame(local_id = pares$local_id[i[1L]], ano = pares$ano[i],
                   x = pares$x[i] - mean(pares$x[i]),
                   y = pares$y[i] - mean(pares$y[i]))
      else NULL
    }))
  if (is.null(saida))
    return(data.frame(local_id = integer(0), ano = integer(0),
                      x = numeric(0), y = numeric(0)))
  rownames(saida) <- NULL
  saida[order(saida$local_id, saida$ano), , drop = FALSE]
}

#' Empilha séries achatadas (lista nomeada por mdata_id) numa matriz wide
#' por (local_id, ano): full outer join das colunas, nunca listwise — cada
#' par de indicadores usa as observações que ambos têm
#' @keywords internal
explorar_wide <- function(series) {
  if (length(series)) {
    series <- series[names(series) != "" & vapply(
      series, function(s) !is.null(s) && NROW(s) > 0L, logical(1))]
  }
  if (!length(series)) return(NULL)
  colunas <- lapply(seq_along(series), function(i) {
    d <- series[[i]][, c("local_id", "ano", "value")]
    names(d)[3L] <- names(series)[[i]]
    d
  })
  Reduce(function(a, b) merge(a, b, by = c("local_id", "ano"), all = TRUE),
         colunas)
}

#' Colunas do wide com dispersão nula (dp ~ 0): correlação indefinida, a
#' matriz as exclui por default
#' @keywords internal
explorar_degeneradas <- function(wide) {
  if (is.null(wide) || ncol(wide) <= 2L) return(setNames(logical(0),
                                                        character(0)))
  m <- as.matrix(wide[, -(1:2), drop = FALSE])
  stats::setNames(vapply(seq_len(ncol(m)), function(j) {
    v <- m[, j]
    v <- v[is.finite(v)]
    length(v) > 0L && stats::sd(v) < 1e-12
  }, logical(1)), colnames(m))
}

#' Matriz N x N de correlações pairwise (Pearson) e de n de pares de um
#' wide, na leitura pedida: pooled (observações), between (média por
#' localidade, uma linha por localidade) ou within (desvio da localidade;
#' localidade com 1 observação na coluna vira NA, desvio indefinido)
#' @keywords internal
explorar_matriz <- function(wide, leitura = "pooled") {
  vazio <- function() list(
    r = matrix(numeric(0), 0L, 0L,
               dimnames = list(character(0), character(0))),
    n = matrix(numeric(0), 0L, 0L,
               dimnames = list(character(0), character(0))))
  if (is.null(wide) || ncol(wide) <= 2L) return(vazio())
  leitura <- if (identical(leitura, "between") || identical(leitura, "within"))
    leitura else "pooled"
  cols <- setdiff(names(wide), c("local_id", "ano"))
  m <- as.matrix(wide[, cols, drop = FALSE])
  storage.mode(m) <- "double"
  if (identical(leitura, "between")) {
    loc <- as.character(wide$local_id)
    unicos <- sort(unique(loc))
    m <- do.call(cbind, lapply(cols, function(cl) {
      v <- m[, cl]
      ok <- is.finite(v)
      if (!any(ok)) return(rep(NA_real_, length(unicos)))
      soma <- tapply(v[ok], loc[ok], sum)
      cnt <- tapply(v[ok], loc[ok], length)
      (soma / cnt)[unicos]
    }))
    colnames(m) <- cols
  } else if (identical(leitura, "within")) {
    loc <- as.character(wide$local_id)
    for (j in seq_along(cols)) {
      v <- m[, j]
      ok <- is.finite(v)
      soma <- tapply(v[ok], loc[ok], sum)
      cnt <- tapply(v[ok], loc[ok], length)
      med <- soma / cnt
      m[, j] <- ifelse(ok & cnt[match(loc, names(cnt))] >= 2L,
                       v - med[match(loc, names(med))], NA_real_)
    }
  }
  n <- crossprod(!is.na(m))
  r <- if (nrow(m) < 2L)
    matrix(NA_real_, ncol(m), ncol(m),
           dimnames = list(colnames(m), colnames(m)))
  else suppressWarnings(stats::cor(m, use = "pairwise.complete.obs"))
  list(r = r, n = n)
}

#' Ranking de correlação de um alvo contra todos os demais indicadores da
#' matriz: |r| decrescente com sinal e n de pares (NA em r = série
#' degenerada ou sem pares)
#' @keywords internal
explorar_preditores <- function(mat, alvo) {
  vazio <- data.frame(mdata_id = character(0), r = numeric(0),
                      n = numeric(0))
  alvo <- as.character(alvo)[1L]
  if (is.null(mat) || !length(mat$r) || is.na(alvo) ||
      !alvo %in% colnames(mat$r)) return(vazio)
  outros <- setdiff(colnames(mat$r), alvo)
  d <- data.frame(mdata_id = outros,
                  r = unname(mat$r[alvo, outros]),
                  n = unname(mat$n[alvo, outros]))
  d[order(-abs(d$r), -d$n), , drop = FALSE]
}
