# diagnostico_objetivo2_3_dw (manual, .ignore): conferencia SOMENTE LEITURA no
# banco para confirmar a hipotese de que objetivo2_3 ficou zerado em 2025 por
# falta de datasus_popmun no ano (denominador -Inf/max massificado em 0) e que o
# comp_objetivo2 herda o problema via indseixo('objetivo2').
#
# Uso, no host com acesso ao DW:
#   set -a; . /caminho/para/.Renviron; set +a
#   Rscript coleta/diagnostico_objetivo2_3_dw.R
#
# Nao escreve nada no banco.

con <- tryCatch(
  DBI::dbConnect(RPostgres::Postgres(),
                 user = Sys.getenv("user", "aedi"),
                 password = Sys.getenv("password", "aEd1#man@gR"),
                 host = Sys.getenv("host", "127.0.0.1"),
                 dbname = Sys.getenv("dbname", "aedidb")),
  error = function(e) stop("banco indisponivel (", conditionMessage(e),
                           "); rode este script no host com acesso ao banco do painel"))
on.exit(DBI::dbDisconnect(con), add = TRUE)

resumo <- function(series) {
  DBI::dbGetQuery(con, sprintf(
    "SELECT m.orig_name AS serie, d.refdate, count(*) AS n,
            count(*) FILTER (WHERE d.value = 0) AS n_zero,
            count(*) FILTER (WHERE d.value IS NULL) AS n_na,
            round(min(d.value)::numeric, 6) AS min_v,
            round(max(d.value)::numeric, 6) AS max_v,
            round(avg(d.value)::numeric, 6) AS med_v
       FROM data_values d JOIN mdata m ON m.mdata_id = d.mdata_id
      WHERE m.orig_name IN (%s)
      GROUP BY 1, 2 ORDER BY 1, 2",
    paste0("'", series, "'", collapse = ", ")))
}

series <- c("datasus_popmun", "maxpopestadual", "max_massa_salarial_na_uf",
            "massa_salarial_municipal", "objetivo2_3_via_aedi", "comp_objetivo2")

cat("\n=== 1. serie anual: n linhas, zeros, NA e faixa, por indicador ===\n")
r <- resumo(series)
print(r, row.names = FALSE)

for (s in setdiff(series, unique(r$serie)))
  cat("AVISO: serie ausente do banco (ou sem mdata):", s, "\n")

cat("\n=== 2. anos em que objetivo2_3 ficou 100% zerado ===\n")
o <- r[r$serie == "objetivo2_3_via_aedi", ]
if (!nrow(o)) {
  cat("objetivo2_3_via_aedi nao encontrado no banco.\n")
} else {
  zerados <- o[o$n > 0 & o$n_zero == o$n, ]
  if (!nrow(zerados)) {
    cat("OK: nenhum ano com objetivo2_3 100% zerado.\n")
  } else {
    cat("ZERADOS:", paste(as.character(zerados$refdate), collapse = ", "), "\n")
    pop <- r[r$serie == "datasus_popmun", ]
    for (dt in zerados$refdate) {
      tem_pop <- any(format(pop$refdate, "%Y") == format(dt, "%Y"))
      cat(sprintf("  %s: datasus_popmun no ano? %s\n", as.character(dt),
                  if (tem_pop) "sim (causa nao e a populacao)" else "NAO -> hipotese confirmada"))
    }
  }
}

cat("\n=== 3. comparacao 2025 vs serie historica (mediana dos anos anteriores) ===\n")
for (s in c("massa_salarial_municipal", "objetivo2_3_via_aedi", "comp_objetivo2")) {
  d <- r[r$serie == s, ]
  if (!nrow(d)) next
  d$ano <- format(d$refdate, "%Y")
  base <- d$med_v[d$ano < "2025" & !is.na(d$med_v)]
  novo <- d$med_v[d$ano >= "2025"]
  if (length(base) && length(novo))
    cat(sprintf("%-28s historico med=%.6f | 2025+ med=%.6f\n", s,
                stats::median(base), stats::median(novo, na.rm = TRUE)))
}

cat("\n=== 4. amostra de linhas de objetivo2_3_via_aedi (ultimo refdate) ===\n")
print(DBI::dbGetQuery(con, "
  SELECT d.refdate, d.local_id, round(d.value::numeric, 8) AS obj2_3,
         (SELECT round(v.value::numeric, 4) FROM data_values v
            JOIN mdata mv ON mv.mdata_id = v.mdata_id
           WHERE mv.orig_name = 'massa_salarial_municipal'
             AND v.local_id = d.local_id AND v.refdate = d.refdate) AS massa_mun
    FROM data_values d JOIN mdata m ON m.mdata_id = d.mdata_id
   WHERE m.orig_name = 'objetivo2_3_via_aedi'
     AND d.refdate = (SELECT max(refdate) FROM data_values dd JOIN mdata mm
                        ON mm.mdata_id = dd.mdata_id
                       WHERE mm.orig_name = 'objetivo2_3_via_aedi')
   ORDER BY d.local_id LIMIT 10"), row.names = FALSE)

cat("\n=== 5. refdates existentes por indicador (para conferir 2025-07-01) ===\n")
print(DBI::dbGetQuery(con, "
  SELECT m.orig_name AS serie, min(d.refdate) AS de, max(d.refdate) AS ate,
         count(DISTINCT d.refdate) AS n_refdates
    FROM data_values d JOIN mdata m ON m.mdata_id = d.mdata_id
   WHERE m.orig_name = ANY(ARRAY['datasus_popmun','maxpopestadual',
                                 'objetivo2_3_via_aedi','comp_objetivo2'])
   GROUP BY 1 ORDER BY 1"), row.names = FALSE)

cat("\nDiagnostico concluido (nenhuma escrita realizada).\n")
