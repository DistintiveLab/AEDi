# educ4 (4): Ideb basico (media anos iniciais/finais, rede publica) municipal.
# Serie oficial antiga (pipeline mdr/dadostat) parou em 2021; as bases Ideb do
# DW (ideb_media_basico_redep) ja cobrem as edicoes 2023 e 2025. Este script
# faz o append bienal dessas duas edicoes adotando a derivacao AEDi
# (cor 0.9954 com a serie oficial em 2021). Municipios sem rede publica
# municipal ficam sem valor, igual as bases. Padrao A5b (append).

suppressMessages(library(DBI))

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

edicoes <- as.Date(c("2023-12-31", "2025-12-31"))

novo <- DBI::dbGetQuery(con, paste0(
  "SELECT d.refdate, d.local_id, d.value",
  "  FROM data_values d JOIN mdata m USING (mdata_id)",
  " WHERE m.orig_name = 'ideb_media_basico_redep'",
  "   AND d.refdate IN (",
  paste(sprintf("DATE '%s'", format(edicoes)), collapse = ", "), ")"))
DBI::dbDisconnect(con)

cat("pontos das edicoes novas:", nrow(novo), "\n")
print(table(novo$refdate))
stopifnot(nrow(novo) > 10000)

AEDi:::gravar_serie_dw("educ4",
  data.frame(local = novo$local_id, periodo = novo$refdate,
             valor = novo$value),
  modo = "append")
