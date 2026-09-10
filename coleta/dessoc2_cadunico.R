# dessoc2 (22): % de pessoas em familias com renda ate 1 salario minimo
# no CadUnico sobre a populacao municipal.
# Fonte: CSV exportado do portal CadUnico/MDS (visdata3, em cache — vai ate
# 12/2024). Processa o que existe no cache. Padrao A5b.

dessoc2 <- data.table::fread(
  "coleta/cache/dessoc2_aedi/visdata3-download-02-05-2025 003336.csv",
  encoding = "Latin-1")

# renomeia por posicao (nomes acentuados variam por encoding)
names(dessoc2)[1:5] <- c("codigo","nome","uf","ref","pessoas_1sm")

cat("referencias:", paste(tail(sort(unique(dessoc2$ref)), 5), collapse=" "),
    "| linhas:", nrow(dessoc2), "\n")

dez <- dessoc2[grepl("^12", ref)] |>
  dplyr::transmute(codigo_ibge = as.numeric(codigo),
                   ano = as.numeric(sub(".*/", "", ref)),
                   pessoas_ate_1sm = as.numeric(pessoas_1sm))

# populacao do DW (julho de cada ano -> refdate 31/12)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) codigo_ibge,
        extract(year from d.refdate)::int ano, d.value populacao
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun'")
DBI::dbDisconnect(con)

serie <- dez |>
  dplyr::inner_join(pop, by = c("codigo_ibge", "ano")) |>
  dplyr::filter(populacao > 0) |>
  dplyr::transmute(local = codigo_ibge,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = pessoas_ate_1sm / populacao)

cat("dessoc2:", nrow(serie), "municipios-ano | anos:",
    paste(range(as.numeric(format(unique(serie$periodo), "%Y"))), collapse="-"), "\n")

AEDi:::gravar_serie_dw("dessoc2",
  data.frame(local = serie$local, periodo = serie$periodo, valor = serie$valor))
