# citec4 (9) 2024: depositos de patentes PI/MU por 100 mil habitantes
# (metodo inventor). Fonte: BADEPI v11 do INPI
# (inpidrive tIkguEiqp8cSfeD; series 2000-2024). Baixa/usa cache local.
# Append do refdate 2024-12-31. Padrao A5b.

cacheind <- "coleta/cache/badepi_v11"
if (!dir.exists(cacheind) || !length(list.files(cacheind, pattern = "deposito"))) {
  dir.create(cacheind, recursive = TRUE, showWarnings = FALSE)
  tf <- tempfile(fileext = ".zip")
  h <- curl::new_handle(ssl_verifypeer = 0, followlocation = 1, useragent = "Mozilla/5.0")
  curl::curl_download("https://inpidrive.inpi.gov.br/index.php/s/tIkguEiqp8cSfeD/download", tf, handle = h)
  unzip(tf, exdir = cacheind)
}

dep <- data.table::fread(file.path(cacheind, "badepiv11_ptn_deposito.csv"), encoding = "UTF-8",
                         select = c("ANO", "NO_PEDIDO"))
dep[, NO_PEDIDO := trimws(NO_PEDIDO)]
dep <- dep[substr(NO_PEDIDO, 1, 2) %in% c("PI", "MU", "10", "11", "12", "20", "21", "22")]

inv <- data.table::fread(file.path(cacheind, "badepiv11_ptn_inventor.csv"),
                         select = c("NO_PEDIDO", "CD_IBGE_CIDADE"),
                         colClasses = c(CD_IBGE_CIDADE = "character"), encoding = "Latin-1")
inv[, NO_PEDIDO := trimws(NO_PEDIDO)]
inv <- inv[!(CD_IBGE_CIDADE %in% c("", "  ", "XX", "0000000", NA))]
inv <- unique(inv[, .(NO_PEDIDO, CD_IBGE_CIDADE)])

j <- dep[inv, on = "NO_PEDIDO", nomatch = 0]
serie24 <- j[ANO == 2024, .(n_dep = .N), by = .(local = substr(CD_IBGE_CIDADE, 1, 6))]
serie24[, local := as.numeric(local)]
cat("citec4 2024:", nrow(serie24), "municipios;",
    sum(serie24$n_dep), "depositos PI/MU por inventor\n")
stopifnot(sum(serie24$n_dep) > 10000)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) local, d.value pop
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun' AND d.refdate = DATE '2024-07-01'")
DBI::dbDisconnect(con)

c4 <- merge(serie24, pop, by = "local") |>
  dplyr::transmute(local, valor = 1e5 * n_dep / pop)

AEDi:::gravar_serie_dw("citec4",
  data.frame(local = c4$local, periodo = as.Date("2024-12-31"), valor = c4$valor),
  modo = "append")
