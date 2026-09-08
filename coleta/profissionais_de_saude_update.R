# profissionais_de_saude (mdata 78): CNES/DATASUS profissionais de saude por
# municipio (Jul de cada ano). Acrescenta TODOS os Jul ausentes desde o
# ultimo refdate gravado (append, preservando historico). Padrao A5b.

suppressMessages(library(datasus))

con_dw <- DBI::dbConnect(RPostgres::Postgres(),
                         user = Sys.getenv("user", "aedi"),
                         password = Sys.getenv("password", "aEd1#man@gR"),
                         host = Sys.getenv("host", "127.0.0.1"),
                         dbname = Sys.getenv("dbname", "aedidb"))
ultimo <- DBI::dbGetQuery(con_dw, "
  SELECT max(a.refdate)::date d FROM data_values a
  JOIN mdata m USING (mdata_id) WHERE m.orig_name = 'profissionais_de_saude'")$d
DBI::dbDisconnect(con_dw)
ultimo_ano <- as.numeric(format(ultimo, "%Y"))

faltam <- setdiff((ultimo_ano + 1):as.numeric(format(Sys.Date(), "%Y")),
                  as.numeric(format(Sys.Date(), "%Y")) * 0)  # nada alem do corrente
faltam <- faltam[faltam <= as.numeric(format(Sys.Date(), "%Y"))]
stopifnot(length(faltam) >= 1)

serie <- do.call(rbind, lapply(faltam, \(ano) {
  d <- try(datasus::cnes_prid02br_mun(periodo = sprintf("Jul/%d", ano)), silent = TRUE)
  if (inherits(d, "try-error") || !NROW(d)) return(NULL)
  d <- d[d$Município != "TOTAL", ]
  data.frame(local = as.numeric(sub("^([0-9]+) .*$", "\\1", d$Município)),
             periodo = as.Date(paste0(ano, "-07-01")),
             valor = as.numeric(d$Quantidade))
}))
stopifnot(nrow(serie) > 5000, sum(serie$valor) > 1e6)
cat("anos a acrescentar:", paste(sort(unique(format(serie$periodo, "%Y"))), collapse=" "), "\n")

AEDi:::gravar_serie_dw("profissionais_de_saude", serie, modo = "append")
