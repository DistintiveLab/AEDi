
rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("mte_rais"),
                       user="mte_rais",
                       password=Sys.getenv("pwdrais"),
                       host=Sys.getenv("hostraispsql"))



vinculos_s38 <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr,SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND trunc(cnae_2_0_classe/1000) != 38 GROUP BY municipio")
  )
  a$ano <- ano
  a
}


# anos dinamicos: todas as tabelas rais_vinculo_YYYY existentes no mte_rais
anos_rais <- sort(as.numeric(gsub("\\D", "", grep("^rais_vinculo_[0-9]+$",
  DBI::dbGetQuery(rais, "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='public' AND table_name ~ '^rais_vinculo_[0-9]+$'")$table_name,
  value = TRUE))))

vinculos_rem_s38 <-
  data.table::rbindlist(
    lapply(anos_rais, vinculos_s38)
  )

readr::write_csv(vinculos_rem_s38,'coleta/cache/rais_vinculos_s38/rais_vinculos_s38.csv')

# gravacao no DW (modelo A: recalculo completo, replace=TRUE) ----
# serie municipal de vinculos ativos fora do CNAE 38, refdate 31/12 de cada ano
serie_dw <- vinculos_rem_s38 |>
  dplyr::transmute(local = local, periodo = as.Date(paste0(ano, "-12-31")),
                   valor = qtd_vinculos_agr)

con_aedi <- DBI::dbConnect(RPostgres::Postgres(),
                           user = Sys.getenv("user", "aedi"),
                           password = Sys.getenv("password", "aEd1#man@gR"),
                           host = Sys.getenv("host", "127.0.0.1"),
                           dbname = Sys.getenv("dbname", "aedidb"))
# (scripts sourceados por sys.source nao podem usar on.exit: ele dispara ao
#  fim de CADA expressao e desconectaria a conexao antes do uso)
locais <- DBI::dbGetQuery(con_aedi,
  "SELECT local_id, geoloc_id FROM local WHERE local_id < 6000") |>
  dplyr::mutate(geoloc6 = as.numeric(substr(as.character(geoloc_id), 1, 6)))
serie_dw <- serie_dw |>
  dplyr::inner_join(locais, by = c("local" = "geoloc6")) |>
  # com sanitize=FALSE o db_datawrite espera a coluna de id como "local"
  dplyr::transmute(local = local_id, periodo, valor)

md <- DBI::dbGetQuery(con_aedi,
  "SELECT * FROM mdata WHERE orig_name = 'rais_vinculos_s38'")
if (nrow(md) == 1) {
  exts <- DBI::dbGetQuery(con_aedi,
    "SELECT * FROM mdata_exts WHERE mdata_id = $1", params = list(md$mdata_id))
  metadf <- list(md[, c("orig_name", "data_name", "data_desc")],
                 exts[, setdiff(names(exts), "mdata_id")])
  AEDi:::db_datawrite(metadf, serie_dw, construct = NULL,
                      sanitize = FALSE, replace = TRUE)
  cat("DW atualizado: rais_vinculos_s38 ate", max(serie_dw$periodo), "\n")
} else {
  cat("mdata rais_vinculos_s38 ausente - serie nao gravada no DW\n")
}
DBI::dbDisconnect(con_aedi)
DBI::dbDisconnect(rais)

vinculos_rem_s38 <-
  vinculos_rem_s38|>
  dplyr::mutate(remuneracao_media= massa_salarial/qtd_vinculos_agr)|>
  dplyr::group_by(ano)|>
  dplyr::mutate(mediana_nacional=median(remuneracao_media,na.rm=T))|>
  dplyr::ungroup()|>
  dplyr::mutate(objetivo1_1_aedi = remuneracao_media-mediana_nacional)

#Conferência
#
# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))
#
# locgeoloc <- dbGetQuery(mdr,'select * from local')
#
# objetivo1_1_orig <- dbGetQuery(mdr,
#                                "select refdate,local_id,value from data_values a
#                                left join mdata b on a.mdata_id = b.mdata_id where
#                                orig_name like 'objetivo1_1%'")
#
#
# obj1_1_comp <-
#   objetivo1_1_orig|>
#   dplyr::left_join(locgeoloc|>dplyr::transmute(local_id,local=trunc(geoloc_id/10)))|>
#   dplyr::left_join(
#     vinculos_rem_s38|>dplyr::transmute(refdate=as.Date(paste0(ano,"-12-31")),
#                                                 local,objetivo1_1_aedi))
#
# cor(obj1_1_comp$value,obj1_1_comp$objetivo1_1_aedi,use='complete.obs')
#0.999971
