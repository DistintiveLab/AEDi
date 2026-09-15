##Atualiza RAIS PARCIAL COM dados natureza juridica 2023



rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("mte_rais"),
                       user="mte_rais",
                       password=Sys.getenv("pwdrais"),
                       host=Sys.getenv("hostraispsql"))



# vincpublicos_2023 <-
#   DBI::dbGetQuery(
#     rais,
#     "select * from rais_vinculo_2023 where natureza_juridica < 2000 and vinculo_ativo_31_12 = 1;")

 nomescols2023 <- names(
   DBI::dbGetQuery(
     rais,
     "select * from rais_vinculo_2023 limit 1;"))


sql_query <- paste0("
BEGIN;

INSERT INTO rais_vinculo_2024 (",
                    paste(nomescols2023[-1],collapse=", "),")
SELECT ",paste(nomescols2023[-1],collapse=", "),"
FROM rais_vinculo_2023
WHERE natureza_juridica < 2000;
COMMIT;
")

dbExecute(rais,sql_query)

nomescols2023estab <- names(
  DBI::dbGetQuery(
    rais,
    "select * from rais_estabelecimento_2023 limit 1;"))



sql_query_estab <- paste0("
BEGIN;

INSERT INTO rais_estabelecimento_2024 (",
                    paste(nomescols2023estab[-1],collapse=", "),")
SELECT ",paste(nomescols2023estab[-1],collapse=", "),"
FROM rais_estabelecimento_2023
WHERE natureza_juridica < 2000;
COMMIT;
")

dbExecute(rais,sql_query_estab)
