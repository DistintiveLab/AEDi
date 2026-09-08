# citec4 (9): depositos de patentes (PI+MU) por 100 mil habitantes.
# Serie 2000-2023 ja calculada e gravada no painelpndr dev (metodologia
# inventor/BADEPI v10 do INPI). Traz a serie pronta do remoto para o DW
# local. Recalculo completo (replace). Padrao A5b.

con_rem <- DBI::dbConnect(RPostgres::Postgres(),
                          user = "usr_cggi_admin",
                          password = Sys.getenv("passwddbdev"),
                          host = "10.214.50.169",
                          dbname = "painelpndr")

serie <- DBI::dbGetQuery(con_rem, "
  SELECT d.refdate, d.local_id, d.value
    FROM data_values d JOIN mdata m USING (mdata_id)
   WHERE m.orig_name = 'citec4'")
DBI::dbDisconnect(con_rem)
cat("citec4 do remoto:", nrow(serie), "pontos;",
    format(min(serie$refdate)), "a", format(max(serie$refdate)), "\n")

AEDi:::gravar_serie_dw("citec4",
  data.frame(local = serie$local_id,
             periodo = serie$refdate,
             valor = serie$value))
