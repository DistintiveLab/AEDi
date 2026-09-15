###Filtra e prepara dados base
dbdbase <- DBI::dbGetQuery(con,"SELECT * from geonamed_datavalues WHERE orig_name IN ('emprego_mineracao_municipal','emprego_formal_municipal')")
dbdbase <- dbdbase|>
  dplyr::mutate(data_freq_id=max(data_freq_id))|>

  tidyr::pivot_wider(names_from='orig_name',values_from = 'value', id_cols = c(local_id,refdate),values_fill = 0,unused_fn=dplyr::first)

emp_formun <- readr::read_csv("coleta/cache/emprego_formal_municipal/emprego_formal_municipal.csv")

emprego_mineracao_mun1623 <-
  readr::read_csv("coleta/cache/emprego_mineracao_municipal/emprego_mineracao_municipal1623.csv")

##Puxa da RAIS 2013-2015
rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("mte_rais"),
                       user="mte_rais",
                       password=Sys.getenv("pwdrais"),
                       host=Sys.getenv("hostraispsql"))



pegamin <- \(ano) {
  a <- DBI::dbGetQuery(rais,
                       paste0("SELECT municipio local, COUNT(*) qtd_vinculos_agr FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND cnae_2_0_classe BETWEEN 5000 AND 9999 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

empminmun1315 <- data.table::rbindlist(
  lapply(2013:2015,
         pegamin)
)

empminmun1315 <- empminmun1315|>
  dplyr::transmute(local,qtd_vinculos_agr,periodo=as.Date(paste0(ano,"-12-31")))

emprego_mineracao_mun <-
  rbind(empminmun1315,emprego_mineracao_mun1623)

readr::write_csv(emprego_mineracao_mun,'coleta/cache/emprego_mineracao_municipal/emprego_mineracao_municipal1323.csv')


###Cria indicador
obj4_2_ref <- emp_formun |>
  dplyr::left_join(locgeoloc)|>
  dplyr::mutate(local=trunc(geoloc_id/10))|>
  dplyr::left_join(emprego_mineracao_mun|>
                     dplyr::rename(refdate=periodo,
                                   emprego_mineracao_municipal=qtd_vinculos_agr))|>
  dplyr::rename(setNames(c('emprego_mineracao_municipal','value'), c('a','b'))) |>
  dplyr::mutate(pminr=ifelse(is.na(a),0,a)/b)|>
  dplyr::group_by(refdate)|>
  dplyr::mutate(across(c(a,b),somasna))|>
  dplyr::transmute(local=local_id,valor=a/b,periodo=refdate,prop_minmun=pminr)

#Conferência
# mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                       dbname=Sys.getenv("tdbname"),
#                       user=Sys.getenv("userdb"),
#                       password=Sys.getenv("passwddbdev"),
#                       host=Sys.getenv("hostdbdev"))
objetivo4_2_orig <- dbGetQuery(mdr,
                               "select refdate,local_id,value from data_values a
                               left join mdata b on a.mdata_id = b.mdata_id where
                               orig_name like 'objetivo4_2%'")

obj4_2_compara <- objetivo4_2_orig|>
  dplyr::left_join(obj4_2_ref|>dplyr::rename(local_id=local))|>
  dplyr::transmute(refdate,local_id,obj4_2_base=value,
                   obj4_2_via_aedi=prop_minmun/valor)

cor(obj4_2_compara$obj4_2_base,obj4_2_compara$obj4_2_via_aedi,use='complete.obs')
#0.9971916

summary(obj4_2_compara)


