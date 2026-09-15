##Script para atualização/sobrescrita de indicadores em painelmdr/ aedidb


lista_indicadores_ok <-
  c(
    "objetivo_1_1",
    "objetivo_1_2",
    "objetivo_2_1",
    "objetivo_1_2",
    "objetivo_2_3",
    "objetivo_3_1",
    "objetivo_3_2",
    "objetivo_3_3",
    "objetivo_4_1",
    "objetivo_4_2",
    "objetivo_4_3"
  )


###upsert rais_vlr_rem_dez_s38

recupmdata_id <- \(serie,db=con){
  DBI::dbGetQuery(db,
               paste0("select mdata_id from mdata where orig_name = '",
                      serie,"'")
  )$mdata_id
}





rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("mte_rais"),
                       user="mte_rais",
                       password=Sys.getenv("pwdrais"),
                       host=Sys.getenv("hostraispsql"))

mdr <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       dbname=Sys.getenv("tdbname"),
                       user=Sys.getenv("userdb"),
                       password=Sys.getenv("passwddbdev"),
                       host=Sys.getenv("hostdbdev"))


con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      dbname=Sys.getenv("dbname"),
                      user=Sys.getenv("user"),
                      password=Sys.getenv("password"),
                      host=Sys.getenv("host"))



rais_rem_dez_s38 <- \(ano) {
  a <-
    DBI::dbGetQuery(
      rais,
      paste0("SELECT municipio local, COUNT(*) qtd_vinculos,
             SUM(vl_remun_dezembro_nom) massa_salarial FROM rais_vinculo_",
                              ano," WHERE vinculo_ativo_31_12 = 1  AND
             trunc(cnae_2_0_classe/1000) != 38 GROUP BY municipio")
  )
  a$ano <- ano
  a
}

massa_salarial_vinculos <- data.table:rbindlist(
  lapply(
    2013:2023,
    rais_rem_dez_s38
  )
)

massa_salarial_vinculos[,salmedio:=massa_salarial/qtd_vinculos]

massa_salarial_vinculos[,medianasal:=median(salmedio),by=ano]

massa_salarial_vinculos[,objetivo1_1_aedi:=salmedio-medianasal]

massa_salarial_vinculos[,refdate:=as.Date(paste0(ano,"-12-31"))]

obj1_comp2 <- obj1_1_orig|>
  dplyr::left_join(locgeoloc|>dplyr::mutate(local=trunc(geoloc_id/10)))|>
  dplyr::left_join(massa_salarial_vinculos[,.(refdate,local,objetivo1_1_aedi)])

cor(obj1_comp2$obj1_1_base,obj1_comp2$objetivo1_1_aedi,use="complete.obs")
#0.999971

remdezs38 <- recupmdata_id('rais_vlr_rem_dez_s38')


rds38 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massa_salarial_vinculos,on = c("local"="local")]

rds38[,mdata_id:=remdezs38]
rds38 <- rds38[,.(refdate,mdata_id,local_id,value=massa_salarial)]

rds38 <- rds38[!is.na(local_id),]

upsertdb <- dbx::dbxConnect(
  adapter="postgres",
  host = Sys.getenv('host'),
  dbname=Sys.getenv('dbname'),
  user=Sys.getenv('user'),
  password=Sys.getenv('password'),
)

dbx::dbxUpsert(upsertdb,'data_values',
               rds38,where_cols=c('mdata_id','refdate','local_id'))

vs38id <- recupmdata_id('rais_vinculos_s38')
vs38 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massa_salarial_vinculos,on = c("local"="local")]
vs38[,mdata_id:=vs38id]
vs38 <- vs38[,.(refdate,mdata_id,local_id,value=qtd_vinculos)]
vs38 <- vs38[!is.na(local_id),]
dbx::dbxUpsert(upsertdb,'data_values',
               vs38,where_cols=c('mdata_id','refdate','local_id'))

rmeds38id <- recupmdata_id('rais_media_remdez_s38')
rmeds38 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massa_salarial_vinculos,on = c("local"="local")]
rmeds38[,mdata_id:=rmeds38id]
rmeds38 <- rmeds38[,.(refdate,mdata_id,local_id,value=salmedio)]
rmeds38 <- rmeds38[!is.na(local_id),]
dbx::dbxUpsert(upsertdb,'data_values',
               rmeds38,where_cols=c('mdata_id','refdate','local_id'))


medianas38id <- recupmdata_id('rais_mediana_remmed_s38')
medianas38 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massa_salarial_vinculos,on = c("local"="local")]
medianas38[,mdata_id:=medianas38id]
medianas38 <- medianas38[,.(refdate,mdata_id,local_id,value=medianasal)]
medianas38 <- medianas38[!is.na(local_id),]
dbx::dbxUpsert(upsertdb,'data_values',
               medianas38,where_cols=c('mdata_id','refdate','local_id'))

obj1_1id <- recupmdata_id('objetivo1_1')

obj1_1 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massa_salarial_vinculos,on = c("local"="local")]
obj1_1[,mdata_id:=obj1_1id]
obj1_1 <- obj1_1[,.(refdate,mdata_id,local_id,value=objetivo1_1_aedi)]
obj1_1 <- obj1_1[!is.na(local_id),]

dbx::dbxUpsert(upsertdb,'data_values',
               obj1_1,where_cols=c('mdata_id','refdate','local_id'))


#datasus_popmun aedidb
datasus_popmunid <- recupmdata_id('datasus_popmun')

datasus_popmun <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][popmunicipal,on=c("local"="local")]
datasus_popmun <- datasus_popmun[!is.na(local_id),]
datasus_popmun[,mdata_id:=datasus_popmunid]
datasus_popmun <- datasus_popmun[,.(mdata_id,refdate,local_id,value=populacao)]

dbx::dbxUpsert(upsertdb,'data_values',
               datasus_popmun,where_cols=c('mdata_id','refdate','local_id'))

#maxpopestadual aedidb
maxpopestadualid <- recupmdata_id('maxpopestadual')
maxpopestadual <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][popmunicipal,on=c("local"="local")]
maxpopestadual <- maxpopestadual[!is.na(local_id),]
maxpopestadual[,uf:=trunc(local/10000)]
maxpopestadual[,value:=max(populacao,na.rm=TRUE),by= .(uf,refdate)]
maxpopestadual[,mdata_id:=maxpopestadualid]
maxpopestadual <- maxpopestadual[,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(upsertdb,'data_values',
               maxpopestadual,where_cols=c('mdata_id','refdate','local_id'))

#Complementa 2013 2014 massa_salarial_municipal
massa_salarial_municipalid <- recupmdata_id('massa_salarial_municipal')

massa_salarial_municipal <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][massalmun1314[,`:=` (
  mdata_id=massa_salarial_municipalid,
  value=massa_salarial,
  refdate=as.Date(paste0(ano,"-12-31")))],on=c("local"="local")]
massa_salarial_municipal <- massa_salarial_municipal[,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(upsertdb,'data_values',
               massa_salarial_municipal,where_cols=c('mdata_id','refdate','local_id'))


#pndr
pndrupsert <- dbx::dbxConnect(
  adapter="postgres",
  host = Sys.getenv('hostdbdev'),
  dbname=Sys.getenv('tdbname'),
  user=Sys.getenv('userdb'),
  password=Sys.getenv('passwddbdev'),
)


#Objetivo 1_1 no db pndr
obj1_1idpndr <- recupmdata_id('objetivo1_1',mdr)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj1_1,where_cols=c('mdata_id','refdate','local_id'))


#Objetivo 1_2 no db pndr
obj1_2idpndr <- recupmdata_id('objetivo1_2',mdr)

obj1_2r <- data.table::setDT(objetivo1_2_via_aedi)[,mdata_id:=obj1_2idpndr]

obj1_2r <- obj1_2r[,.(mdata_id,local_id,refdate,value=objetivo1_2_via_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj1_2r,where_cols=c('mdata_id','refdate','local_id'))

#Objetivo 1_3 no db pndr
obj1_3idpndr <- recupmdata_id('objetivo1_3',mdr)

obj1_3 <- obj1_3|>dplyr::transmute(refdate,mdata_id=obj1_3idpndr,local_id,value=objet13med)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj1_3,where_cols=c('mdata_id','refdate','local_id'))

#Objetivo 1_composto no db pndr
obj1_cidpndr <- recupmdata_id('comp_objetivo1',mdr)

obj1_c <- data.table::fread("coleta/cache/objetivo1_composto_aedi/2013_2023_objetivo1_composto_aedi.csv")

obj1_c <- obj1_c|>dplyr::transmute(refdate,mdata_id=obj1_cidpndr,local_id,value=normalizado)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj1_c,where_cols=c('mdata_id','refdate','local_id'))



#Objetivo 2_1  no db pndr
objetivo2_1_aedi <- readRDS('coleta/cache/objetivo2_1_aedi/objetivo2_1_aedi.rds')

obj2_1idpndr <- recupmdata_id('objetivo2_1',mdr)

obj2_1 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][objetivo2_1_aedi,on = c("local"="codmun")]
obj2_1 <- obj2_1[!is.na(local_id),]
obj2_1 <- obj2_1[,`:=` (refdate=as.Date(paste0(ano,"-12-31")),
                        mdata_id=obj2_1idpndr)]
obj2_1 <- obj2_1[,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj2_1,where_cols=c('mdata_id','refdate','local_id'))

#Objetivo 2_2 no db pndr
obj2_2idpndr <- recupmdata_id('objetivo2_2',mdr)
obj2_2 <- data.table::setDT(objetivo2_2_via_aedi)[,mdata_id:=obj2_2idpndr]
obj2_2 <- obj2_2[,.(mdata_id,refdate,local_id,value=objetivo2_2_via_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj2_2,where_cols=c('mdata_id','refdate','local_id'))


#objetivo2_3 no db pndr
obj2_3idpndr <- recupmdata_id('objetivo2_3',mdr)
obj2_3 <- readRDS("coleta/cache/objetivo2_3_via_aedi/obj2_3_massa_salarial_e_indicador.rds")

obj2_3 <- data.table::setDT(obj2_3)[,`:=` (
  mdata_id=obj2_3idpndr,
  refdate=as.Date(paste0(ano,"-12-31")))]

obj2_3 <- obj2_3[,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj2_3,where_cols=c('mdata_id','refdate','local_id'))


#Objetivo 2_composto no db pndr
obj2_cidpndr <- recupmdata_id('comp_objetivo2',mdr)

obj2_c <- data.table::fread("coleta/cache/objetivo2_composto_aedi/obj2_c_aedi.csv")

obj2_c <- obj2_c|>dplyr::transmute(refdate,mdata_id=obj2_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj2_c,where_cols=c('mdata_id','refdate','local_id'))



#objetivo3_1 no db pndr
obj3_1idpndr <- recupmdata_id('objetivo3_1',mdr)
obj3_1 <- data.table::setDT(objetivo3_1_aedi)
obj3_1[,mdata_id:=obj3_1idpndr]
dbx::dbxUpsert(pndrupsert,'data_values',
               obj3_1,where_cols=c('mdata_id','refdate','local_id'))

#objetivo3_2 no db pndr
obj3_2idpndr <- recupmdata_id('objetivo3_2',mdr)
obj3_2 <- data.table::setDT(objetivo3_2_via_aedi)[,mdata_id:=obj3_2idpndr]
obj3_2 <- obj3_2[,.(mdata_id,refdate,local_id,value=objetivo3_2_via_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj3_2,where_cols=c('mdata_id','refdate','local_id'))

#objetivo3_3 no db pndr
obj3_3idpndr <- recupmdata_id('objetivo3_3',mdr)
obj3_3 <- data.table::setDT(objetivo3_3_via_aedi)[,mdata_id:=obj3_3idpndr]
obj3_3 <- obj3_3[,.(mdata_id,refdate,local_id,value=objetivo3_3_via_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj3_3,where_cols=c('mdata_id','refdate','local_id'))

#Objetivo 3_composto no db pndr
obj3_cidpndr <- recupmdata_id('comp_objetivo3',mdr)

obj3_c <- data.table::fread("coleta/cache/objetivo3_composto_aedi/obj3_c_aedi.csv")

obj3_c <- obj3_c|>dplyr::transmute(refdate,mdata_id=obj3_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj3_c,where_cols=c('mdata_id','refdate','local_id'))



#objetivo4_1 no db pndr
obj4_1idpndr <- recupmdata_id('objetivo4_1',mdr)
obj4_1 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][data.table::setDT(empregoformalmun)[,`:=` (
  mdata_id=obj4_1idpndr,
  refdate=as.Date(paste0(ano,"-12-31")),
  value=obj4_1_aedi)
  ],on=c('local'='local')][,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj4_1,where_cols=c('mdata_id','refdate','local_id'))

#objetivo4_2 no db pndr
obj4_2_idpndr <- recupmdata_id('objetivo4_2',mdr)
obj4_2 <-emprego_mineracao_municipal|>
  dplyr::transmute(refdate=as.Date(paste0(ano,'-12-31')),
                   local,
                   obj4_2_aedi)|>
  dplyr::left_join(locgeoloc)|>
  dplyr::transmute(mdata_id=obj4_2_idpndr,
                                      value=obj4_2_aedi,
                                      local_id,
                                      refdate)


dbx::dbxUpsert(pndrupsert,'data_values',
               obj4_2,where_cols=c('mdata_id','refdate','local_id'))

#objetivo4_3 no db pndr
obj4_3_idpndr <- recupmdata_id('objetivo4_3',mdr)
obj4_3 <-data.table::setDT( readRDS("coleta/cache/objetivo4_3_via_aedi/objetivo4_3_aedi.rds"))
obj4_3[,mdata_id:=obj4_3_idpndr]
obj4_3[,refdate:=as.Date(paste0(ano,"-12-31"))]
obj4_3 <- data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][
  obj4_3,on=c('local'='codmun')
]

obj4_3 <- obj4_3[,.(mdata_id,refdate,local_id,value)]
obj4_3 <- obj4_3[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               obj4_3,where_cols=c('mdata_id','refdate','local_id'))

#Objetivo 4_composto no db pndr
obj4_cidpndr <- recupmdata_id('comp_objetivo4',mdr)

obj4_c <- data.table::fread("coleta/cache/objetivo4_composto_aedi/obj4_c_aedi.csv")

obj4_c <- obj4_c|>dplyr::transmute(refdate,mdata_id=obj4_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               obj4_c,where_cols=c('mdata_id','refdate','local_id'))



#eixo1_1 ice / eci CEDEPLAR
e1_1idpndr <- recupmdata_id('desprod1',db = mdr)
e1_1 <- readxl::read_xlsx("coleta/cache/dataviva_atualizacao/dataviva_atualizacao.xlsx")|>
  dplyr::rename(codmun=`ID IBGE Municípios`)|>
  dplyr::mutate(codmun=ifelse(codmun==4314530,4314548,codmun))|>
  tidyr::pivot_longer(-codmun,names_to="refdate",values_to="value")|>
  dplyr::mutate(refdate=as.Date(paste0(refdate,"-12-31")),mdata_id=e1_1idpndr)|>
  dplyr::left_join(locgeoloc,by=c("codmun"="geoloc_id"))|>
  dplyr::select(refdate,mdata_id,local_id,value)



dbx::dbxUpsert(pndrupsert,'data_values',
               e1_1,where_cols=c('mdata_id','refdate','local_id'))



#eixo1_2 industria stotal
e1_2idpndr <- recupmdata_id('desprod2')
e1_2 <- data.table::fread(
  'coleta/cache/desprod2_aedi/desprod2_aedi.csv'
)
e1_2[,mdata_id:=e1_2idpndr]
e1_2 <- e1_2[,.(mdata_id,refdate,local_id,value=100*desprod2_aedi)]

e1_2 <- e1_2[refdate>'2010-01-01',]

dbx::dbxUpsert(pndrupsert,'data_values',
               e1_2,where_cols=c('mdata_id','refdate','local_id'))

#eixo1_3 salario medio formal

e1_3idpndr <- recupmdata_id('desprod3')

e1_3 <- data.table::fread('coleta/cache/desprod3_aedi/desprod_aedi1324parcial.csv')

e1_3[,mdata_id:=e1_3idpndr]

e1_3 <- e1_3[,.(mdata_id,refdate,local_id,value=desprod3_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e1_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo1_4

e1_4idpndr <- recupmdata_id('desprod4')

e1_4 <- data.table::fread('coleta/cache/desprod4_aedi/desprod4_aedi1323.csv')

e1_4[,mdata_id:=e1_4idpndr]

e1_4 <- e1_4[,.(mdata_id,refdate,local_id,value=desprod4_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e1_4,where_cols=c('mdata_id','refdate','local_id'))


#e1 desprod _composto no db pndr
e1_cidpndr <- recupmdata_id('comp_desprod',mdr)

e1_c <- data.table::fread("coleta/cache/desprod_composto_aedi/desprod_c_aedi.csv")

e1_c <- e1_c|>dplyr::transmute(refdate,mdata_id=e1_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               e1_c,where_cols=c('mdata_id','refdate','local_id'))







#eixo2_1

e2_1idpndr <- recupmdata_id('citec1')

e2_1 <- data.table::fread('coleta/cache/citec1_aedi/citec1_aedi.csv')

e2_1[,mdata_id:=e2_1idpndr]

e2_1 <- e2_1[,.(mdata_id,refdate,local_id,value=citec1_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e2_1,where_cols=c('mdata_id','refdate','local_id'))

#eixo2_2
e2_2idpndr <- recupmdata_id('citec2')
e2_2 <- data.table::fread("coleta/cache/citec2_aedi/citec2_aedi.csv")

e2_2[,mdata_id:=e2_2idpndr]

e2_2 <- data.table::setDT(locgeoloc)[
  ,local:=trunc(geoloc_id/10)][
    ,.(local,local_id)][
      e2_2,on = c('local'='local')][
        ,.(mdata_id,refdate=periodo,local_id,value=citec2)]



dbx::dbxUpsert(pndrupsert,'data_values',
               e2_2,where_cols=c('mdata_id','refdate','local_id'))

#eixo2_3
e2_3idpndr <- recupmdata_id('citec3')
e2_3 <- data.table::fread("coleta/cache/citec3_aedi/citec3_aedi.csv")

e2_3[,mdata_id:=e2_3idpndr]
e2_3 <- e2_3[,.(mdata_id,refdate,local_id,value=citec3)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e2_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo2_4 <- pendente!Patentes - pendencia revista encontrado formato de numero badepi v10 e 'avisos' dados v0 pndr
e2_4idpndr <- recupmdata_id('citec4',db = mdr)
e2_4 <- data.table::fread("coleta/cache/citec4_aedi/citec4_aedi.csv")

e2_4[,mdata_id:=e2_4idpndr]
e2_4 <- e2_4[,.(mdata_id,refdate,local_id,value=citec4)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e2_4,where_cols=c('mdata_id','refdate','local_id'))


#e2 citec _composto no db pndr
e2_cidpndr <- recupmdata_id('comp_citec',mdr)

e2_c <- data.table::fread("coleta/cache/citec_composto_aedi/citec_c_aedi.csv")

e2_c <- e2_c|>dplyr::transmute(refdate,mdata_id=e2_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               e2_c,where_cols=c('mdata_id','refdate','local_id'))





#eixo3_1 pndr
e3_1idpndr <- recupmdata_id('educ1')
e3_1 <- data.table::fread("coleta/cache/educ1_esgoto/educ1_aedi.csv")

e3_1[,mdata_id:=e3_1idpndr]
e3_1 <-
  data.table::setDT(locgeoloc)[,codmun:=trunc(geoloc_id/10)][e3_1[,refdate:=as.Date(paste0(ano,'-12-31'))], on = c('codmun'='codmun')]

e3_1 <- e3_1[,.(mdata_id,refdate,local_id,value)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e3_1,where_cols=c('mdata_id','refdate','local_id'))

#eixo3_2 pndr
e3_2idpndr <- recupmdata_id('educ2')
e3_2 <- data.table::fread("coleta/cache/educ2_aedi/educ2_aedi.csv")

e3_2[,mdata_id:=e3_2idpndr]

e3_2 <- e3_2[,.(mdata_id,refdate,local_id,value=educ2)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e3_2,where_cols=c('mdata_id','refdate','local_id'))

#eixo3_3 pndr PENDENTE EPCT - confirmada troca para afd  - cor 1
e3_3idpndr <- recupmdata_id('educ3',db=mdr)
e3_3 <- readRDS("coleta/cache/afd/afd_aedi_2013_2024.rds")
e3_3 <- e3_3|>
  dplyr::left_join(locgeoloc)|>
  dplyr::transmute(refdate,mdata_id=e3_3idpndr,local_id,value=afd_aedi)




dbx::dbxUpsert(pndrupsert,'data_values',
               e3_3,where_cols=c('mdata_id','refdate','local_id'))


#eixo3_4 pndr
e3_4idpndr <- recupmdata_id('educ4')
e3_4 <- data.table::fread("coleta/cache/educ4_aedi/educ4_aedi.csv")

e3_4[,mdata_id:=e3_4idpndr]

e3_4 <- e3_4[,.(mdata_id,refdate,local_id,value=educ4_aedi)]

dbx::dbxUpsert(pndrupsert,'data_values',
               e3_4,where_cols=c('mdata_id','refdate','local_id'))


#e3 educ _composto no db pndr
e3_cidpndr <- recupmdata_id('comp_educ',mdr)

e3_c <- data.table::fread("coleta/cache/educ_composto_aedi/educ_c_aedi.csv")

e3_c <- e3_c|>dplyr::transmute(refdate,mdata_id=e3_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               e3_c,where_cols=c('mdata_id','refdate','local_id'))





#eixo4_1 pndr PENDENTE SNIS - confirmado quebra de serie com anotação
e4_1idpndr <- recupmdata_id('infra1',db=mdr)
e4_1 <- data.table::fread("coleta/cache/infra1_aedi/infra1_aedi.csv")

e4_1[,mdata_id:=e4_1idpndr]


e4_1 <-
  data.table::setDT(locgeoloc)[,codmun:=trunc(geoloc_id/10)][e4_1,on=c('geoloc_id'='geoloc_id')][
    ,.(mdata_id,refdate,local_id,value=infra1)]


e4_1 <- e4_1[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e4_1,where_cols=c('mdata_id','refdate','local_id'))


#eixo4_2 pndr PENDENTE acessos internet
e4_2idpndr <- recupmdata_id('infra2',db=mdr)
e4_2 <- data.table::fread("coleta/cache/infra2_aedi/infra2_aedi.csv")

e4_2[,mdata_id:=e4_2idpndr]


e4_2 <-
  data.table::setDT(locgeoloc)[,codmun:=trunc(geoloc_id/10)][e4_2,on=c('geoloc_id'='geoloc_id')][
    ,.(mdata_id,refdate,local_id,value=infra2_aedi)]


e4_2 <- e4_2[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e4_2,where_cols=c('mdata_id','refdate','local_id'))


#eixo4_3

e4_3idpndr <- recupmdata_id('infra3')
e4_3 <- data.table::fread("coleta/cache/infra3_aedi2/infra3_aedi_2013_2024.csv")

e4_3[,mdata_id:=e4_3idpndr]


e4_3 <-
  data.table::setDT(locgeoloc)[,codmun:=trunc(geoloc_id/10)][e4_3,on=c('local'='local')][
    ,.(mdata_id,refdate,local_id,value=infra3_aedi)]


e4_3 <- e4_3[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e4_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo4_4
e4_4idpndr <- recupmdata_id('infra4')
e4_4 <- data.table::fread("coleta/cache/infra4_aedi_s/infra4_aedi_2015_2024.csv")

e4_4[,mdata_id:=e4_4idpndr]

e4_4[,refdate:=as.Date(paste0(ano,'-12-31'))]

e4_4 <-
  data.table::setDT(locgeoloc)[e4_4,on=c('geoloc_id'='codmun')][
    ,.(mdata_id,refdate,local_id,value=infra4_aedi)]


e4_4 <- e4_4[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e4_4,where_cols=c('mdata_id','refdate','local_id'))


#e4 infra _composto no db pndr
e4_cidpndr <- recupmdata_id('comp_infra',mdr)

e4_c <- data.table::fread("coleta/cache/infra_composto_aedi/infra_c_aedi.csv")

e4_c <- e4_c|>dplyr::transmute(refdate,mdata_id=e4_cidpndr,local_id,value)

##CONFIRMAR COM EQUIPE
 dbx::dbxUpsert(pndrupsert,'data_values',
                e4_c,where_cols=c('mdata_id','refdate','local_id'))





#eixo5_1 pndr
e5_1idpndr <- recupmdata_id('dessoc1')
e5_1 <- data.table::fread('coleta/cache/dessoc1_aedi/dessoc1_aedi1324.csv')

e5_1[,mdata_id:=e5_1idpndr]
e5_1[,refdate:=as.Date(paste0(ano,'-12-31'))]

e5_1 <-
  data.table::setDT(locgeoloc)[,cd_mun:=trunc(geoloc_id/10)][e5_1,on=c('cd_mun'='cd_mun')][
    ,.(mdata_id,refdate,local_id,value=dessoc1)]

e5_1 <- e5_1[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e5_1,where_cols=c('mdata_id','refdate','local_id'))

#eixo5_2 pndr
e5_2idpndr <- recupmdata_id('dessoc2')
e5_2 <- data.table::fread('coleta/cache/dessoc2_aedi/dessoc2_aedi.csv')

e5_2[,mdata_id:=e5_2idpndr]

e5_2 <-
  data.table::setDT(locgeoloc)[,codigo_ibge:=trunc(geoloc_id/10)][e5_2,on=c('codigo_ibge'='codigo_ibge')][
    ,.(mdata_id,refdate,local_id,value=dessoc2_aedi)]

e5_2 <- e5_2[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e5_2,where_cols=c('mdata_id','refdate','local_id'))

#eixo5_3 pndr
e5_3idpndr <- recupmdata_id('dessoc3')
e5_3 <- data.table::fread('coleta/cache/dessoc3_aedi/dessoc_3_aedi_comp1323.csv')

e5_3[,mdata_id:=e5_3idpndr]

e5_3 <-
  data.table::setDT(locgeoloc)[e5_3,on=c('geoloc_id'='codigo_municipio')][
    ,.(mdata_id,refdate=ano,local_id,value=valor)]

e5_3 <- e5_3[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e5_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo5_4 pndr
e5_4idpndr <- recupmdata_id('dessoc4')
e5_4 <- data.table::fread('coleta/cache/dessoc4_aedi/dessoc4_aedi_1323.csv')

e5_4[,mdata_id:=e5_4idpndr]

#e5_4[massa_salarial_1==0,massa_salarial_1:=massa_salarial_2]

#e5_4[,dessoc4:=(massa_salarial_2/qtd_vinculos_agr_2)/(massa_salarial_1/qtd_vinculos_agr_1)]

e5_4 <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][e5_4,on=c('local'='geoloc_id')][
    ,.(mdata_id,refdate,local_id,value=dessoc4_aedi)]

e5_4 <- e5_4[!is.na(local_id),]

dbx::dbxUpsert(pndrupsert,'data_values',
               e5_4,where_cols=c('mdata_id','refdate','local_id'))


#e5 dessoc _composto no db pndr
e5_cidpndr <- recupmdata_id('comp_dessoc',mdr)

# ##CONFIRMAR COM EQUIPE

e5_c <- data.table::fread("coleta/cache/dessoc_composto_aedi/dessoc_c_aedi24p.csv")

e5_c <- e5_c|>dplyr::transmute(refdate,mdata_id=e5_cidpndr,local_id,value=normalizado)

##CONFIRMAR COM EQUIPE
dbx::dbxUpsert(pndrupsert,'data_values',
               e5_c,where_cols=c('mdata_id','refdate','local_id'))




#eixo6_1 pndr
e6_1idpndr <- recupmdata_id('governativas1')
e6_1 <- data.table::fread('coleta/cache/governativas1_aedi/governativas_aedi_1323.csv')

e6_1[,mdata_id:=e6_1idpndr]

e6_1 <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][e6_1,on=c('local'='geoloc_id')][
    ,.(mdata_id,refdate,local_id,value=gov1_aedi)]



dbx::dbxUpsert(pndrupsert,'data_values',
               e6_1,where_cols=c('mdata_id','refdate','local_id'))

#eixo6_2 pndr
e6_2idpndr <- recupmdata_id('governativas2')
e6_2 <- data.table::fread('coleta/cache/governativas2_aedi/governativas2_aedi1323.csv')

e6_2[,mdata_id:=e6_2idpndr]

e6_2 <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][e6_2[,refdate:=as.Date(paste0(ano,'-12-31'))],on=c('local'='local')][
    ,.(mdata_id,refdate,local_id,value=governativas2_aedi)]



dbx::dbxUpsert(pndrupsert,'data_values',
               e6_2,where_cols=c('mdata_id','refdate','local_id'))


#eixo6_3 pndr
e6_3idpndr <- recupmdata_id('governativas3')
e6_3 <- data.table::fread('coleta/cache/governativas3_aedi/gov3_aedi_1323.csv')

e6_3[,mdata_id:=e6_3idpndr]

e6_3 <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][e6_3[,refdate:=as.Date(paste0(ano,'-12-31'))],on=c('local'='local')][
    ,.(mdata_id,refdate,local_id,value=governativas3_aedi)]



dbx::dbxUpsert(pndrupsert,'data_values',
               e6_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo6_4 pndr
e6_4idpndr <- recupmdata_id('governativas4',db=mdr)
e6_4 <- readRDS('coleta/cache/governativas4_aedi/ifsmparcial.rds')
data.table::setDT(e6_4)

e6_4[,mdata_id:=e6_4idpndr]

e6_4[codmun==53,codmun:=5300108]

e6_4 <-
  data.table::setDT(locgeoloc)[e6_4[,refdate:=as.Date(paste0(ano,'-12-31'))],on=c('geoloc_id'='codmun')][
    ,.(mdata_id,refdate,local_id,value)]



dbx::dbxUpsert(pndrupsert,'data_values',
               e6_4,where_cols=c('mdata_id','refdate','local_id'))



#e6 governativas _composto no db pndr
e6_cidpndr <- recupmdata_id('comp_governativas',mdr)

e6_c <- data.table::fread("coleta/cache/governativas_composto_aedi/governativas_c_aedi.csv")

e6_c <- e6_c|>dplyr::transmute(refdate,mdata_id=e6_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               e6_c,where_cols=c('mdata_id','refdate','local_id'))




#eixo7_1 pndr
e7_1idpndr <- recupmdata_id('sust1')
e7_1 <- data.table::fread('coleta/cache/sust1_aedi/sust1_aedi.csv')

e7_1[,mdata_id:=e7_1idpndr]

e7_1 <- e7_1[ano>2012,]
#e7_1 <- e7_1[ano<2024,]

e7_1 <-
  data.table::setDT(locgeoloc)[,local:=trunc(geoloc_id/10)][e7_1[,refdate:=as.Date(paste0(ano,'-12-31'))],on=c('local'='local')][
    ,.(mdata_id,refdate,local_id,value=sust1_aedi)]

e7_1 <- e7_1[!is.na(local_id),]


dbx::dbxUpsert(pndrupsert,'data_values',
               e7_1,where_cols=c('mdata_id','refdate','local_id'))

#eixo7_2 pndr
e7_2idpndr <- recupmdata_id('sust2')
e7_2 <- data.table::fread('coleta/cache/sust2_aedi/sust2_aedi_max100.csv')

e7_2[,mdata_id:=e7_2idpndr]

e7_2 <-
  data.table::setDT(locgeoloc)[e7_2[,refdate:=as.Date(paste0(year,'-12-31'))],on=c('geoloc_id'='geocode_ibge')][
    ,.(mdata_id,refdate,local_id,value=desmatamento_acumulado)]




dbx::dbxUpsert(pndrupsert,'data_values',
               e7_2,where_cols=c('mdata_id','refdate','local_id'))

#eixo7_3 pndr
e7_3idpndr <- recupmdata_id('sust3')
e7_3 <- data.table::fread('coleta/cache/sust2_aedi/sust2_aedi_max100.csv')

e7_3[,mdata_id:=e7_3idpndr]

e7_3 <-
  data.table::setDT(locgeoloc)[e7_3[,refdate:=as.Date(paste0(year,'-12-31'))],on=c('geoloc_id'='geocode_ibge')][
    ,.(mdata_id,refdate,local_id,value=area_desmatamento)]


e7_3 <- e7_3[refdate>'2007-12-31',]

dbx::dbxUpsert(pndrupsert,'data_values',
               e7_3,where_cols=c('mdata_id','refdate','local_id'))

#eixo7_4 pndr
e7_4idpndr <- recupmdata_id('sust4')
e7_4 <- data.table::fread('coleta/cache/sust4_aedi.csv')

e7_4[,mdata_id:=e7_4idpndr]

e7_4 <-
  data.table::setDT(locgeoloc)[e7_4,on=c('geoloc_id'='geoloc_id')][
    ,.(mdata_id,refdate,local_id,value=emissoes)]

e7_4 <- e7_4|>
  dplyr::group_by(mdata_id,refdate,local_id)|>
  dplyr::summarise(value=max(value,na.rm=TRUE))

dbx::dbxUpsert(pndrupsert,'data_values',
               e7_4,where_cols=c('mdata_id','refdate','local_id'))


#e7 sust _composto no db pndr
e7_cidpndr <- recupmdata_id('comp_sust',mdr)

e7_c <- data.table::fread("coleta/cache/sust_composto_aedi/sust_c_aedi.csv")

e7_c <- e7_c|>dplyr::transmute(refdate,mdata_id=e7_cidpndr,local_id,value)

dbx::dbxUpsert(pndrupsert,'data_values',
               e7_c,where_cols=c('mdata_id','refdate','local_id'))



###Atualização de metadados:

mdata_previo <- dbx::dbxSelect(pndrupsert,'select * from mdata')

mdata_atual <-
  mdata_previo|>
  dplyr::mutate(
    data_name=dplyr::case_when(
      orig_name=='objetivo1_3' ~ 'Diferencial entre Número de Médicos por Habitante e Mediana Nacional',
      orig_name=='objetivo3_1' ~ 'Percentual de vínculos formais com ensino superior',
      orig_name=='desprod4' ~ 'Escala produtiva',
      orig_name=='educ3' ~'Indicador de Adequação da Formação Docente',
      orig_name=='infra4' ~ 'Despesas nas áreas de habitação e recuperação de áreas degradadas per capita',
      orig_name=='dessoc2' ~ 'Percentual de Pessoas de Baixa Renda no Cadastro Único em relação à população',
      T ~ data_name),
    data_desc=dplyr::case_when(
      orig_name=='objetivo1_1' ~ ' (massa salarial municipal / total de trabalhadores formais) – mediana nacional de (massa salarial municipal / total de trabalhadores formais). O cálculo deve ser feito observando os setores de agricultura, indústria, comercio e serviços, com a exceção do setor de Adm. Pública).',
      orig_name=='objetivo1_2' ~ '(índice anos iniciais + índice anos finais)/2 – mediana nacional de (índice anos iniciais + índice anos finais)/2. Para anos pares, utilizar o valor do índice do ano anterior.',
      orig_name=='objetivo1_3' ~ ' Número de médicos (tabela de ocupações TABNET/datasus) dividido pela população total, por ano e município – mediana nacional anual desse número',
      orig_name=='objetivo2_1' ~ 'ICi = -somatorio(s=1 s) Esi/Ei  * Rs * ln(Esi/Ei)  , onde Esi representa o número de empregos do setor s no município i e Ei o número total de empregos no município i; = Rs = 1− Esm / Em mede a raridade do setor s na área central m, onde Esm é o número de empregos do setor s na UF m e Em o total de empregos na UF m.',
      orig_name=='objetivo2_2' ~ 'projeção ou estimativa de população utilizada para cálculo das cotas do Fundo de Participação dos Estados e Municípios do ano corrente / mesmo indicador da maior cidade da UF',
      orig_name=='objetivo2_3' ~ 'Massa salarial municipal / massa salarial da maior cidade da UF',
      orig_name=='objetivo3_1' ~ '(Número de vínculos ativos em 31/12 ocupados por trabalhadores com ensino superior, mestrado ou doutorado / número total de vínculos ativos em 31/12) x 100',
      orig_name=='objetivo3_2' ~ 'massa salarial municipal / total de trabalhadores formais. O cálculo deve ser feito observando os setores de agricultura, indústria, comercio e serviços, com a exceção do setor de Adm. Pública).',
      orig_name=='objetivo3_3' ~ 'projeção ou estimativa de população utilizada para cálculo das cotas do Fundo de Participação dos Estados e Municípios do ano corrente / mesmo indicador do ano imediatamente anterior',
      orig_name=='objetivo4_1' ~ 'Quociente Locacional do setor agrícola considerando como referência a média nacional . QL agricola = % de empregos agrícolas no total do município / % empregos agrícolas no total nacional, Divisão da CNAE = 01',
      orig_name=='objetivo4_2' ~ 'Quociente Locacional do setor de mineração considerando como referência a média nacional . QL mineração = % de empregos no setor de mineração no total do município / % empregos em mineração no total nacional, Divisões CNAE 05,06,07,08 e 09',
      orig_name=='objetivo4_3' ~ ' 1 – [(número de trabalhadores no setor s, município m / número de trabalhadores no município m) - (número de trabalhadores no setor s, Brasil / número de trabalhadores no Brasil)] / 2',
      orig_name=='desprod1' ~ 'Índice de Complexidade das Localidades - Emprego, por ano e município',
      orig_name=='desprod2' ~ '[número de trabalhadores formais empregados na atividade industrial / total de trabalhadores formais]/100.',
      orig_name=='desprod3' ~ 'massa salarial municipal/total de trabalhadores formais. O cálculo deve ser feito observando os setores de agricultura, indústria, comercio e serviços, com a exceção do setor de Adm. Pública).',
      orig_name=='desprod4' ~ 'massa salarial municipal dividido pelo número total de estabelecimentos no município. Observação: O cálculo deve ser feito desconsiderando o setor de Administração Pública - Divisão 84 CNAE (Administração pública, defesa e seguridade social)',
      orig_name == 'citec1' ~ 'número empresas (de 1 a 49 empregados) ativas em 31/12 registradas nos grupos 21.1 (Fabricação de produtos farmoquímicos), 21.2 (Fabricação de produtos farmacêuticos), 26.6 (Fabricação de aparelhos eletromédicos e eletroterapêuticos e equipamentos de irradiação) e 32.5 (Fabricação de instrumentos e materiais para uso médico e odontológico e de artigos ópticos) da CNAE 2.0, dividido pelo número de habitantes da localidade vezes 1 milhão.',
      orig_name == 'citec2' ~ 'número vínculos ativos em 31/12 registrados nos subgrupos 203 (Pesquisadores), 234 (Professores do ensino superior) e 395 (Técnicos de apoio em pesquisa e desenvolvimento) da CBO, dividido pelo número de habitantes da localidade vezes 1 milhão.',
      orig_name == 'citec3' ~ 'número empregos em estabelecimentos registrados na divisão 72 (Pesquisa e Desenvolvimento científico) da CNAE, dividido pela população da localidade vezes 1 milhão.',
      orig_name == 'citec4' ~ 'número de pedidos de patentes do tipo Patente de Modelo de Utilidade e Patente de Invenção depositadas no INPI por local de residência dos inventores, dividido pelo número de habitantes vezes 100 mil.',
      orig_name == 'educ1' ~ 'número de estabelecimentos com acesso rede de esgotamento sanitário, por ano e município, dividido pelo número total de escolas no município',
      orig_name == 'educ2' ~ 'número de estabelecimentos com acesso à internet, por ano e município, dividido pelo número total de escolas no município.',
      orig_name=='educ3' ~"Proporção de docentes no ensino médio com formação superior de licenciatura (ou bacharelado com complementação pedagógica) na mesma área da disciplina que leciona ('Grupo 1' nas planilhas municipais do INEP), total (urbana e rural), de todas as dependências administrativas (categoria 'total' de dependência administrativa), por município e ano.",
      orig_name=='educ3' ~"Nota média dos índices divulgados para os anos iniciais e finais do ensino fundamental. Para anos pares, utilizar o valor do índice do ano anterior.",
      orig_name=='infra1' ~ 'População urbana atendida com abastecimento de água (Até 2021 - AG026) / População urbana residente do município com abastecimento de água (Até 2021 - GE06a) * 100. 2022 - Informação estimada a partir de microdados do Censo IBGE 2022. 2023 em diante - variável nomeada "IAG0002" ',
      orig_name=='infra2' ~ 'soma dos acessos a internet de alta velocidade (> 34 Mbps por disponibilidade/viabilidade)/soma total de acessos a internet, para Pessoas Físicas.',
      orig_name=='infra3' ~ 'somatório das internações pelas CIDS: doenças de transmissão feco-oral (diarreias [A09], febres entéricas [A25] e hepatite A [B15]); doenças transmitidas por inseto vetor (dengue [A90], febre amarela [A95], leishmanioses [B55], leishmaniose tegumentar [B55.9], leishmaniose visceral [B55.0], filariose linfática [B74], malária [B50] e doença de Chagas [B57]); doenças transmitidas por contato com a água (leptospirose [A27] e esquistossomose [B65]); doenças relacionadas à higiene (doenças nos olhos [Z13.5], tracomas [H54.3], conjuntivites [H10], doenças da pele [B08] e micoses superficiais (B36]); e geo-helmintos e teníases (helmintíases [B82.0] e teníases [83.9]) / 10.000.',
      orig_name=='infra4' ~ 'Valor das despesas nominais pagas nas subfunções habitação (15) e recuperação de áreas degradadas (CONTA 18.543) / população',
      orig_name=='dessoc1' ~ 'internações por Desnutrição (E40 - E4620) em relação a internações totais, por ano e município.',
      orig_name=='dessoc2' ~ 'Percentual de Pessoas de Baixa Renda no Cadastro Único em relação à população',
      orig_name=='dessoc3' ~ 'porcentagem dos alunos matriculados que têm idade pelo menos 2 anos maior do que a idade esperada para aquela série, por ano e município.',
      orig_name=='dessoc4' ~ '(massa salarial municipal feminina / total de trabalhadoras formais do sexo feminino)/(massa salarial municipal masculina / total de trabalhadores formais do sexo masculino). O cálculo deve ser feito observando os setores agricultura, indústria, comercio e serviços, com a exceção do setor de Adm. Pública).',
      orig_name=='governativas1' ~ 'Número total de Dirigentes da Administração Pública Municipal com ensino superior completo (Natureza Jurídica Especial = Setor Público Municipal) sobre o número total de Dirigentes  da Administração Pública Municipal. Famílias CBO 1112 e 114',
      orig_name=='governativas2' ~ 'Número total de empregados da Administração Pública em Geral com ensino superior completo (Natureza Jurídica Especial = Setor Público Municipal) sobre o número total de empregados da Administração Pública em Geral Municipal.',
      orig_name=='governativas3' ~ 'Massa salarial da Administração Pública em Geral (Natureza Jurídica Especial = Setor Público Municipal) sobre o número total de empregados da Administração Pública em Geral (Natureza Jurídica Especial = Setor Público Municipal). Observação: É considerada "Administração Pública em Geral" a Classe CNAE 8411-6.',
      orig_name=='governativas4' ~ ' razão entre receitas de arrecadação própria e receitas totais dos municípios.',
      orig_name=='sust1' ~ 'número empregos em estabelecimentos registrados na divisão 38 (Coleta, Tratamento e Disposição de Resíduos) e 39 (Descontaminação e outros serviços de Gestão de Resíduos) da CNAE, dividido pela população da localidade vezes 1 milhão.',
      orig_name=='sust2' ~ 'área desmatada acumulada no município',
      orig_name=='sust3' ~ 'área desmatada no ano de análise – área desmatada no ano anterior.',
      orig_name=='sust4' ~ 'Emissões de gases de efeito estufa – remoções de gases de efeito estufa (por mudança de uso da terra, vegetação secundária ou florestas protegidas), para os setores "Agropecuária" e "Processos Industriais"',
      T ~ data_name))

dbx::dbxUpsert(pndrupsert,'mdata',
               mdata_atual,where_cols=c('mdata_id'))

#Eixo 6 e 7 Trocados
datag_previo <- dbx::dbxSelect(pndrupsert,'select * from datagroup')

datag_a <-
  datag_previo|>
  dplyr::mutate(
    datagroup_name= dplyr::case_when(
      datagroup_id == 6 ~ 'Eixo 7',
      datagroup_id == 7 ~ 'Eixo 6',
      T ~ datagroup_name
    )
  )

dbx::dbxUpsert(pndrupsert,'datagroup',
               datag_a[1:12,],where_cols = c('datagroup_id'))
