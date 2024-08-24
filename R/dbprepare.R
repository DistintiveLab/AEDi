#' Prepare app_db
#'
#' Writes sqlite database on current dir
#'
#' @name prepare_db
#' @param tdbname Name of the database that will be created
#'  @importFrom RSQLite SQLite
#'  @importFrom DBI dbExecute dbGetQuery dbSendQuery

#Clean environment
rm(list=ls())
prepare_db <- \(tdbname="dashboard_db.sqlite") {

con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tdbname)
# if concurrent use is to be expected
DBI::dbExecute(con, "PRAGMA busy_timeout=10000")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
DBI::dbExecute(con, "PRAGMA synchronous=NORMAL")

data_values <- tibble::tribble(
  ~mdata_id,~local_id,~refdate,~value
) |>dplyr::mutate(
  dplyr::across(matches("_id$"),as.integer),
  dplyr::across(refdate,as.Date),
  dplyr::across(value,as.double)
)

 geoloc <- tibble::tribble(
   ~geoloc_id,~geometry
 )|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),dplyr::across(geometry,as.list))

local <- tibble::tribble(
  ~local_id,~geoloc_id,~local_name,
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),
          dplyr::across(local_name,as.character))

datagroup <- tibble::tribble(
  ~datagroup_id,~data_group_name,~datagroup_desc
)|>
  dplyr::mutate(dplyr::across(matches("_id$"),as.integer),
                dplyr::across(matches("dataunit|source|url|name|desc"),as.character))

group_parent <- tibble::tribble(
  ~datagroup_id,~datagroup_parentid
)|>
  dplyr::mutate(dplyr::across(matches("id$"),as.integer),
                dplyr::across(matches("dataunit|source|url|name"),as.character))

mdata_group <- tibble::tribble(
  ~mdata_id,~data_group_id
)|>
  dplyr::mutate(dplyr::across(matches("_id$"),as.integer),
                dplyr::across(matches("dataunit|source|url|name"),as.character))

mdata <- tibble::tribble(
  ~mdata_id,~data_name,~data_desc
)|>
  dplyr::mutate(dplyr::across(matches("_id$"),as.integer),
         dplyr::across(matches("dataunit|source|url|name|desc"),as.character))

mdata_exts <- tibble::tribble(
  ~mdata_id,~data_class_id,~data_freq_id,~dataunit_num,~dataunit_den,~data_type_id,~data_source,~datasource_type_id,~data_url
) |>
  dplyr::mutate(dplyr::across(matches("_id$"),as.integer),
                dplyr::across(matches("dataunit|source|url|name"),as.character))
mdata_timetable <-  tibble::tribble(
  ~mdata_id,~last_refdate,~last_update
)|>
  dplyr::mutate(dplyr::across(matches("id$"),as.integer),dplyr::across(contains("date"),as.Date))

data_class <- tibble::tribble(
  ~data_class_id,~class_name,
  1,"Dado Bruto",
  2,"Indicador"
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),
                 dplyr::across(contains("name"),as.character))


data_freq <- tibble::tribble(
  ~data_freq_id,~freq_name,~freq_ndays,
  1,"di\u00e0ria",1,
  2,"semanal",7,
  3,"quinzenal",15,
  4,"mensal",30,
  5,"bimestral",60,
  6,"trimestral",90,
  7,"quadrimestral",120,
  8,"semestral",180,
  9,"anual",365,
  10,"bienal",730
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),
                 dplyr::across(contains("name"),as.character))

data_type <- tibble::tribble(
  ~data_type_id,~type_name,
  1, "estoque",
  2, "fluxo",
  3, "rank"
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),
                 dplyr::across(contains("name"),as.character))

datasource_type <- tibble::tribble(
  ~datasource_type_id,~datasource_name,
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),dplyr::across(contains("name"),as.character))





mvis <- tibble::tribble(
  ~mvis_id,~vis_name,~vis_type_id,~vis_focus_id,
)|>dplyr::mutate(dplyr::across(contains("_id"),as.integer),dplyr::across(contains("name"),as.character))

visdata <- tibble::tribble(
  ~mvis_id,~mdata_id
)|>dplyr::mutate(dplyr::across(everything(),as.integer))


vis_type <- tibble::tribble(
  ~vis_type_id,~vistype_name,
  1,"tabela",
  2,"gr\u00e0fico",
  3,"mapa"
)|>dplyr::mutate(dplyr::across(contains("id"),as.integer),dplyr::across(contains("name"),as.character))

vis_focus <- tibble::tribble(
  ~vis_focus_id,~focus_name,
  1,"espacial",
  2,"temporal"
)|>dplyr::mutate(dplyr::across(contains("id"),as.integer),dplyr::across(contains("name"),as.character))



lapply(ls(pattern="^[gmdlv]"),\(i) {
  tabel <- get(i)
  fieldtypes <- #gsub("character","text",
    sapply(tabel,class)
  #)
  # print(str(fieldtypes))
  attr(fieldtypes,"name") <- names(i)
  print(i)
  print(fieldtypes)
  dbWriteTable(con,i,tabel,field.types=fieldtypes)
}
)


DBI::dbDisconnect(con)


##Define relationships (based on https://www.sqlite.org/lang_altertable.html#otheralter)

##Define how many first cols as primary_key de todas as tabelas
npks <- data.frame(table=ls(pattern="^[^ct]"),
                   n_pk = 1)


npks[npks$table== "data_values",]$n_pk <- 2
npks[npks$table== "visdata",]$n_pk <- 2
npks[npks$table== "mdata_group",]$n_pk <- 2
npks$nids  <-  sapply(npks$table,\(x){sum(grepl("_id$",names(get(x))))})

npks$ownpk <- sapply(npks$table,\(x){grepl(paste0("^",x,"_id"),names(get(x)[1]))})

npks <- npks|>dplyr::arrange(desc(ownpk),n_pk,nids)
##Manual hack
npks <- rbind(npks[npks$table!="data_values",],npks[npks$table=="data_values",])
print(npks)




altera_adiciona_chave <- \(tbname,n_pk=1,tdbname = "dashboard_db.sqlite")  {
  con <- dbConnect(SQLite(), tdbname)
  id_tab <- names(get(tbname))
  # get_pks <- \(tbname,pknumber=1){
  #   names(get(tbname))[pknumber]
  # }

  get_fks <- \(tbn=tbname,fknum=1){
    #Hack for 1 to 1 relationship
    print(tbn)
    print(n_pk)
    if (n_pk==1 & grepl(paste0("^",tbn,"_id"),names(get(tbn))[1]) ) {

    nmbut_pk <- names(get(tbn))[-1]
    } else {
      nmbut_pk <- names(get(tbn))
    }
    print(nmbut_pk)
    nm_fk_ft <- data.frame(
      fk=nmbut_pk[grepl("_id$",nmbut_pk)])
    if(length(nm_fk_ft)>0){
    nm_fk_ft <- nm_fk_ft    |>
      dplyr::mutate(ft=gsub("_id$","",fk))
    }
    #|>
     # dplyr::filter(ft!=tbname)
  }

  fk_ids <- get_fks()
  print(fk_ids)
  ##0) If foreign key constraints are enabled, disable them using PRAGMA foreign_keys=OFF.
  DBI::dbExecute(con,"PRAGMA foreign_keys=OFF")
  ##1) start a transaction
  DBI::dbExecute(con,"BEGIN TRANSACTION")



  ##2) run pgrama schema_version
  version <- DBI::dbGetQuery(con, "PRAGMA schema_version;")

  ##3) remember format of all indexes, triggers and views
  caract_tab <- DBI::dbGetQuery(con, paste0("SELECT type,sql FROM sqlite_schema WHERE tbl_name ='",tbname,"'"))

  createquery <- gsub("\\n"," ",caract_tab$sql[1])
  ##4.0) RENAME TABLE
  dbSendQuery(con,paste0("ALTER TABLE `",tbname,"` RENAME TO `",tbname,"_old`"))
  ##4) USE CREATE TABLE
  if (n_pk == 1) {
    print(paste("adiciona \\u00fanica chave prim\u00e0ria para tabela ",tbname))
    createquery <-gsub(paste0("(",id_tab[1],"`) ([^,]*),"),"\\1 \\2 PRIMARY KEY,", createquery)
  } else {
    print(paste("adiciona",n_pk,"chaves prim\u00e0rias  para tabela ",tbname))
    createquery <- gsub("\\)$",paste0(", PRIMARY KEY (",paste0("`",id_tab[1:n_pk],collapse="`, "),"`))"),createquery)
  }
  adiciona_foreign <- \(fk,ft){
    if(length(fk)!=0) {
    #"(.*)[, ]+(`",fk,"`),*",
    result <- paste0(gsub("\\)$","",createquery),", CONSTRAINT ","fk_",ft,
    " FOREIGN KEY (",fk,") REFERENCES ",ft,"(",fk,"))")
    assign("createquery",result,envir = parent.frame(2))
    }
  }
if(length(fk_ids)>0){mapply(adiciona_foreign,fk_ids$fk,fk_ids$ft)}
createquery <- paste0(gsub("(\\( +, +)([^ ])","(\\2",gsub(",( +,)+",", ",createquery),")"))
# createquery <- gsub("(_id` )REAL","\\1INTEGER",createquery)
##hack to fix same fk two times in |>

if(tbname=="group_parent"){
  createquery <- gsub("(^.*)(PRIMARY KEY)(.*),([^,]+$)","\\1 \\3,\\2 (datagroup_id,datagroup_parentid)\\4",paste(gsub(")$","",createquery),"CONSTRAINT fk_parent FOREIGN KEY (datagroup_parentid) REFERENCES datagroup(datagroup_id))"))
}
print(createquery)
  dbSendQuery(con,createquery)

  ##5) INSERT INTO
  dbSendQuery(con,paste0("INSERT INTO ",tbname," SELECT * FROM `",tbname,"_old`;"))

  ##6) DROP TABLE
  dbSendQuery(con,paste0("DROP TABLE ",tbname,"_old;"))

  # ##7) ALTER TABLE new_X RENAME TO X
  # dbSendQuery(con,paste0("ALTER TABLE ",tbname,"_new RENAME TO ",tbname))

  ##8) use create index, trigger, view <- not needed since all are recently created tables , using old format from carac_tab as guide

  ##9) If schema change affects views, DROP VIEW then CREATE_VIEW

  ##10) If foreign key constraints were enabled run PRAGMA foreign_key_check
  dbSendQuery(con,"PRAGMA foreign_key_check")

  ##11) Commit transaction started in 2
  dbExecute(con,"COMMIT TRANSACTION")
  ##12) If 0 done, undo
  DBI::dbExecute(con,"PRAGMA foreign_keys=ON")
  DBI::dbDisconnect(con)
}

mapply(altera_adiciona_chave,npks$table,npks$n_pk,USE.NAMES=F)



}
