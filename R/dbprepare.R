#' Prepare app_db
#'
#' Writes sqlite database on current dir or pgsql tables to aedi_db
#' if 'pgsql' , user , password and database should be set up
#' separately on the corresponding PostgreSQL Server
#'
#' @name prepare_db
#' @param tdbname Name of the database that will be created, defaults to aedidb
#' @param type type of db backend - defaults to sqlite, alternative pgsql
#' @param userdb Name of the user with connection permissions
#' @param passwddb userdb password for connecting to tdbname
#' @param hostdb host resolvable domain or address
#' @param postgis is postgis extension enabled on pgsql database? defaults to TRUE
#'  @importFrom RSQLite SQLite
#'  @importFrom DBI dbExecute dbGetQuery dbSendQuery

#Clean environment
rm(list=ls())
prepare_db <- \(tdbname="aedidb",type="sqlite",userdb="aedi",passwddb="aEd1#man@gR",hostdb='127.0.0.1',postgis=TRUE,geo=TRUE) {
  ##base data
  {

    spanperiod_group <- tibble::tribble(
      ~datagroup_id,~period_start,~period_end
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name"),as.character),
                    dplyr::across(c("period_start","period_end"),as.Date))


    data_values <- tibble::tribble(
      ~mdata_id,~local_id,~refdate,~value
    ) |>dplyr::mutate(
      dplyr::across(dplyr::matches("_id$"),as.integer),
      dplyr::across(refdate,as.Date),
      dplyr::across(value,as.double)
    )

    geoloc <- tibble::tribble(
      ~geoloc_id,~geometry
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),dplyr::across(geometry,as.list))


    institution <- tibble::tribble(
      ~institution_id,~institution_sname,~institution_name,~institution_url,~institution_desc)|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
dplyr::across(dplyr::matches("dataunit|source|url|name|desc"),as.character))

    officialer <- tibble::tribble(
      ~officialer_id,~officialer_name,~officialer_email,~officialer_tel,~institution_id,~officialer_obs
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$|tel"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name|desc|email|obs"),as.character))

    local_group <- tibble::tribble(
      ~local_id,~datagroup_id
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name"),as.character))

    local <- tibble::tribble(
      ~local_id,~geoloc_id,~local_name,
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),
                     dplyr::across(local_name,as.character))

    datagroup <- tibble::tribble(
      ~datagroup_id,~datagroup_name,~datagroup_desc
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name|desc"),as.character))

    group_parent <- tibble::tribble(
      ~datagroup_id,~datagroup_parentid
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name"),as.character))

    mdata_group <- tibble::tribble(
      ~mdata_id,~datagroup_id
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name"),as.character))



    mdata <- tibble::tribble(
      ~mdata_id,~orig_name,~data_name,~data_desc
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("_id$"),as.integer),
                    dplyr::across(dplyr::matches("dataunit|source|url|name|desc"),as.character))

    mdata_exts <- tibble::tribble(
      ~mdata_id,~data_class_id,~data_freq_id,~dataunit_num,~dataunit_den,~data_type_id,~datasource_id,~data_url,~mdata_obs
    ) |>
      dplyr::mutate(
                    dplyr::across(dplyr::matches("dataunit|source|url|name|_obs"),as.character),
                    dplyr::across(dplyr::matches("_id$"),as.integer))

    mdata_timetable <-  tibble::tribble(
      ~mdata_id,~last_refdate,~last_update
    )|>
      dplyr::mutate(dplyr::across(dplyr::matches("id$"),as.integer),dplyr::across(dplyr::contains("date"),as.Date))

    data_class <- tibble::tribble(
      ~data_class_id,~class_name,
      1,"Indicador/Dado Bruto",
      2,"Indicador direto",
      3,"Indicador combinado",
      4,"Indicador composto",
      5,"Espacial - indicador transformado para ",
      6,"Temporal - indicador transformado para "
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),
                     dplyr::across(dplyr::contains("name"),as.character))


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
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),
                     dplyr::across(dplyr::contains("name"),as.character))

    data_type <- tibble::tribble(
      ~data_type_id,~type_name,
      1, "estoque",
      2, "fluxo",
      3, "rank"
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),
                     dplyr::across(dplyr::contains("name"),as.character))

    datasource_type <- tibble::tribble(
      ~datasource_type_id,~datasource_type_name,
      1,"url fixo",
      2, "arquivo local(upload)",
      3, "dados.gov.br",
      4, "ckan",
      5, "ibge_sidra",
      6, "ibge_ftp",
      7, "ipeadata",
      8, "bcb",
      9, "arquivo do servidor",
      10, "url de pasta ou combinada",
      11,"datasus",
      12,"dataviva",
      13,"inpi",
      14,"finbra",
      15,"terrabrasilis",
      16,"derivado_interno"
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),dplyr::across(dplyr::contains("name"),as.character))


    datasource <- tibble::tribble(
      ~datasource_id,~datasource_name,~datasource_desc,
      ~datasource_url,~data_freq_id,~datasource_lastupdate,
      ~datasource_type_id,~institution_id,
      ~officialer_id,~datasource_delay
        )|>
      dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),
                    dplyr::across(dplyr::contains("_delay"),as.integer),
                    dplyr::across(dplyr::matches("name|url|desc"),as.character),
                    dplyr::across(dplyr::contains("date"),as.Date))


    mvis <- tibble::tribble(
      ~mvis_id,~vis_name,~vis_type_id,~vis_focus_id,
    )|>dplyr::mutate(dplyr::across(dplyr::contains("_id"),as.integer),dplyr::across(dplyr::contains("name"),as.character))

    visdata <- tibble::tribble(
      ~mvis_id,~data_class_id
    )|>dplyr::mutate(dplyr::across(dplyr::everything(),as.integer))


    vis_type <- tibble::tribble(
      ~vis_type_id,~vistype_name,
      1,"tabela",
      2,"gr\u00e0fico",
      3,"mapa"
    )|>dplyr::mutate(dplyr::across(dplyr::contains("id"),as.integer),dplyr::across(dplyr::contains("name"),as.character))

    vis_focus <- tibble::tribble(
      ~vis_focus_id,~focus_name,
      1,"espacial",
      2,"temporal",
      3, "complexo"
    )|>dplyr::mutate(dplyr::across(dplyr::contains("id"),as.integer),dplyr::across(dplyr::contains("name"),as.character))


    ##Define how many first cols as primary_key for all base tables
    npks <- data.frame("table"=ls(pattern="^[^ctphu]"),
                       "n_pk" = 1)


    npks[npks$table== "data_values",]$n_pk <- 3
    npks[npks$table== "visdata",]$n_pk <- 2
    npks[grepl("group_|_group",npks$table),]$n_pk <- 2
    npks$nids  <-  sapply(npks$table,\(x){sum(grepl("_id$",names(get(x))))})

    npks$ownpk <- sapply(npks$table,\(x){grepl(paste0("^",x,"_id"),names(get(x)[1]))})

    npks <- npks|>dplyr::arrange(dplyr::desc(ownpk),n_pk,nids)
    ##Manual hack
    npks <- rbind(npks[npks$table!="data_values",],npks[npks$table=="data_values",])
    ##Get pk col names
    npks$cols <- mapply(\(y,x)names(get(y))[1:x],
                        npks$table,npks$n_pk,USE.NAMES = F)

  }



  ##util functions
  retrieve_fks <- \(tblname,fknum=1){

    if(!exists("n_pk")){n_pk <- 1}
    #Hack for 1 to 1 relationship
    print(tblname)
    nmtb <- names(get(tblname))

    if (n_pk==1 & grepl(paste0("^",tblname,"_id"),nmtb[1]) ) {

      nmbut_pk <- nmtb[-1]
    } else {
      nmbut_pk <- nmtb
    }
    print(nmbut_pk)
    nm_fk_ft <- data.frame(
      fk=nmbut_pk[grepl("id$",nmbut_pk)])
    if(length(nm_fk_ft)>0){
      nm_fk_ft <- nm_fk_ft    |>
        dplyr::mutate(ft=gsub("_id$|_parentid","",fk))
    }
    print("finished.")
    nm_fk_ft
    }


  pk_add <- \(tbname,colsname="_id") {
    if (sum(colsname=="_id")>0) {
      colsname <- paste0(tbname,"_id")
    }
    print(paste("pkadd",tbname))
    try(DBI::dbExecute(con,
                         paste0("ALTER TABLE ", tbname,
                                " DROP CONSTRAINT IF EXISTS ",tbname, "_pkey;")))

    if (length(colsname)==1 ) {
      bq <-           paste0("ALTER TABLE ", tbname,
                             " ADD PRIMARY KEY (",colsname, ");")
      if(colsname==paste0(tbname,"_id") &names(get(tbname))[1]==colsname) {
        upc <- paste0("ALTER TABLE ",tbname,
                      " ALTER COLUMN ",colsname)
        query <- paste0(upc,
                        " SET NOT NULL;")
        try(DBI::dbExecute(con,
                             query))
        query <- paste0(upc,
                        " ADD GENERATED BY DEFAULT AS IDENTITY;"
        )
        try(DBI::dbExecute(con,
                             query))
      }
    } else {
      bq <-           paste0("ALTER TABLE ", tbname,
                             paste0(" ADD PRIMARY KEY (",paste0(colsname,collapse=", "),");"))

    }

    try(DBI::dbExecute(con,
                         bq))
  }

  popultab <- \(i) {
    print(paste("populating table",i))
    tabel <- get(i)
    fieldtypes <- sapply(tabel,class)
    if (type=="pgsql") {
      fieldtypes <- gsub("character","text",fieldtypes)
      fieldtypes <- gsub("list","array",fieldtypes)

    }
    attr(fieldtypes,"name") <- names(i)

    if(postgis & type=="pgsql" & "array" %in% fieldtypes) {
      tabel <- sf::st_as_sf(tabel,crs="wsg84",sf_column_name="geometry")

#      sf::st_write(tabel,con,i, delete_layer = TRUE)
      sf::write_sf(tabel,con,drop=T)
      try(DBI::dbExecute(con,paste0("ALTER TABLE tabel RENAME TO ",i)))
    } else {
      try(DBI::dbWriteTable(con,i,tabel,field.types=fieldtypes))
    }
  }


  fk_relations <- \(tbname,n_pk=1)  {


    fk_ids <- retrieve_fks(tbname)
    fk_add <- \(fk,ft){
      fkname <- paste0("fk_",ft,"_",fk)
      bq <-  paste0("ALTER TABLE ",tbname,
                    " ADD CONSTRAINT ",fkname,
                    " FOREIGN KEY (",fk,") REFERENCES ",
                    ft,"(",paste0(ft,"_id"),")"
      )
      if(fk=="mdata_id"){
        bq <- paste0(bq," ON DELETE CASCADE")
      }

      try(DBI::dbExecute(con,bq))
    }
    mapply(fk_add,fk_ids$fk,fk_ids$ft,USE.NAMES=F)
  }

  altera_adiciona_chave <- \(atbname=tbname,n_pk=1,sqlitedbname = tdbname)  {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlitedbname)
    id_tab <- names(get(atbname))
    # get_pks <- \(atbname,pknumber=1){
    #   names(get(atbname))[pknumber]
    # }


    fk_ids <- retrieve_fks(atbname)

    ##0) If foreign key constraints are enabled, disable them using PRAGMA foreign_keys=OFF.
    #DBI::dbExecute(con,"PRAGMA foreign_keys=OFF")
    ##1) start a transaction
    DBI::dbExecute(con,"BEGIN TRANSACTION")


    ##2) run pgrama schema_version
    version <- DBI::dbGetQuery(con, "PRAGMA schema_version;")

    ##3) remember format of all indexes, triggers and views
    caract_tab <- DBI::dbGetQuery(con, paste0("SELECT type,sql FROM sqlite_schema WHERE tbl_name ='",atbname,"'"))



    createquery <- gsub("\\n"," ",caract_tab$sql[1])
    ##4.0) RENAME TABLE
    DBI::dbSendQuery(con,paste0("ALTER TABLE `",atbname,"` RENAME TO `",atbname,"_old`"))
    ##4) USE CREATE TABLE
    if (n_pk == 1) {
      print(paste("adiciona \\u00fanica chave prim\u00e0ria para tabela ",atbname))
      createquery <-gsub(paste0("(",id_tab[1],"`) ([^,]*),"),"\\1 \\2 PRIMARY KEY,", createquery)
    } else {
      print(paste("adiciona",n_pk,"chaves prim\u00e0rias  para tabela ",atbname))
      createquery <- gsub("\\)$",paste0(", PRIMARY KEY (",paste0("`",id_tab[1:n_pk],collapse="`, "),"`))"),createquery)
    }

#    print("processar chaves externas")
    adiciona_foreign <- \(fk,ft){
      if(length(fk)!=0) {
        #"(.*)[, ]+(`",fk,"`),*",
        result <- paste0(gsub("\\)$","",createquery),", CONSTRAINT ","fk_",ft,
                         " FOREIGN KEY (",fk,") REFERENCES ",ft,"(",fk,"))")
        assign("createquery",result,envir = parent.frame(2))
      }
    }

    if(length(fk_ids)>0){
      if(nrow(fk_ids)>0){
        mapply(adiciona_foreign,fk_ids$fk,fk_ids$ft)
        }
    }
    createquery <- paste0(gsub("(\\( +, +)([^ ])","(\\2",gsub(",( +,)+",", ",createquery),")"))
    # createquery <- gsub("(_id` )REAL","\\1INTEGER",createquery)

    ##hack to fix same fk two times in |>

    if(atbname=="group_parent"){
#      createquery <- gsub("(^.*)(PRIMARY KEY)(.*),([^,]+$)","\\1 \\3,\\2 (datagroup_id,datagroup_parentid)\\4",paste(gsub(")$","",createquery),"CONSTRAINT fk_parent FOREIGN KEY (datagroup_parentid) REFERENCES datagroup(datagroup_id))"))
      createquery <- gsub("\\)$",", CONSTRAINT fk_parent FOREIGN KEY (datagroup_parentid) REFERENCES datagroup(datagroup_id))",createquery)
    }


    DBI::dbSendQuery(con,createquery)

    ##5) INSERT INTO
    DBI::dbSendQuery(con,paste0("INSERT INTO ",atbname," SELECT * FROM `",atbname,"_old`;"))

    ##6) DROP TABLE
    DBI::dbSendQuery(con,paste0("DROP TABLE ",atbname,"_old;"))

    ##7) If foreign key constraints were enabled run PRAGMA foreign_key_check
    DBI::dbSendQuery(con,"PRAGMA foreign_key_check")

    ##8) Commit transaction started in 2
    DBI::dbExecute(con,"COMMIT TRANSACTION")
    ##9) If 0 done, undo
    #DBI::dbExecute(con,"PRAGMA foreign_keys=ON")


    DBI::dbDisconnect(con)
  }


  if(type=="sqlite"){
    tdbname=paste0(tdbname,".sqlite")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tdbname)
    DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
    DBI::dbExecute(con, "PRAGMA synchronous=NORMAL")
    lapply(ls(pattern="^[gmdlvios]"),popultab
    )

    DBI::dbDisconnect(con)
    ##Define relationships (based on https://www.sqlite.org/lang_altertable.html#otheralter)
    mapply(altera_adiciona_chave,npks$table,npks$n_pk,USE.NAMES=F)

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tdbname)

    DBI::dbExecute(con,paste0("CREATE VIEW named_datavalues as ",
                              "SELECT datasource_name,orig_name,",
                              "mdata_exts.data_freq_id,data_type_id,local_id,refdate,
                              COALESCE(value,0) value FROM ",
                              "data_values LEFT JOIN mdata ON ",
                              "data_values.mdata_id = mdata.mdata_id LEFT JOIN ",
                              "mdata_exts ON data_values.mdata_id = ",
                              "mdata_exts.mdata_id LEFT JOIN datasource ON ",
                              "mdata_exts.datasource_id = datasource.datasource_id "
                              ))
    DBI::dbDisconnect(con)

  } else if (type=="pgsql") {
    con <- DBI::dbConnect(RPostgres::Postgres(), dbname=tdbname,
                          user=userdb,password=passwddb,host=hostdb,
                          options = "-c client_min_messages=warning")

    lapply(npks$table,popultab)

    ## add pk first time
    mapply(pk_add,npks[npks$ownpk,]$table,npks[npks$ownpk,]$cols,USE.NAMES = F)

    ## Add foreing keys to all tables with foreign keys
    npkwf <- npks[npks$ownpk!=T | npks$nids >npks$n_pk ,]
    print("Check tables with fk")
    mapply(fk_relations,npkwf$table,npkwf$n_pk,USE.NAMES=F)

    print("Now adding primary keys")
    ## Add all primary keys
    mapply(pk_add,npks$table,npks$cols,USE.NAMES = F)

    ## Review at some point if this for tables:
    ## data_class, data_freq, data_type,datagroup,datasource_type,
    ## geoloc, institution, mdata, vis_focus, vis_type, local, officialer
    ## mvis, datasource
    ##Error : Failed to fetch row: ERROR:  multiple primary keys for table \"data_class\" are not allowed\n\n


    DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW named_datavalues as ",
                              "SELECT datasource_name,orig_name,",
                              "mdata_exts.data_freq_id,data_type_id,local_id,refdate,value FROM ",
                              "data_values LEFT JOIN mdata ON ",
                              "data_values.mdata_id = mdata.mdata_id LEFT JOIN ",
                              "mdata_exts ON data_values.mdata_id = ",
                              "mdata_exts.mdata_id LEFT JOIN datasource ON ",
                              "mdata_exts.datasource_id = datasource.datasource_id "
                              ))
    if(geo){
      source('R/create_extend_geogroup_view.R')
      criar_recortes_geograficos()
      DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW geonamed_datavalues as SELECT named_datavalues.*, recortes_geograficos.* FROM named_datavalues LEFT JOIN ",
                                "local ON named_datavalues.local_id  = local.local_id LEFT JOIN recortes_geograficos ON ",
                                "local.geoloc_id = recortes_geograficos.codigo_ibge"))

      # Não funcionou não índice único
      # DBI::dbExecute(con,
      #                paste0("CREATE UNIQUE INDEX IF NOT EXISTS origname_index ON
      #                 public.geonamed_datavalues USING btree
      #                 (orig_name ASC NULLS LAST) WITH (FILLFACTOR=90)
      #                 TABLESPACE pg_default;"))

    }
    DBI::dbDisconnect(con)

  }


}
