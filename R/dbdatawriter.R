#' Do some checking, sanitizing and write to backend database
#' `db_datawrite()` does some checking, sanitizing and writes new data to
#' backend database
#'
#' @param metadf dataframe list with  the following expected structure :
#'  [[1]] -> df with
#'  orig_name
#'  data_name
#'  data_desc
#'  [[2]] -> data.frame with at least
#'  data_class_id
#'  data_freq_id
#'  dataunit_num   - TBI
#'  dataunit_den   - TBI
#'  datasource_id
#'  data_url <- construct ? call?
#'  @param datadf dataframe with the following expected structure:
#'  local <-> local_id or name/code pairable with local table
#'  periodo <- date or string/number in yyyy,yyyy-mm, yyyy-mm-dd
#'  valor <- numeric
#'  @param construct The call that extracted/obtained the data
#'  @param grp Should a datagroup be defined - for tables with subgroups (as sidra)
#'  @param engine Defaults to postgresql, sqlite TBI.
#'
#' @importFrom DBI dbConnect dbGetQuery dbAppendTable
#' @importFrom data.table rbindlist

db_datawrite <- \(metadf,datadf,construct,grp=FALSE,engine="postgresql",sanitize=TRUE,forceunique=T,replace=FALSE) {
  if(engine!="postgresql"){
    return("Em construção, não disponível outra engine ainda")
  }

  condw <- DBI::dbConnect(RPostgres::Postgres(),
                        user=Sys.getenv("user"),
                        password=Sys.getenv("password"),
                        host=Sys.getenv("host"),
                        dbname=Sys.getenv("dbname"))





####DATA SANITIZATION - PRECHECK
#0 check if orig_name and data_name are in fact new:

  oldmdata <-
    DBI::dbGetQuery(condw,
                    "SELECT * from mdata;")


  existe <- metadf[[1]]$orig_name %in% oldmdata$orig_name |
     metadf[[1]]$data_name %in% oldmdata$data_name

  if(existe & !replace) {
    msg <- "Detectado indicador com mesmo nome na base. O módulo é apenas para adição de novos dados, cheque os nomes caso efetivamente seja novo dado (ou use replace=TRUE para recarregar a série completa do indicador)"
    print(msg)
    return(msg)
  }

  # modelo A (versões de carga): recálculo completo por indicador.
  # Remove, em transação, o mdata existente e REINSERE com o MESMO mdata_id
  # (estável para consumidores), série completa recalculada — pontos antigos
  # revisados pela fonte passam a entrar.
  existing_id <- NA_integer_
  if(existe & replace) {
    existing_id <- oldmdata$mdata_id[oldmdata$orig_name == metadf[[1]]$orig_name][1]
    DBI::dbBegin(condw)
    DBI::dbExecute(condw, "DELETE FROM data_values WHERE mdata_id = $1",
                   params = list(existing_id))
    DBI::dbExecute(condw, "DELETE FROM mdata_timetable WHERE mdata_id = $1",
                   params = list(existing_id))
    DBI::dbExecute(condw, "DELETE FROM mdata_exts WHERE mdata_id = $1",
                   params = list(existing_id))
    DBI::dbExecute(condw, "DELETE FROM mdata WHERE mdata_id = $1",
                   params = list(existing_id))
    # SEM commit aqui: a transação aberta cobre deletes + reinserção +
    # refresh, com rollback automático se qualquer passo falhar
    # (o indicador volta ao estado anterior, íntegro).
    on.exit({ if (DBI::dbIsValid(condw)) { try(DBI::dbRollback(condw), silent = TRUE) } }, add = TRUE)
  }


if(sanitize){
#1 sanitize locals
  print("sanitizing locals")
  ##PROCESS - PAIR LOC NAME WITH LOCAL ID
  locais <- DBI::dbGetQuery(condw,"select local_id,local_name,geoloc_id from local")
  locais <- locais|>dplyr::filter(local_id<6000 | local_id==7087)|>
    dplyr::mutate(geoloc_idc=as.numeric(substr(`geoloc_id`,1,6)))
  idbrasil <- locais[locais$local_name=="Brasil",]$local_id

  if(!is.numeric(datadf$local)) {
    datadf <- datadf|>
    tidyr::separate_wider_delim(local,delim=" ",names=c("geoloc_idc","local_nome"),too_many="merge",too_few="align_end") |>
    dplyr::mutate(geoloc_idc=ifelse(local_nome=="Brasil",idbrasil,geoloc_idc))|>
    dplyr::mutate(across(geoloc_idc,as.numeric))|>
    dplyr::left_join(locais|>dplyr::filter(local_id<5800|local_id==idbrasil),by="geoloc_idc")
  } else {
    if(unique(nchar(datadf$local)==6)) {
      print("local numérico codigo IBGE 6")
      datadf <- datadf|>
        dplyr::left_join(locais,by = c("local"="geoloc_idc"))
    } else {
      datadf <- datadf|>
        dplyr::left_join(locais,by = c("local"="geoloc_id"))
    }
}

#2 sanitize dates
print("sanitizing dates")
if(!lubridate::is.Date(datadf$periodo)) {

    datadf$periodo <- as.Date.character(
      datadf$periodo,tryFormats = c("%Y","%Y-%m","%Y-%m-%d"))

    if(metadf[[2]]$data_freq_id==9) {
      lubridate::month(datadf$periodo) <- 12
      lubridate::day(datadf$periodo) <- 31
    }

}

} else {
  datadf$local_id <- datadf$local
}


###  prepare data for writing

  # replace=TRUE reusa o id antigo; carga nova pega max+1
  newmdataid <- if (!is.na(existing_id)) existing_id else
    1+as.numeric(max(oldmdata$mdata_id))

  addmid <- \(x){cbind(mdata_id=newmdataid,x)}

  mdatanew <- addmid(metadf[[1]])

   mdataextnew <- addmid(metadf[[2]])

   mdatattnew <- addmid(data.frame(
     last_refdate=max(datadf$periodo),
      last_update=Sys.Date()
   ))

   ndv <- addmid(data.frame(
     local_id = datadf$local_id,
     refdate = datadf$periodo,
     value=datadf$valor))


   print(datadf[is.na(datadf$local_id),])

   ndv <- ndv[!is.na(ndv$local_id),]

   # if(forceunique){
   #   ndv <- dplyr::distinct(ndv)
   # }

   DBI::dbAppendTable(condw,"mdata",mdatanew)
   DBI::dbAppendTable(condw,"mdata_exts",mdataextnew)
   DBI::dbAppendTable(condw,"mdata_timetable",mdatattnew)

   DBI::dbAppendTable(condw,"data_values",ndv)


   if(grp){
     dgroupnew <- data.frame(
       datagroup_id= 1+as.numeric(DBI::dbGetQuery(con,"select MAX(mdata_id) from datagroup")),
       datagroup_name= paste0(mdatanew$orig_name,"_grps"),
       datagroup_desc= paste0(mdatanew$data_name," (grupos/classificadores)")
     )


     dgroupingnew <- addmid(datagroup|>dplyr::select(datagroup_id))

     DBI::dbAppendTable(condw,"datagroup",dgroupnew)
     DBI::dbAppendTable(condw,"mdata_group",dgroupingnew)
   }

   DBI::dbExecute(condw,"refresh materialized view named_datavalues;")
   DBI::dbExecute(condw,"refresh materialized view geonamed_datavalues;")
   if (!is.na(existing_id)) DBI::dbCommit(condw)
   DBI::dbDisconnect(condw)
   return("All done!")
}
