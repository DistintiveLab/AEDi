

educ4_orig <- DBI::dbGetQuery(mdr,"select
                                refdate,local_id,
                              value from data_values a
                              left join mdata c on a.mdata_id = c.mdata_id where orig_name LIKE 'educ4%'")

educ4_aedi <- DBI::dbGetQuery(
  con,
  "select refdate,local_id,value educ4_aedi from
  data_values a left join mdata b on
  a.mdata_id = b.mdata_id where orig_name LIKE
   'ideb_media_basico_redep%'")

#data.table::fwrite(educ4_aedi,'coleta/cache/educ4_aedi/educ4_aedi.csv')
educ4_compara <-
  educ4_orig|>
  dplyr::left_join(educ4_aedi)

educ4_compara <-
  educ4_compara|>dplyr::rename(educ4_base=value)

cor(educ4_compara$educ4_base,educ4_compara$educ4_aedi,use='complete.obs')

summary(educ4_compara)
