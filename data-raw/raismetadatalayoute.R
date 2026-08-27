### add last layouts from RAIS
custom_recodes <-
  c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" , "ç" = "%E7" , "õ" = "%F5" , "ô" = "%F4" )

lraisurl <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/Layouts/"

lraisurle <-
  RCurl::curlPercentEncode(
    paste0(lraisurl,"estabelecimento/RAIS_estabelecimento_layout2018e2019.xls") ,
    codes = custom_recodes )


download.file(lraisurle,f,method = "curl")

raismetalayoute <- readxl::read_xls(f,skip = 2)

raismetalayoute[117,]$Nome <- "TIPO_ESTAB_1"

usethis::use_data(raismetalayoute,overwrite=T)
