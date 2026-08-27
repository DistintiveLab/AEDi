### add last layouts from RAIS
custom_recodes <-
  c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" , "ç" = "%E7" , "õ" = "%F5" , "ô" = "%F4" )

lraisurl <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/Layouts/"
lraisurlv <-
  RCurl::curlPercentEncode(
    paste0(lraisurl,"vínculos/RAIS_vinculos_layout2020.xls") ,
    codes = custom_recodes )


f <- tempfile()
download.file(lraisurlv,f,method = "curl")


raismetalayoutv <- readxl::read_xls(f,skip = 3)

raismetalayoutv[280,]$Nome <- "TIPO_ESTBL_1"

usethis::use_data(raismetalayoutv,overwrite=T)
