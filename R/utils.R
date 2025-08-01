#0 funções uteis
ajam <- \(col){
  str_to_title(format.Date(as.Date(paste0(col,01),"%Y%m%d"),"%b/%Y"))

}

somasna <- \(x){sum(x,na.rm=T)}

maxsna <- \(x){max(x,na.rm=T)}

minsna <- \(x){min(x,na.rm=T)}

mediasna <- \(x){mean(x,na.rm=T)}

percentvar_sna <- \(x){
  valbase <- dplyr::lag(x)
  ifelse(is.na(x)| is.na(valbase),
    NA,
  100*(x-valbase)/valbase)
  }

agregfunc <- c('somasna','mediasna','maxsna','minsna','first','last','percentvar_sna')
names(agregfunc) <-
  c('Soma',
    'Média',
    'Máximo',
    'Mínimo',
    'Primeiro',
    'Último',
    'Var % t-1')

#funções úteis
mmov <- \(x,t=12) {
  round(frollmean(x, t, align = "right", na.rm = T), 2)
}

smov <- \(x,t=12) {
  round(frollsum(x, t, align = "right", na.rm = T), 2)
}

mmov3 <- \(x){
  mmov(x,t=3)
}

smov <- \(x) {
  smov(x,t=3)
}

numvirg <- \(x) {
  format(x,decimal.mark = ",",big.mark=".",nsmall=1,digits = 1,scientific = F)
}

zerovirg <- \(x) {
  format(x,decimal.mark = ",",big.mark=".",scientific = F)
}


#compara período prévio - vetor
pprev <- \(x,t=12) {
  result <- (x/dplyr::lag(x,t))-1
}

##comp acumulado - no ano x 12 meses

compacumulado <- \(x,doze=T,freq=12,coldata="periodo",colval="valor") {
  if (doze) {
  basecomp <- smov(x,freq)

  } else {
    try({
      ano <- lubridate::year(get(paste0("x$",coldata)))
      basecomp <- x|>
        dplyr::group_by(ano)|>
        transmute(aca=cumsum(colval))
      basecomp <- basecomp$aca
    })
  }
  result <- pprev((basecomp/dplyr::lag(basecomp,freq)),freq)
}

