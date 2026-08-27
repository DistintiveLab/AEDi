# MIGRADO PARA PACOTE SIDRA
####Pega infos para compor buscas


#input n. tabela
#"https://sidra.ibge.gov.br/t/8694/n1/all/n3/all/v/7167,7168/p/all/c11046/all/d/v7167%205,v7168%205"

filtratabs <- \(x="A70"){
  rc <- substr(x,1,1)
  xa <- substr(x,start=2,nchar(x))
  sidra::tab_agr(x)$tabelas
}

desctabela <- \(tabela=1705){
  sidra::tab_meta(tabela)
}
