# dessoc3 (23): Taxa de Distorcao Idade-Serie (Ensino Fundamental, Total).
# Fonte: educabR::le_idadeserie por ano. Padrao A5b.

suppressMessages(library(educabR))
idade_serie <- \(ano) {
  tryCatch(
    educabR::le_idadeserie(ano) |>
      dplyr::filter(rede == "Total", detalhe %in%
        c("Total_Ensino Fundamental_Total",
          "Total_Taxa de Distorção Idade-Série - Ensino Fundamental_Total Fundamental")) |>
      dplyr::select(ano, codigo_municipio, valor),
    error = function(e) NULL)
}

# anos que a fonte publica; tenta de 2013 ate o ano corrente
anos_tentar <- 2013:as.numeric(format(Sys.Date(), "%Y"))
res <- lapply(anos_tentar, \(a) {
  d <- idade_serie(a)
  if (is.null(d) || !nrow(d)) NULL else { d$ano_num <- a; d }
})
serie <- data.table::rbindlist(res, fill = TRUE)
serie <- serie[!is.na(codigo_municipio) & !is.na(valor)]
serie[, codigo_municipio := as.numeric(codigo_municipio)]
cat("dessoc3: anos", paste(sort(unique(serie$ano_num)), collapse = ","),
    "| municipios:", length(unique(serie$codigo_municipio)), "\n")

AEDi:::gravar_serie_dw("dessoc3",
  data.frame(local = trunc(serie$codigo_municipio / 10),
             periodo = as.Date(paste0(serie$ano_num, "-12-31")),
             valor = as.numeric(serie$valor)))
