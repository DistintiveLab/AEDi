# educ3 (3): Adequacao da Formacao Docente — % de docentes do Ensino
# Fundamental com formacao adequada (Grupo 1 do AFD/INEP).
# Fonte: educabR::le_afd (metainep atualizado com 2025; download com TLS
# relaxado para o CDN do INEP). Padrao A5b.

pkgload::load_all("/home/borges/pRojetos/educabR", export_all = FALSE)

anos_afd <- as.integer(sort(unique(metainep$periodo[grepl("Adequa", metainep$assunto)])))
cat("edicoes AFD disponiveis:", paste(anos_afd, collapse=","), "\n")

tudo <- data.table::rbindlist(lapply(anos_afd, \(ano) {
  d <- tryCatch(
    le_afd(ano = ano, niveis = "ensino_fundamental", subniveis = "total",
           dependencias = "total", localizacoes = "total",
           cache_dir = "coleta/cache/afd"),
    error = function(e) { cat("ano", ano, "ERRO:", substr(conditionMessage(e),1,50), "\n"); NULL })
  if (is.null(d)) return(NULL)
  Sys.sleep(3)  # pausa entre downloads (CDN do INEP e intermitente)
  d |> dplyr::filter(indicador_afd == "grupo_1", !is.na(codigo_municipio))
}))

serie <- tudo |>
  dplyr::transmute(local = trunc(codigo_municipio / 10),
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = valor)

cat("educ3 AFD:", nrow(serie), "pontos | anos:",
    paste(range(as.numeric(format(unique(serie$periodo), "%Y"))), collapse="-"),
    "| media:", round(mean(serie$valor, na.rm=TRUE), 1), "%\n")

AEDi:::gravar_serie_dw("educ3",
  data.frame(local = serie$local, periodo = serie$periodo, valor = serie$valor))
