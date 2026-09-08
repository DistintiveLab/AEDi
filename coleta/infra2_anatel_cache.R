# infra2 (17) 2025: % acessos banda larga fixa em alta velocidade (>34Mbps).
# Replica a metodologia do basecalc (ANATEL, todos os tipos de pessoa) sobre
# o CSV Acessos_Banda_Larga_Fixa_2025.csv ja em cache. Append do refdate
# 2025-12-31. Padrao A5b.

suppressMessages(library(data.table))
d <- fread("coleta/cache/infra2_aedi/Acessos_Banda_Larga_Fixa_2025.csv",
           encoding = "UTF-8")
codcol <- grep("IBGE", names(d), value = TRUE)[1]
faicol <- grep("Faixa", names(d), value = TRUE)[1]

dd <- d[, .(acessos = sum(Acessos),
            alta = sum(Acessos[get(faicol) == "> 34Mbps"])),
        by = .(codigo_ibge_municipio = as.numeric(get(codcol)))]
dd[, valor := 100 * alta / acessos]
dd <- dd[!is.na(codigo_ibge_municipio)]
stopifnot(nrow(dd) > 5000)

AEDi:::gravar_serie_dw("infra2",
  data.frame(local = dd$codigo_ibge_municipio,
             periodo = as.Date("2025-12-31"),
             valor = dd$valor),
  modo = "append")
