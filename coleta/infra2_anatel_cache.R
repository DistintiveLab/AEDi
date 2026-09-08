# infra2 (17): % de acessos de banda larga fixa em alta velocidade (>34Mbps)
# por municipio. Fonte: serie ja calculada em coleta/cache/infra2_aedi/
# infra2_aedi.csv (metodologia alternativa do upsert.R, base ANATEL
# 2013-2024; mesma serie que foi upsertada no painelpndr dev).
# Recalculo completo (replace). Padrao A5b.

e42 <- data.table::fread("coleta/cache/infra2_aedi/infra2_aedi.csv")

AEDi:::gravar_serie_dw("infra2",
  data.frame(local = e42$geoloc_id,      # 7 digitos direto no geoloc
             periodo = as.Date(e42$refdate),
             valor = e42$infra2_aedi))
