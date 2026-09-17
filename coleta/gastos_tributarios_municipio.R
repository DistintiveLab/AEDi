# gastos_tributarios_municipio.R -----------------------------------------
# Fase D (roadmap_gastos_tributarios.md, D3): municipalizacao dos gastos
# tributarios FEDERAIS 2022 (bases efetivas, DGT/RFB) como FATO no DW —
# tabela gasto_tributario_municipio(ano, local_id, grupo, valor).
#
# Porta a rotina de consulta MIDR-IICA (pndr_relatorios,
# 2025-08-Produto-5/dataprep/gastos_tributarios.R) com substituicoes
# documentadas:
#   - demografia do Censo (CSV local)     -> SIDRA v3 tabela 9606 (Censo 2022);
#   - exportacoes comexstat (secoes I/II) -> VAB agropecuaria (SIDRA 5938, var 513);
#   - demonstrativo SIMPLES do BB         -> arrecadacao previdenciaria DARF (RFB);
#   - renuncias PJ da CGU                 -> ADIADA p/ D3b: enquanto isso o grupo
#     "demais_tributos" (II, IRPJ, IPI-I, IPI-V, IOF, PIS, CSLL, COFINS, CIDE,
#     AFRMM, CONDECINE) usa a chave DARF como fallback;
#   - CEBAS/MDS (entidades filantropicas) -> populacao total (SIDRA 9606);
#   - ITR ODS (SIAFI, sheet 2022)         -> itr.xlsx sheet "2020" como chave
#     espacial (zip do autor indisponivel, 404).
#
# Mecanismo (Quadro VII-REG da DGT, bases efetivas 2022, unidade R$ 1,00):
#   alvo_subcat[regiao] = (v_subcat[regiao] / soma das 5 subcats-chave do
#     tributo na regiao) x total do tributo na regiao
#   gt_municipio = alvo[regiao] x prop da chave do municipio dentro da regiao
# As 5 subcats de IRPF (~90% do GT do tributo) e de CPSS (~96%) sao
# renormalizadas para 100%, como na referencia. Municipios ausentes da chave
# nao recebem parcela (comportamento da referencia).
#
# Rodar a partir da raiz do repositorio (pndr_dashboard; copia sincronizada
# em AEDi/coleta). Cache relativo. Conexao DW via env (padrao D2):
# tdbname/userdb/passwddbdev/hostdbdev.
# GOTCHA: .Renviron do pndr_dashboard SOBRESCREVE essas env vars (aponta p/
# o remoto 10.214.50.169). Para gravar no DW LOCAL, prefixe
#   R_ENVIRON_USER=/dev/null env tdbname=aedidb userdb=aedi \
#     passwddbdev='...' hostdbdev=127.0.0.1 Rscript coleta/gastos_tributarios_municipio.R

suppressMessages({
  library(DBI); library(dplyr); library(tidyr); library(readr)
  library(readxl); library(httr); library(jsonlite); library(stringi)
})

ano_gt    <- 2022L
cache_dir <- "coleta/cache/gastos_tributarios"
fontes    <- file.path(cache_dir, "fontes")
dir.create(fontes, recursive = TRUE, showWarnings = FALSE)
options(timeout = 600)

## 0. constantes -----------------------------------------------------------
REGIOES <- c(NORTE = "1", NORDESTE = "2", SUDESTE = "3", SUL = "4",
             `CENTRO-OESTE` = "5")                        # 1o digito do codmun7
COLS_REG <- c("NORTE", "NORDESTE", "CENTRO-OESTE", "SUDESTE", "SUL")  # ordem do Q VII

uf_por_codigo <- c(`11` = "RO", `12` = "AC", `13` = "AM", `14` = "RR",
                   `15` = "PA", `16` = "AP", `17` = "TO", `21` = "MA",
                   `22` = "PI", `23` = "CE", `24` = "RN", `25` = "PB",
                   `26` = "PE", `27` = "AL", `28` = "SE", `29` = "BA",
                   `31` = "MG", `32` = "ES", `33` = "RJ", `35` = "SP",
                   `41` = "PR", `42` = "SC", `43` = "RS", `50` = "MS",
                   `51` = "MT", `52` = "GO", `53` = "DF")

# chaves municipais por grupo (metodo da referencia; substituicoes acima)
#   irpf65                <- Censo 2022: pop 65+ (SIDRA 9606, cat. idade 65+)
#   irpf_molestia         <- BEN: aposentadorias por invalidez (valor R$)
#   irpf_educacao         <- grandes numeros IRPF: deducao despesas instrucao
#   irpf_medicas          <- grandes numeros IRPF: deducao despesas medicas
#   irpf_peculio          <- BEN: pensoes por morte (valor R$)
#   cpss_folha            <- arrecadacao previdenciaria DARF
#   cpss_filantropicas    <- populacao total (ref. usava CEBAS/MDS)
#   cpss_exportacao_rural <- VAB agropecuaria (ref. usava comexstat)
#   cpss_mei              <- arrecadacao MEI: INSS (ref. usava a mesma)
#   cpss_simples          <- arrecadacao previdenciaria DARF (ref. usava BB)
#   irrf                  <- grandes numeros IRPF: imposto pago
#   itr                   <- arrecadacao ITR por municipio (2020, chave espacial)
#   demais_tributos       <- DARF (fallback; CGU no D3b)

tiracento <- \(x) toupper(stri_trans_general(x, "latin-ascii"))

# correcao manual de nomes RFB (atuais) -> grafias da base kelvins (antigas,
# verificadas por grep: kelvins traz "AUGUSTO SEVERO (CAMPO GRANDE)",
# "JANUARIO CICCO (BOA SAUDE)", "AMPARO DO SERRA", "SANT'ANA DO LIVRAMENTO",
# "BRAZOPOLIS", "SAO THOME DAS LETRAS", "OLHO-D'AGUA DO BORGES", "MOGI MIRIM",
# "PASSA-VINTE", "BIRITIBA-MIRIM", "DONA EUSEBIA", "AMPARO DE SAO FRANCISCO"...
# e NAO tem Sao Caitano/PE nem Muquem/GO — esses ficam como AVISO).
# Sem correspondencia (AVISO): SAO CAITANO/PE, MUQUEM DO SAO FRANCISCO/GO
# (kelvins o lista com uf=BA, codigo 2922250 — inconsistente).
corrige_nomes_rfb <- \(d) {
  d |> mutate(municipio = case_when(
    municipio == "ASSU" & uf == "RN" ~ "ACU",
    municipio == "BOA SAUDE" & uf == "RN" ~ "JANUARIO CICCO (BOA SAUDE)",
    municipio == "CAMPO GRANDE" & uf == "RN" ~ "AUGUSTO SEVERO (CAMPO GRANDE)",
    municipio == "BOM JESUS" & uf == "GO" ~ "BOM JESUS DE GOIAS",
    municipio == "AMPARO DA SERRA" ~ "AMPARO DO SERRA",
    municipio == "BALNEARIO DE PICARRAS" ~ "BALNEARIO PICARRAS",
    municipio == "BARAO DO MONTE ALTO" ~ "BARAO DE MONTE ALTO",
    municipio == "BELEM DE SAO FRANCISCO" ~ "BELEM DO SAO FRANCISCO",
    municipio == "BRASOPOLIS" ~ "BRAZOPOLIS",
    municipio == "ELDORADO DOS CARAJAS" ~ "ELDORADO DO CARAJAS",
    municipio == "EMBU" ~ "EMBU DAS ARTES",
    municipio == "IGUARACI" ~ "IGUARACY",
    municipio == "LAGOA DO ITAENGA" ~ "LAGOA DE ITAENGA",
    municipio == "MOGI-MIRIM" ~ "MOGI MIRIM",
    municipio == "MUNHOZ DE MELLO" ~ "MUNHOZ DE MELO",
    municipio == "OLHO D'AGUA DOS BORGES" ~ "OLHO-D'AGUA DO BORGES",
    municipio == "OLHOS-D'AGUA" ~ "OLHOS D'AGUA",
    municipio == "PARATI" ~ "PARATY",
    municipio == "POXOREO" ~ "POXOREU",
    municipio == "PRESIDENTE CASTELO BRANCO" & uf == "SC" ~ "PRESIDENTE CASTELLO BRANCO",
    municipio == "SANTA CRUZ DO MONTE CASTELO" ~ "SANTA CRUZ DE MONTE CASTELO",
    municipio == "SANTA ISABEL DO PARA" ~ "SANTA IZABEL DO PARA",
    municipio == "SANTANA DO LIVRAMENTO" ~ "SANT'ANA DO LIVRAMENTO",
    municipio == "SAO DOMINGOS DE POMBAL" & uf == "PB" ~ "SAO DOMINGOS",
    municipio == "SAO TOME DAS LETRAS" ~ "SAO THOME DAS LETRAS",
    municipio == "SAO VALERIO DA NATIVIDADE" ~ "SAO VALERIO",
    municipio == "TRAJANO DE MORAIS" ~ "TRAJANO DE MORAES",
    municipio == "NOVA DO MAMORE" ~ "NOVA MAMORE",
    municipio == "GRACCHO CARDOSO" ~ "GRACHO CARDOSO",
    municipio == "FLORINEA" ~ "FLORINIA",
    municipio == "TABOCAO" ~ "FORTALEZA DO TABOCAO",
    municipio %in% c("NULL", "BOA ESPERANCA DO NORTE") ~ "NOVA UBIRATA",
    grepl("DO LEVERG", municipio) ~ gsub("DO LE", "DE LE", municipio),
    TRUE ~ municipio))
}

# nome (ja tiracento) + uf -> codigo IBGE 7d na base kelvins
join_ibge <- \(d, base_mun) {
  d <- corrige_nomes_rfb(d)
  d <- d |> group_by(municipio, uf) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  sem <- anti_join(d, base_mun, by = c("municipio", "uf"))
  if (nrow(sem)) cat(sprintf("  AVISO: %d municipios da fonte sem codigo IBGE (%s)\n",
                             nrow(sem), paste(head(sem$municipio, 10), collapse = ", ")))
  inner_join(d, base_mun, by = c("municipio", "uf")) |>
    select(codmun7 = codigo_ibge, where(is.numeric), -codigo_uf)
}

regiao_de <- \(codmun7) names(REGIOES)[match(substr(sprintf("%07d", codmun7), 1, 1), REGIOES)]

## 1. base de municipios (kelvins) + local_id do DW ------------------------
arq_kelvins <- file.path(fontes, "municipios_kelvins.csv")
if (!file.exists(arq_kelvins))
  download.file("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv",
                arq_kelvins, mode = "wb", quiet = TRUE)
base_mun <- read_csv(arq_kelvins, show_col_types = FALSE, progress = FALSE) |>
  transmute(codigo_ibge = as.integer(codigo_ibge), nome, codigo_uf,
            municipio = tiracento(nome), uf = unname(uf_por_codigo[as.character(codigo_uf)])) |>
  filter(!is.na(uf))
stopifnot(nrow(base_mun) >= 5500)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("tdbname"), user = Sys.getenv("userdb"),
                 password = Sys.getenv("passwddbdev"), host = Sys.getenv("hostdbdev"))
local_dw <- dbGetQuery(con,
  "SELECT local_id, geoloc_id::int AS codmun7 FROM local WHERE local_id < 5571")
stopifnot(nrow(local_dw) >= 5500)
sem_dw <- anti_join(base_mun, local_dw, by = c("codigo_ibge" = "codmun7"))
if (nrow(sem_dw)) cat(sprintf("AVISO: %d municipios kelvins sem local_id no DW (%s)\n",
                               nrow(sem_dw), paste(head(sem_dw$municipio, 10), collapse = ", ")))

## 2. fontes: download se ausente ------------------------------------------
baixa <- \(arq, url) {
  if (!file.exists(arq) || file.size(arq) < 1e4) {
    cat("baixando", basename(arq), "...\n")
    ok <- tryCatch({ download.file(url, arq, mode = "wb", quiet = TRUE); TRUE },
                   error = \(e) FALSE, warning = \(w) FALSE)
    if (!ok) stop("falha no download: ", url)
  }
  invisible(arq)
}
f_dgt  <- baixa(file.path(fontes, "dgt_2022.xlsx"),
  "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/relatorios/renuncia/gastos-tributarios-bases-efetivas/dgt-bases-efetivas-2022-serie-2020-a-2025-quadros.xlsx/@@download/file")
f_irpf <- baixa(file.path(fontes, "irpf_gn_2022.xlsx"),
  "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/estudos/imposto-de-renda/estudos-por-ano/grandes-numeros-do-IRPF-2008-a-2023/grandes-numeros-do-irpf-2023-ano-calendario-2022-tabelas/@@download/file")
f_ben  <- baixa(file.path(fontes, "ben_municipios_especie_2022.xlsx"),
  "https://www.gov.br/previdencia/pt-br/assuntos/previdencia-social/arquivos/ben_municipios_especie_2022.xlsx")
f_darf <- baixa(file.path(fontes, "arrecadacao-previdenciaria-por-municipio-2022.xlsx"),
  "https://www.gov.br/receitafederal/pt-br/acesso-a-informacao/dados-abertos/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-das-receitas-previdenciarias/arrecadacao-previdenciaria-por-municipio-2022.xlsx")
f_mei  <- baixa(file.path(fontes, "arrecadacao-do-mei-por-municipio-2015-a-2023.xlsx"),
  "https://www.gov.br/receitafederal/pt-br/acesso-a-informacao/dados-abertos/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-do-mei-por-municipio/arrecadacao-do-mei-por-municipio-2015-a-2023.xlsx")
f_itr  <- file.path(fontes, "itr.xlsx")
if (!file.exists(f_itr))
  stop("ITR obrigatorio: coloque 'itr.xlsx' (arrecadacao ITR por municipio) em ", fontes,
       " — sem URL publica (distribuicao restrita da RFB).")

## 3. SIDRA API v3 (parser validado; UMA classificacao por chamada) ---------
sidra_v3 <- \(tabela, variavel, periodo, classificacao = NULL) {
  arq <- file.path(cache_dir, sprintf("sidra_%s_%s_%s_%s.rds", tabela, variavel,
                                      gsub("[^0-9]", "-", periodo),
                                      gsub("[^0-9]", "", paste(classificacao, collapse = ""))))
  if (file.exists(arq)) return(readRDS(arq))
  url <- sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/periodos/%s/variaveis/%s?localidades=N6[all]",
                tabela, periodo, variavel)
  if (!is.null(classificacao)) url <- paste0(url, "&classificacao=", classificacao)
  # v3: multiplas classificacoes na mesma chamada -> HTTP 500; ao restringir
  # uma, as demais caem em "Total" (comportamento desejado aqui).
  r <- NULL
  for (i in 1:5) {
    r <- tryCatch(GET(url, user_agent("Mozilla/5.0"), timeout(120)), error = \(e) NULL)
    if (!is.null(r) && status_code(r) == 200) break
    cat("  SIDRA indisponivel (tentativa", i, "/5), aguardando 60s...\n"); Sys.sleep(60); r <- NULL
  }
  if (is.null(r)) stop("SIDRA fora do ar e sem cache: ", url)
  res <- fromJSON(content(r, "text"), simplifyVector = TRUE)
  rd <- res$resultados[[1]]
  d <- lapply(seq_len(nrow(rd)), \(j) {
    grp <- paste(na.omit(unlist(rd$classificacoes[[j]]$categoria)), collapse = ";")
    s <- rd$series[[j]]
    sv <- s$serie
    if (is.data.frame(sv)) {  # jsonlite: 1 coluna por periodo, 1 linha por localidade
      data.frame(grupo_idade = grp, codmun7 = as.integer(s$localidade$id),
                 periodo = names(sv)[1],
                 valor = suppressWarnings(as.numeric(sv[[1]])))
    } else {                  # lista de vetores nomeados (ou vetor unico)
      if (!is.list(sv)) sv <- list(sv)
      data.frame(grupo_idade = grp, codmun7 = as.integer(s$localidade$id),
                 periodo = names(sv[[1]])[1],
                 valor = vapply(sv, \(x) suppressWarnings(as.numeric(x[[1]])),
                                numeric(1)))
    }
  }) |> bind_rows() |> filter(!is.na(valor))
  stopifnot(nrow(d) > 0)
  saveRDS(d, arq)
  d
}

## 4. DGT Quadro VII (REG): alvos regionais por grupo ----------------------
qvii <- read_xlsx(f_dgt, sheet = "Q VII (REG)", col_names = FALSE)
hdr_row <- which(tiracento(trimws(as.character(qvii[[1]]))) ==
                 "TRIBUTO / GASTO TRIBUTARIO")[1]
stopifnot(!is.na(hdr_row))
reg_hdr <- tiracento(trimws(as.character(unlist(qvii[hdr_row, 2:6]))))
stopifnot(identical(reg_hdr, COLS_REG))
tot_reg_row <- max(which(tiracento(trimws(as.character(qvii[[1]]))) == "TOTAL"))
# fator de unidade auto: total esperado ~ R$ 492 bi (bases efetivas 2022)
tot_raw <- sum(as.numeric(unlist(qvii[tot_reg_row, 2:6])))
fator <- c(`1e9` = 1e9, `1e6` = 1e6, `1e3` = 1e3, `1` = 1)[
  which.min(abs(tot_raw * c(1e9, 1e6, 1e3, 1) - 492e9))]
cat(sprintf("DGT Q VII: TOTAL bruto %.6g (row %d) -> fator %g => R$ %.2f bi\n",
            tot_raw, tot_reg_row, fator, tot_raw * fator / 1e9))

valores_reg <- \(ii) {
  m <- vapply(ii, \(k) suppressWarnings(as.numeric(unlist(qvii[k, 2:6]))), numeric(5))
  out <- t(m) * fator
  colnames(out) <- COLS_REG
  out
}

sigla_de <- \(lab) {
  lab <- tiracento(lab)
  case_when(
    grepl("^IMPOSTO SOBRE IMPORTACAO", lab) ~ "II",
    grepl("PESSOA FISICA", lab) ~ "IRPF",
    grepl("PESSOA JURIDICA", lab) ~ "IRPJ",
    grepl("RETIDO NA FONTE", lab) ~ "IRRF",
    grepl("PRODUTOS INDUSTRIALIZADOS", lab) & grepl("VINCULADO", lab) ~ "IPI-V",
    grepl("PRODUTOS INDUSTRIALIZADOS", lab) & grepl("INTERN", lab) ~ "IPI-I",
    grepl("OPERACOES FINANCEIRAS", lab) ~ "IOF",
    grepl("^CONTRIBUICAO SOCIAL PARA O PIS", lab) ~ "PIS",
    grepl("LUCRO LIQUIDO", lab) ~ "CSLL",
    grepl("FINANCIAMENTO DA SEGURIDADE", lab) ~ "COFINS",
    grepl("DOMINIO ECONOMICO", lab) ~ "CIDE",
    grepl("MARINHA MERCANTE", lab) ~ "AFRMM",
    grepl("^CONTRIBUICAO PARA O DESENVOLVIMENTO DA INDUSTRIA", lab) ~ "CONDECINE",
    grepl("^CONTRIBUICAO PARA A PREVIDENCIA SOCIAL", lab) ~ "CPSS",
    grepl("TERRITORIAL RURAL", lab) ~ "ITR",
    TRUE ~ NA_character_)
}
SUBCATS <- tribble(
  ~tributo, ~grupo,                   ~padrao,
  "IRPF", "irpf65",                   "65 ANOS OU MAIS",
  "IRPF", "irpf_molestia",            "MOLESTIA",
  "IRPF", "irpf_educacao",            "DESPESAS COM EDUCACAO",
  "IRPF", "irpf_medicas",             "DESPESAS MEDICAS",
  "IRPF", "irpf_peculio",             "SEGURO OU PECULIO",
  "CPSS", "cpss_folha",               "DESONERACAO DA FOLHA",
  "CPSS", "cpss_filantropicas",       "ENTIDADES FILANTROPICAS",
  "CPSS", "cpss_exportacao_rural",    "EXPORTACAO DA PRODUCAO RURAL",
  "CPSS", "cpss_mei",                 "MEI",
  "CPSS", "cpss_simples",             "SIMPLES NACIONAL")

SIGLAS <- c("II","IRPF","IRPJ","IRRF","IPI-I","IPI-V","IOF","PIS","CSLL",
           "COFINS","CIDE","AFRMM","CONDECINE","CPSS","ITR")

linhas <- tibble(i = (hdr_row + 1L):(tot_reg_row - 1L)) |>
  mutate(lab = tiracento(trimws(as.character(qvii[[1]][i]))),
         tem_valor = vapply(i, \(k) {
           v <- suppressWarnings(as.numeric(unlist(qvii[k, 2:6]))); any(!is.na(v))
         }, logical(1))) |>
  filter(lab != "", tem_valor, !lab %in% SIGLAS)  # descarta sigla repetida (ex.: linha 243 "ITR")

trib_seq <- character(nrow(linhas)); blk <- NA_character_
for (k in seq_len(nrow(linhas))) {
  s <- sigla_de(linhas$lab[k])
  if (!is.na(s)) blk <- s
  trib_seq[k] <- blk
}
linhas$tributo_hdr <- vapply(linhas$lab, \(l) sigla_de(l), character(1))
linhas$bloco <- trib_seq

hdr_linhas <- linhas |> filter(!is.na(tributo_hdr))
totais_reg <- bind_cols(tibble(tributo = hdr_linhas$tributo_hdr),
                        as_tibble(valores_reg(hdr_linhas$i)))
stopifnot(nrow(totais_reg) == 15L, !anyDuplicated(totais_reg$tributo),
          setequal(totais_reg$tributo,
                   c("II","IRPF","IRPJ","IRRF","IPI-I","IPI-V","IOF","PIS",
                     "CSLL","COFINS","CIDE","AFRMM","CONDECINE","CPSS","ITR")))
tot_15 <- sum(as.numeric(unlist(totais_reg[COLS_REG])), na.rm = TRUE)
cat(sprintf("DGT: soma dos 15 tributos = R$ %.2f bi (linha TOTAL: R$ %.2f bi)\n",
            tot_15 / 1e9, tot_raw * fator / 1e9))
stopifnot(abs(tot_15 - tot_raw * fator) / (tot_raw * fator) < 0.01)

sc_rows <- list()
for (r in seq_len(nrow(SUBCATS))) {
  hit <- which(is.na(linhas$tributo_hdr) &
                 linhas$bloco == SUBCATS$tributo[r] &
                 grepl(SUBCATS$padrao[r], linhas$lab))
  if (length(hit) != 1L)
    stop(sprintf("subcat '%s': %d linhas no bloco %s (esperava 1)",
                 SUBCATS$grupo[r], length(hit), SUBCATS$tributo[r]))
  sc_rows[[r]] <- tibble(tributo = SUBCATS$tributo[r], grupo = SUBCATS$grupo[r],
                         i = linhas$i[hit])
}
sc_rows <- bind_rows(sc_rows)
sc_mat <- valores_reg(sc_rows$i)
subcats_reg <- bind_cols(sc_rows |> select(tributo, grupo), as_tibble(sc_mat))

alvo_sc <- sc_mat
for (tb in unique(subcats_reg$tributo)) {
  idx <- which(subcats_reg$tributo == tb)
  den <- colSums(sc_mat[idx, , drop = FALSE], na.rm = TRUE)
  tot <- as.numeric(unlist(totais_reg[totais_reg$tributo == tb, COLS_REG]))
  alvo_sc[idx, ] <- sweep(sweep(sc_mat[idx, , drop = FALSE], 2, den, "/"), 2, tot, "*")
}
alvo_sc[!is.finite(alvo_sc)] <- 0

DEMAIS <- c("II", "IRPJ", "IPI-I", "IPI-V", "IOF", "PIS", "CSLL",
            "COFINS", "CIDE", "AFRMM", "CONDECINE")
alvo_demais <- colSums(as.matrix(totais_reg[totais_reg$tributo %in% DEMAIS, COLS_REG]))

alvo_de <- \(grupo) {
  v <- if (grupo %in% subcats_reg$grupo) {
    alvo_sc[match(grupo, subcats_reg$grupo), ]
  } else if (grupo == "irrf") {
    as.numeric(unlist(totais_reg[totais_reg$tributo == "IRRF", COLS_REG]))
  } else if (grupo == "itr") {
    as.numeric(unlist(totais_reg[totais_reg$tributo == "ITR", COLS_REG]))
  } else if (grupo == "demais_tributos") {
    as.numeric(alvo_demais)
  } else stop("grupo sem alvo: ", grupo)
  names(v) <- COLS_REG
  v
}

## 5. chaves municipais por grupo ------------------------------------------
cat("\n== chaves municipais ==\n")

# (a) SIDRA 9606 (Censo 2022): pop 65+ e populacao total
cats65 <- "287[93096,93097,93098,49108,49109,60040,60041,6653]"
ch_irpf65 <- sidra_v3(9606, 93, 2022, cats65) |>
  group_by(codmun7) |> summarise(valor = sum(valor), .groups = "drop")
cat(sprintf("  pop 65+ (9606): %d munis, %.2f mi\n",
            nrow(ch_irpf65), sum(ch_irpf65$valor) / 1e6))

poptotal <- sidra_v3(9606, 93, 2022, "287[100362]") |> select(codmun7, valor)
ch_cpss_filantropicas <- poptotal
cat(sprintf("  pop total (9606): %d munis, %.2f mi\n",
            nrow(poptotal), sum(poptotal$valor) / 1e6))

# (b) SIDRA 5938: VAB agropecuario municipal (var 513; mil R$).
# PIB municipal 2022 ainda inedito na 5938 (valores "..."): usa o ultimo ano
# com dados N6 publicados. A chave e proporcional dentro da regiao, entao o
# ano de referencia nao afeta o metodo.
ch_cpss_exportacao_rural <- NULL
for (ano_vab in c(ano_gt, ano_gt - 1L, ano_gt - 2L)) {
  cand <- tryCatch(sidra_v3(5938, 513, ano_vab), error = \(e) NULL)
  if (!is.null(cand) && nrow(cand) > 0L) {
    ch_cpss_exportacao_rural <- cand |> select(codmun7, valor)
    cat(sprintf("  VAB agro (5938, ano %d): %d munis, R$ %.1f bi\n", ano_vab,
                nrow(ch_cpss_exportacao_rural),
                sum(ch_cpss_exportacao_rural$valor) / 1e6))
    break
  }
}
stopifnot(!is.null(ch_cpss_exportacao_rural))

# (c) BEN: aposentadorias por invalidez / pensoes por morte (Cod. IBGE direto)
ben <- read_xlsx(f_ben, sheet = 3, skip = 7, col_names = FALSE, range = "A8:M5577")
hdr3 <- read_xlsx(f_ben, sheet = 3, skip = 4, col_names = FALSE, n_max = 3)
nmb <- vapply(seq_len(ncol(ben)), \(j) {
  v <- as.character(unlist(hdr3[1:3, j])); v <- v[!is.na(v) & trimws(v) != ""]
  paste(v, collapse = " - ")
}, character(1))
names(ben) <- nmb
col_cod <- nmb[grepl("Cod\\.? IBGE", nmb)][1]
col_inv <- nmb[grepl("Aposentadorias por invalidez", nmb)][1]
col_mor <- nmb[grepl("Pensões por morte", nmb)][1]
stopifnot(!is.na(col_cod), !is.na(col_inv), !is.na(col_mor))
ben_m <- ben |>
  transmute(codmun7 = suppressWarnings(as.integer(trimws(as.character(.data[[col_cod]])))),
            invalidez = suppressWarnings(as.numeric(.data[[col_inv]])),
            morte = suppressWarnings(as.numeric(.data[[col_mor]]))) |>
  filter(!is.na(codmun7))
cat(sprintf("  BEN: %d munis (invalidez R$ %.1f bi, morte R$ %.1f bi)\n",
            nrow(ben_m), sum(ben_m$invalidez, na.rm = TRUE) / 1e9,
            sum(ben_m$morte, na.rm = TRUE) / 1e9))
ch_irpf_molestia <- ben_m |> select(codmun7, valor = invalidez)
ch_irpf_peculio  <- ben_m |> select(codmun7, valor = morte)

# (d) grandes numeros IRPF (Tab13): deducoes + imposto pago
cat("  IRPF Tab13 (join por nome)...\n")
irpf_gn <- read_xlsx(f_irpf, sheet = "Tab13", skip = 1)
stopifnot(all(c("descricao", "deducao_despesa_com_instrucao",
                "deducao_despesas_medicas", "imposto_pago") %in% names(irpf_gn)))
irpf_mun <- irpf_gn |>
  transmute(nome_uf = trimws(as.character(descricao)),
            deducao_despesa_com_instrucao, deducao_despesas_medicas, imposto_pago) |>
  filter(grepl(" - [A-Z]{2}$", nome_uf)) |>
  mutate(uf = sub(".* - ([A-Z]{2})$", "\\1", nome_uf),
         municipio = tiracento(sub(" - [A-Z]{2}$", "", nome_uf))) |>
  join_ibge(base_mun)
cat(sprintf("  IRPF Tab13: %d munis\n", nrow(irpf_mun)))
ch_irpf_educacao <- irpf_mun |> select(codmun7, valor = deducao_despesa_com_instrucao) |>
  filter(!is.na(valor))
ch_irpf_medicas  <- irpf_mun |> select(codmun7, valor = deducao_despesas_medicas) |>
  filter(!is.na(valor))
ch_irrf          <- irpf_mun |> select(codmun7, valor = imposto_pago) |>
  filter(!is.na(valor))

# (e) DARF: arrecadacao previdenciaria por municipio
cat("  DARF (join por nome)...\n")
darf_raw <- read_xlsx(f_darf, sheet = "DARF", skip = 5)
nm_d <- names(darf_raw)
col_mun <- nm_d[grepl("^MUNICIP", tiracento(nm_d))][1]
col_uf  <- nm_d[trimws(toupper(nm_d)) == "UF"][1]
col_val <- nm_d[grepl("ARRECADAC", tiracento(nm_d))][1]
stopifnot(!is.na(col_mun), !is.na(col_uf), !is.na(col_val))
ch_darf <- darf_raw |>
  transmute(municipio = tiracento(as.character(.data[[col_mun]])),
            uf = trimws(as.character(.data[[col_uf]])),
            valor = suppressWarnings(as.numeric(.data[[col_val]]))) |>
  filter(!is.na(valor), !municipio %in% c("EXTERIOR", "TOTAL")) |>
  join_ibge(base_mun)
cat(sprintf("  DARF: %d munis, R$ %.2f bi\n",
            nrow(ch_darf), sum(ch_darf$valor) / 1e9))

# (f) MEI: INSS Simples Nacional MEI 2022
# (aba lida UMA vez inteira: leituras com n_max apar a largura pela linha e
# desalinham header vs dados — header = linhas 3-4, dados = linha 5+)
cat("  MEI (join por nome)...\n")
mei_all <- read_xlsx(f_mei, sheet = 3, col_names = FALSE)
mei <- mei_all[-(1:4), ]
# row 3 traz o ano apenas na 1a coluna de cada trio ICMS/ISS/INSS: propagar
r3 <- as.character(unlist(mei_all[3, ]))
r4 <- as.character(unlist(mei_all[4, ]))
for (j in seq_along(r3))
  if ((is.na(r3[j]) || trimws(r3[j]) == "") && j > 1L) r3[j] <- r3[j - 1L]
nm_m <- ifelse(is.na(r4), r3, paste(r3, r4, sep = " - "))
names(mei) <- nm_m
col_mun <- nm_m[grepl("^MUNICIP", tiracento(nm_m))][1]
col_inss <- nm_m[grepl("2022", nm_m) & grepl("INSS", tiracento(nm_m)) &
                 grepl("MEI", tiracento(nm_m))]
stopifnot(!is.na(col_mun), length(col_inss) == 1L)
ch_cpss_mei <- mei |>
  transmute(nome_uf = tiracento(as.character(.data[[col_mun]])),
            valor = suppressWarnings(as.numeric(.data[[col_inss]]))) |>
  filter(!is.na(valor), grepl(" - [A-Z]{2}$", nome_uf)) |>
  mutate(uf = sub(".* - ([A-Z]{2})$", "\\1", nome_uf),
         municipio = sub(" - [A-Z]{2}$", "", nome_uf)) |>
  join_ibge(base_mun)
cat(sprintf("  MEI: %d munis, R$ %.0f mi\n",
            nrow(ch_cpss_mei), sum(ch_cpss_mei$valor) / 1e6))

# (g) ITR: arrecadacao 2020 como chave espacial (substituicao documentada)
cat("  ITR 2020 (join por nome)...\n")
itr_raw <- read_xlsx(f_itr, sheet = "2020", col_names = FALSE, col_types = "text")
ch_itr <- itr_raw |>
  transmute(nome_uf = tiracento(as.character(...2)),
            valor = suppressWarnings(as.numeric(...3))) |>
  filter(!is.na(valor), valor != 0, grepl(" - [A-Z]{2}$", nome_uf)) |>
  mutate(uf = sub(".* - ([A-Z]{2})$", "\\1", nome_uf),
         municipio = sub(" - [A-Z]{2}$", "", nome_uf)) |>
  join_ibge(base_mun)
cat(sprintf("  ITR: %d munis, R$ %.0f mi\n", nrow(ch_itr), sum(ch_itr$valor) / 1e6))

chaves <- list(
  irpf65                = ch_irpf65,
  irpf_molestia         = ch_irpf_molestia,
  irpf_educacao         = ch_irpf_educacao,
  irpf_medicas          = ch_irpf_medicas,
  irpf_peculio          = ch_irpf_peculio,
  cpss_folha            = ch_darf,
  cpss_filantropicas    = ch_cpss_filantropicas,
  cpss_exportacao_rural = ch_cpss_exportacao_rural,
  cpss_mei              = ch_cpss_mei,
  cpss_simples          = ch_darf,
  irrf                  = ch_irrf,
  itr                   = ch_itr,
  demais_tributos       = ch_darf)

## 6. municipalizacao: alvo regional x prop da chave na regiao -------------
municipaliza <- \(chave, alvo) {
  chave |>
    filter(!is.na(codmun7), is.finite(valor), valor > 0) |>
    mutate(regiao = regiao_de(codmun7)) |>
    filter(!is.na(regiao)) |>
    group_by(regiao) |>
    mutate(prop = valor / sum(valor, na.rm = TRUE)) |>
    ungroup() |>
    mutate(alvo_reg = unname(alvo[regiao])) |>
    transmute(codmun7, valor = prop * alvo_reg) |>
    filter(is.finite(valor))
}

cat("\n== municipalizacao ==\n")
fato <- bind_rows(lapply(names(chaves), \(grupo) {
  gt <- municipaliza(chaves[[grupo]], alvo_de(grupo))
  cat(sprintf("  %-22s %5d munis  R$ %12.2f mi\n",
              grupo, nrow(gt), sum(gt$valor) / 1e6))
  mutate(gt, grupo)
}))

## 7. consolidacao, auditoria e gravacao no DW ------------------------------
tot_mun <- fato |> group_by(codmun7) |>
  summarise(valor = sum(valor), .groups = "drop") |>
  mutate(grupo = "total")
fato <- bind_rows(fato, tot_mun)

resumo <- fato |> group_by(grupo) |>
  summarise(n_mun = n(), valor = sum(valor), .groups = "drop") |>
  arrange(desc(valor))
print(as.data.frame(resumo), digits = 10, row.names = FALSE)
tot_grupos <- sum(resumo$valor[resumo$grupo != "total"])
stopifnot(abs(tot_grupos - tot_15) / tot_15 < 0.01)
cat(sprintf("Total municipalizado (13 grupos): R$ %.2f bi | DGT 15 tributos: R$ %.2f bi\n",
            tot_grupos / 1e9, tot_15 / 1e9))

auditoria <- fato |>
  mutate(regiao = regiao_de(codmun7)) |>
  left_join(base_mun |> transmute(codmun7 = codigo_ibge, nome, uf), by = "codmun7") |>
  select(codmun7, nome, uf, regiao, grupo, valor)
write_csv(auditoria, file.path(cache_dir, "todas_renuncias_municipalizadas_2022.csv"))

total_csv <- auditoria |>
  filter(grupo == "total") |>
  select(-grupo) |>
  left_join(poptotal |> transmute(codmun7, populacao_2022 = valor), by = "codmun7") |>
  mutate(gasto_tributario_per_capita = valor / populacao_2022)
write_csv(total_csv, file.path(cache_dir, "total_gastos_tributarios_mun_2022.csv"))

fato_dw <- fato |>
  transmute(ano = ano_gt, codmun7, grupo, valor) |>
  inner_join(local_dw, by = "codmun7") |>
  transmute(ano, local_id = as.integer(local_id), grupo, valor = round(valor, 2))
sem_local <- anti_join(fato |> distinct(codmun7), local_dw, by = "codmun7")
if (nrow(sem_local)) cat(sprintf("AVISO: %d codmun7 das chaves sem local_id no DW\n",
                                 nrow(sem_local)))
stopifnot(!any(is.na(fato_dw$local_id)))

dbBegin(con)
dbExecute(con, paste("CREATE TABLE IF NOT EXISTS gasto_tributario_municipio (",
                     "ano integer NOT NULL, local_id integer NOT NULL,",
                     "grupo text NOT NULL, valor numeric)"))
dbExecute(con, sprintf("DELETE FROM gasto_tributario_municipio WHERE ano = %d", ano_gt))
dbWriteTable(con, "gasto_tributario_municipio", fato_dw, append = TRUE)
dbCommit(con)
chk <- dbGetQuery(con, paste("SELECT grupo, count(*) AS n, sum(valor) AS soma",
                             "FROM gasto_tributario_municipio WHERE ano = 2022",
                             "GROUP BY grupo ORDER BY soma DESC"))
print(chk, row.names = FALSE)
cat(sprintf("OK: %d linhas gravadas em gasto_tributario_municipio (ano %d)\n",
            nrow(fato_dw), ano_gt))
dbDisconnect(con)
