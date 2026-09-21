# C5: pre-verificacao de novidades na fonte, por FORA dos scripts de coleta.
#
# O script e apenas PARSEADO (nunca executado): localiza-se a primeira
# chamada de baixar/coletar e, por familia de fonte, consulta-se o periodo
# mais recente DISPONIVEL na fonte (catalogo do pacote ou API/HEAD leve) e
# compara-se com o que ja consta no DW (orig_name) ou no artefato de cache.
# Sem novidades -> o lote marca a atualizacao como completa e PULA a coleta
# (ver executar_script_coleta / atualizar_indicadores em executa_atualizacao.R).
#
# Familias suportadas: edubr (ex-educabR; metainep: le_afd, le_idadeserie,
# le_ideb), sidra (API v3 do IBGE) e leitura/download por URL (HEAD/Last-Modified).
# Fonte nao reconhecida, primeira carga ou probe indisponivel -> executa.

#' Mapa funcao edubr (ex-educabR) -> filtro de assunto no metainep (sem acentos)
#' @keywords internal
.assunto_educabr <- c(
  le_afd        = "Adequa",
  le_idadeserie = "Distor",
  le_ideb       = "Ideb"
)

#' Leitores/downloaders cujo 1o argumento string http(s) caracteriza coleta
#' @keywords internal
.leitores_url <- c(
  "readr::read_csv", "readr::read_csv2", "readxl::read_excel",
  "data.table::fread", "utils::read.csv", "utils::read.csv2",
  "read.csv", "read.csv2", "download.file", "httr::GET",
  "curl::curl_download"
)

#' Argumento de uma call por nome (prioridade) ou posicao 1-based do
#' argumento (pos = 1 e o primeiro argumento); NULL se ausente
#' @keywords internal
.arg_call <- function(call, nms = character(), pos = NULL) {
  nms_call <- names(call)[-1]
  for (p in nms) {
    i <- which(nms_call == p)
    if (length(i)) return(call[[i + 1]])
  }
  if (!is.null(pos) && length(call) > pos) return(call[[pos + 1]])
  NULL
}

#' Catalogo metainep do edubr (dataset lazy-data, nao vai em exports)
#' @keywords internal
.metainep_edubr <- function() {
  e <- new.env(parent = emptyenv())
  data("metainep", package = "edubr", envir = e)
  e$metainep
}

#' Classifica uma call como chamada de coleta (educabr/sidra/url).
#' O head pode ser simbolo ("sidra(...)") ou call de namespace
#' ("educabR::le_afd(...)") / acesso ("obj$metodo(...)"), por isso a
#' classificacao e por deparse do nome completo.
#' @keywords internal
.classifica_chamada <- function(call) {
  nome <- deparse(call[[1]])[1]
  if (grepl("^(educabR|edubr)::le_", nome))  # educabR: scripts gerados pre-rename
    return(list(tipo = "educabr", fun = sub("^.*::", "", nome)))
  if (nome %in% c("sidra::sidra", "sidra")) {
    tab <- .arg_call(call, c("tabela", "x"), 1)
    if (!is.null(tab) && (is.numeric(tab) || is.character(tab)))
      return(list(tipo = "sidra", tabela = deparse(tab)[1]))
    return(NULL)
  }
  if (nome %in% .leitores_url) {
    alvo <- .arg_call(call, c("file", "url", "path"), 1)
    if (!is.null(alvo) && is.character(alvo) &&
        grepl("^https?://", alvo[1]))
      return(list(tipo = "url", url = alvo[1]))
  }
  NULL
}

#' Extrai da AST do script a primeira chamada de coleta, sem executa-la.
#' Desce em corpos de funcao e argumentos (ex.: o le_afd dentro da funcao
#' passada a um lapply). Retorna lista(tipo, ...) ou NULL.
#' @keywords internal
.primeira_chamada_coleta <- function(exprs) {
  acha <- function(e) {
    if (!is.call(e)) return(NULL)
    alvo <- .classifica_chamada(e)
    if (!is.null(alvo)) return(alvo)
    for (a in as.list(e)[-1]) {
      r <- if (is.function(a)) Recall(body(a)) else if (is.call(a)) acha(a) else NULL
      if (!is.null(r)) return(r)
    }
    NULL
  }
  for (e in exprs) {
    r <- acha(e)
    if (!is.null(r)) return(r)
  }
  NULL
}

#' Caminho do 1o artefato saveRDS() do script (relativo a raiz), ou NULL
#' @keywords internal
.artefato_cache <- function(exprs) {
  for (e in exprs) {
    if (is.call(e) && !is.call(e[[1]]) &&
        deparse(e[[1]])[1] %in% c("saveRDS", "base::saveRDS")) {
      alvo <- .arg_call(e, "file", 2)
      if (!is.null(alvo) && is.character(alvo) && length(alvo) == 1)
        return(alvo)
    }
  }
  NULL
}

#' Probe INEP/edubr: ultimo ano disponivel no catalogo metainep
#' @keywords internal
.probe_educabr <- function(fun) {
  padrao <- .assunto_educabr[[fun]]
  if (is.na(padrao)) return(NULL)
  meta <- .metainep_edubr()
  meta <- meta[grepl(padrao, meta$assunto), ]
  anos <- suppressWarnings(as.integer(meta$periodo))
  anos <- sort(anos[!is.na(anos)])
  if (!length(anos)) return(NULL)
  list(ultimo = max(anos),
       detalhe = sprintf("INEP %s até %d", fun, max(anos)),
       assinatura = sprintf("inep:%s:%d", fun, max(anos)))
}

#' Probe URL estatica: data de ultima modificacao via HEAD ( Last-Modified )
#' @keywords internal
.probe_url <- function(url) {
  r <- tryCatch(httr::HEAD(url, httr::timeout(15)),
                error = function(e) NULL)
  if (is.null(r)) return(NULL)
  if (httr::status_code(r) >= 400) return(NULL)
  lm <- httr::headers(r)[["last-modified"]]
  if (is.null(lm)) return(NULL)
  d <- strptime(lm, "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  if (is.na(d)) return(NULL)
  list(ultimo = as.POSIXct(d),
       detalhe = sprintf("arquivo alterado em %s", format(as.POSIXct(d))),
       assinatura = paste0("url:", lm))
}

#' Probe SIDRA (API v3 do IBGE): ultimos periodos publicados na tabela.
#' A resposta e um data.frame(id, literals, modificacao) em ordem
#' cronologica; usa-se apenas a coluna id, filtrada por formato valido
#' (AAAA ou AAAAMM) para nao confundir datas internas (ex.: modificacao
#' "01/01/0001") com periodos publicados.
#' @keywords internal
.probe_sidra <- function(tabela) {
  base <- "https://servicodados.ibge.gov.br/api/v3/agregados"
  per <- tryCatch(
    jsonlite::fromJSON(paste0(base, "/", tabela, "/periodos")),
    error = function(e) NULL)
  if (is.null(per)) return(NULL)
  ids <- if (is.data.frame(per)) as.character(per$id) else as.character(per)
  ids <- ids[grepl("^[0-9]{4}([0-9]{2})?$", ids)]
  if (!length(ids)) return(NULL)
  ids <- unique(ids[order(as.integer(ids))])
  list(periodos = ids, ultimo = ids[length(ids)],
       detalhe = sprintf("SIDRA t/%s até %s", tabela, ids[length(ids)]),
       assinatura = paste0("sidra:", tabela, ":", ids[length(ids)]))
}

#' Estado atual do indicador: max(refdate) no DW (orig_name = nome do
#' script) ou, sem registro no DW, no artefato saveRDS do cache
#' @keywords internal
.estado_atual_indicador <- function(nome_script, raiz, con = NULL) {
  dw <- tryCatch({
    if (is.null(con)) {
      con <- AEDi:::controle_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
    }
    d <- AEDi:::resumo_indicadores_dw(con)
    d <- d[d$orig_name == nome_script, ]
    if (nrow(d) && !is.na(d$max_refdate[1])) d$max_refdate[1] else NULL
  }, error = function(e) NULL)
  if (!is.null(dw))
    return(list(refdate_max = as.Date(dw), origem = "DW"))
  arq <- tryCatch(
    .artefato_cache(parse(file.path(raiz, "coleta",
                                    paste0(nome_script, ".R")))),
    error = function(e) NULL)
  if (is.null(arq)) return(NULL)
  arq <- file.path(raiz, arq)
  if (!file.exists(arq)) return(NULL)
  obj <- tryCatch(readRDS(arq), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj) || !"refdate" %in% names(obj))
    return(NULL)
  rd <- suppressWarnings(as.Date(obj$refdate))
  rd <- rd[!is.na(rd)]
  if (!length(rd)) return(NULL)
  list(refdate_max = max(rd), origem = "cache")
}

#' C5: verifica, SEM executar o script, se a fonte tem dados mais recentes
#' do que o que ja consta (DW ou cache). Retorno:
#' list(pular = logical, motivo = character, assinatura = character(1)).
#' Em duvida (fonte desconhecida, primeira carga, probe fora do ar),
#' pular = FALSE, ou seja, executa a coleta.
#' @param nome_script nome do script de coleta (sem .R)
#' @param raiz raiz do projeto AEDi
#' @param con conexao aberta opcional
#' @keywords internal
verificar_novidade_fonte <- function(nome_script, raiz = .aedi_raiz(),
                                     con = NULL) {
  arquivo <- file.path(raiz, "coleta", paste0(nome_script, ".R"))
  if (!file.exists(arquivo))
    return(list(pular = FALSE,
                motivo = "script não encontrado para verificação",
                assinatura = NA_character_))
  exprs <- parse(arquivo)
  ch <- .primeira_chamada_coleta(exprs)
  if (is.null(ch))
    return(list(pular = FALSE,
                motivo = "nenhuma chamada de coleta reconhecida no script",
                assinatura = NA_character_))
  est <- .estado_atual_indicador(nome_script, raiz, con)
  if (is.null(est))
    return(list(pular = FALSE,
                motivo = "sem estado anterior (primeira carga)",
                assinatura = NA_character_))
  ano_est <- as.integer(format(est$refdate_max, "%Y"))

  if (identical(ch$tipo, "educabr")) {
    f <- .probe_educabr(ch$fun)
    if (is.null(f))
      return(list(pular = FALSE,
                  motivo = sprintf("fonte %s sem probe", ch$fun),
                  assinatura = NA_character_))
    return(list(
      pular = !is.na(ano_est) && ano_est >= f$ultimo,
      motivo = sprintf("%s; %s em %d", f$detalhe, est$origem, ano_est),
      assinatura = f$assinatura))
  }

  if (identical(ch$tipo, "url")) {
    f <- .probe_url(ch$url)
    if (is.null(f))
      return(list(pular = FALSE,
                  motivo = "fonte por URL inacessível para verificação",
                  assinatura = NA_character_))
    ctl <- tryCatch(AEDi:::ler_controle(nome_script,
                                        projeto = AEDi:::.nome_projeto(raiz)),
                    error = function(e) NULL)
    inalterado <- !is.null(ctl) && nrow(ctl) == 1 &&
      identical(ctl$status[1], "ok") &&
      identical(ctl$hash_estado[1], f$assinatura)
    return(list(
      pular = inalterado,
      motivo = sprintf("%s; última coleta %s", f$detalhe,
                       if (inalterado) "com o mesmo arquivo" else "antiga"),
      assinatura = f$assinatura))
  }

  if (identical(ch$tipo, "sidra")) {
    f <- .probe_sidra(ch$tabela)
    if (is.null(f))
      return(list(pular = FALSE,
                  motivo = "SIDRA inacessível para verificação",
                  assinatura = NA_character_))
    ultimo_ano <- max(suppressWarnings(as.integer(substr(f$periodos, 1, 4))))
    sem_novidade <- !is.na(ano_est) && ano_est >= ultimo_ano
    return(list(
      pular = sem_novidade,
      motivo = sprintf("%s; %s em %d", f$detalhe, est$origem, ano_est),
      assinatura = f$assinatura))
  }

  list(pular = FALSE, motivo = "familia de fonte sem probe",
       assinatura = NA_character_)
}
