# Dependencias entre series do DW e scripts de coleta
# (fase 1 do roadmap_dependencias_orquestrador.md, pndr_coord).
#
# Um script que DERIVA uma serie de outra (DW -> DW, padrao A5b: compostos,
# sincronizacoes _via_aedi -> builder, diferenciais) so deve rodar quando
# os insumos estao pelo menos tao frescos quanto a serie que ele proprio
# grava - foi assim que o objetivo2_3 zerou em 2025 (massa_salarial_municipal
# rodou com o popmun ainda sem 2025-07-01; ver
# pndr_coord/bug_obj23_zeros_2025_2026-09-22.md).
#
# O grafo vive no DW: coluna controle_execucao.dependencias_json (JSONB),
# no formato {"series_proprias": [...],
#             "deps": [{"serie": ..., "regua": ..., "acao": ...}]}.
# A semente versionada fica em <raiz>/coleta/dependencias.csv e e carregada
# com carregar_dependencias(); sem grafo no DW para o script, o runtime cai
# na semente CSV (projeto que ainda nao carregou continua protegido).
#
# Regua de comparacao: POR ANO, nunca data completa (popmun e afins usam
# refdates -07-01 contra -12-31 dos indicadores). Na fase 1 as reguas
# "proprio" e "fonte" aplicam a MESMA regra - ano(dep) >= ano da serie
# propria; a regua "fonte" (ano-alvo da fonte primaria, que pegaria o bug
# original) exige reconhecer RAIS em verificar_novidade_fonte() e fica
# para a fase 1.5. Acao "pular" (default) bloqueia a execucao com skip
# registrado no controle; "avisar" apenas loga em warn e executa (lags
# cronicos legitimos, ex.: sust4/citec4/infra1 em 2024). Em duvida, executa.

#' Valores validos do manifesto (fase 1)
#' @keywords internal
.reguas_dependencia <- c("proprio", "fonte")
.acoes_dependencia  <- c("pular", "avisar")

#' Le e valida o manifesto CSV de dependencias
#' @keywords internal
.ler_manifesto_arquivo <- function(arquivo) {
  tab <- utils::read.csv(arquivo, stringsAsFactors = FALSE,
                         colClasses = "character", encoding = "UTF-8",
                         na.strings = "")
  tab <- as.data.frame(lapply(tab, trimws), stringsAsFactors = FALSE)
  names(tab) <- trimws(names(tab))
  obrigatorias <- c("script", "dep_orig_name", "regua", "acao")
  faltam <- setdiff(obrigatorias, names(tab))
  if (length(faltam))
    stop(sprintf("manifesto de dependencias sem colunas obrigatorias (%s): %s",
                 paste(faltam, collapse = ", "), arquivo), call. = FALSE)
  if (nrow(tab)) {
    tab$script <- sub("\\.R$", "", tab$script, ignore.case = TRUE)
    ruim <- !tab$regua %in% .reguas_dependencia
    if (any(ruim))
      stop(sprintf("regua invalida (%s) no manifesto %s; use %s",
                   paste(unique(tab$regua[ruim]), collapse = ", "), arquivo,
                   paste(.reguas_dependencia, collapse = "|")), call. = FALSE)
    ruim <- !tab$acao %in% .acoes_dependencia
    if (any(ruim))
      stop(sprintf("acao invalida (%s) no manifesto %s; use %s",
                   paste(unique(tab$acao[ruim]), collapse = ", "), arquivo,
                   paste(.acoes_dependencia, collapse = "|")), call. = FALSE)
  }
  tab
}

#' Le a semente de dependencias versionada no projeto
#'
#' Colunas do CSV: `script` (nome sem .R), `dep_orig_name` (serie no mdata),
#' `regua` ("proprio" ou "fonte"), `acao` ("pular" ou "avisar") e
#' `serie_propria` (opcional: series gravadas pelo script quando o parse do
#' arquivo nao as encontra, ex. gravacao via variavel; varios nomes
#' separados por ";". Vale a uniao das linhas do script - basta declarar em
#' uma unica linha). Retorna NULL quando o projeto nao tem manifesto.
#'
#' @param raiz raiz do projeto dono do lote (diretorio com coleta/)
#' @return data.frame do manifesto ou NULL
#' @export
ler_dependencias <- function(raiz = .aedi_raiz()) {
  arquivo <- file.path(raiz, "coleta", "dependencias.csv")
  if (!file.exists(arquivo)) return(NULL)
  .ler_manifesto_arquivo(arquivo)
}

#' Union das series proprias declaradas nas linhas do manifesto do script
#' (coluna serie_propria, ";"-separada)
#' @keywords internal
.series_proprias_manifesto <- function(linhas) {
  declaradas <- unlist(strsplit(linhas$serie_propria[!is.na(linhas$serie_propria) &
                                                        nzchar(linhas$serie_propria)],
                                ";"))
  if (is.null(declaradas)) return(character(0))
  unique(trimws(declaradas[nzchar(trimws(declaradas))]))
}

#' Series gravadas pelo script, detectadas do texto bruto (tolerante a
#' scripts que nao parseiam, no espirito de .preparar_cache_script):
#' chamadas gravar_serie_dw("nome", ...) com o nome como literal, com ou sem
#' namespace AEDi::/AEDi:::. Gravacoes via variavel (compostos) nao sao
#' detectadas - declarar serie_propria no manifesto.
#' @keywords internal
.series_proprias_script <- function(nome_script, raiz = .aedi_raiz()) {
  arquivo <- file.path(raiz, "coleta", paste0(nome_script, ".R"))
  if (!file.exists(arquivo)) return(character(0))
  txt <- paste(readLines(arquivo, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, gregexpr(
    "(AEDi:{2,3})?(gravar_serie_dw|db_datawrite)\\(\\s*[\"']([^\"']+)[\"']",
    txt))[[1]]
  if (!length(m)) return(character(0))
  unique(trimws(sub(".*[\"']([^\"']+)[\"']$", "\\1", m)))
}

#' max(refdate) por orig_name no DW; NA para series ausentes
#' @keywords internal
.frescor_series <- function(orig_names, con = NULL) {
  if (!length(orig_names)) return(setNames(as.Date(NA_character_), character(0)))
  fechar <- is.null(con)
  if (fechar) { con <- AEDi:::controle_con(); on.exit(DBI::dbDisconnect(con)) }
  resumo <- AEDi:::resumo_indicadores_dw(con)
  fr <- setNames(resumo$max_refdate, resumo$orig_name)
  out <- as.Date(unname(fr[orig_names]))
  names(out) <- orig_names
  out
}

#' Decisao PURA (sem DB), testavel: dadas as deps declaradas, o frescor
#' (max refdate; NA = ausente) de cada serie e as series proprias do
#' script, decide pular/avisar. Regra fase 1 (reguas proprio e fonte
#' coincidem): viola quem tem ano(dep) < ano(serie propria), ou dep ausente
#' do DW com serie propria ja existente. Sem serie propria no DW (primeira
#' carga) ou sem deps, executa. A primeira violacao de acao "pular" decide;
#' violacoes "avisar" viram motivo com pular = FALSE.
#' @keywords internal
.avaliar_dependencias <- function(deps, frescor, series_proprias) {
  informar <- function(pular, motivo) list(pular = pular, motivo = motivo)
  if (!nrow(deps) || !length(series_proprias))
    return(informar(FALSE, ""))
  fr_prop <- frescor[series_proprias]
  fr_prop <- fr_prop[!is.na(fr_prop)]
  if (!length(fr_prop))
    return(informar(FALSE, ""))  # primeira carga: nada a proteger
  topo <- names(fr_prop)[which.max(fr_prop)]
  ano_proprio <- as.integer(format(fr_prop[[topo]], "%Y"))
  avisos <- character()
  for (i in seq_len(nrow(deps))) {
    dep <- deps$serie[i]
    f <- frescor[dep]
    if (is.na(f)) {
      viol <- sprintf(paste0("dependencia '%s' ausente do DW enquanto a ",
                             "serie propria '%s' ja existe (%d)"),
                      dep, topo, ano_proprio)
    } else {
      ano_dep <- as.integer(format(f, "%Y"))
      if (ano_dep >= ano_proprio) next
      viol <- sprintf(paste0("dependencia '%s' em %d, anterior a serie ",
                             "propria '%s' (%d)"),
                      dep, ano_dep, topo, ano_proprio)
    }
    if (identical(deps$acao[i], "pular")) return(informar(TRUE, viol))
    avisos <- c(avisos, viol)
  }
  informar(FALSE, paste(avisos, collapse = "; "))
}

#' Padroniza um registro de dependencias (vindo do JSON do DW) para
#' list(deps = data.frame(serie, regua, acao), series_proprias = character);
#' NULL se registro vazio/invalido. Acao desconhecida vira "avisar"
#' (fail-open).
#' @keywords internal
.normalizar_registro <- function(reg) {
  if (is.null(reg)) return(NULL)
  deps <- reg$deps
  if (is.null(deps)) return(NULL)
  pega <- function(o, campo, default) {
    v <- o[[campo]]
    if (is.null(v) || length(v) != 1) default else as.character(v)
  }
  # array JSON de objetos: data.frame (fromJSON simplifica) ou lista; um
  # array com UM objeto pode chegar como lista nomeada sozinha
  df <- if (is.data.frame(deps)) {
    if (!nrow(deps) || !"serie" %in% names(deps)) return(NULL)
    out <- data.frame(serie = as.character(deps$serie),
                      stringsAsFactors = FALSE)
    out$regua <- if ("regua" %in% names(deps)) as.character(deps$regua) else "proprio"
    out$acao  <- if ("acao"  %in% names(deps)) as.character(deps$acao)  else "avisar"
    out
  } else if (is.list(deps)) {
    objs <- if ("serie" %in% names(deps)) list(deps) else deps
    data.frame(serie = vapply(objs, pega, character(1), "serie", NA_character_),
               regua = vapply(objs, pega, character(1), "regua", "proprio"),
               acao  = vapply(objs, pega, character(1), "acao", "avisar"),
               stringsAsFactors = FALSE)
  } else return(NULL)
  df <- df[!is.na(df$serie) & nzchar(df$serie), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$acao[!df$acao %in% .acoes_dependencia] <- "avisar"
  sp <- unlist(reg$series_proprias)
  list(deps = df, series_proprias = if (is.null(sp)) character(0) else as.character(sp))
}

#' Registro de dependencias de um script: DW primeiro (dependencias_json da
#' linha (projeto, nome_script)), CSV-semente como fallback
#' @keywords internal
.registro_dependencias <- function(nome_script, raiz, con = NULL) {
  json <- tryCatch({
    fechar <- is.null(con)
    if (fechar) {
      con <- AEDi:::controle_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
    }
    DBI::dbGetQuery(con,
      "SELECT dependencias_json FROM controle_execucao
        WHERE projeto = $1 AND nome_script = $2",
      params = list(AEDi:::.nome_projeto(raiz), nome_script))
  }, error = function(e) NULL)
  if (!is.null(json) && nrow(json) == 1 &&
      !is.na(json$dependencias_json[1]))
    return(.normalizar_registro(
      jsonlite::fromJSON(json$dependencias_json[1])))
  manifesto <- tryCatch(AEDi:::ler_dependencias(raiz), error = function(e) NULL)
  if (is.null(manifesto)) return(NULL)
  linhas <- manifesto[manifesto$script == nome_script, , drop = FALSE]
  if (!nrow(linhas)) return(NULL)
  list(deps = data.frame(serie = linhas$dep_orig_name,
                         regua = linhas$regua,
                         acao = linhas$acao,
                         stringsAsFactors = FALSE),
       series_proprias = .series_proprias_manifesto(linhas))
}

#' Verifica, SEM executar o script, se as dependencias declaradas permitem
#' a execucao (mesmo contrato de verificar_novidade_fonte(): retorno
#' list(pular, motivo); em duvida, pular = FALSE, ou seja, executa).
#' Series proprias: declaradas no grafo (series_proprias) ou, em falta,
#' detectadas do texto do script.
#'
#' @param nome_script nome do script de coleta (sem .R)
#' @param raiz raiz do projeto dono do lote (diretorio com coleta/)
#' @param con conexao aberta opcional
#' @keywords internal
verificar_dependencias <- function(nome_script, raiz = .aedi_raiz(),
                                   con = NULL) {
  reg <- .registro_dependencias(nome_script, raiz, con)
  if (is.null(reg) || !nrow(reg$deps))
    return(list(pular = FALSE, motivo = ""))
  proprias <- if (length(reg$series_proprias)) reg$series_proprias else
    .series_proprias_script(nome_script, raiz)
  if (!length(proprias))
    return(list(pular = FALSE, motivo = ""))
  frescor <- .frescor_series(unique(c(reg$deps$serie, proprias)), con)
  .avaliar_dependencias(reg$deps, frescor, proprias)
}

#' Define o grafo de dependencias de um script no DW
#'
#' Grava em `controle_execucao.dependencias_json` (UPSERT da linha
#' `(projeto, nome_script)`; `projeto` = basename da raiz).
#'
#' @param nome_script nome do script (sem .R)
#' @param deps data.frame com colunas `serie`, `regua`, `acao` - ou vetor
#'   de nomes de series (assume regua "proprio" e acao "pular")
#' @param series_proprias series gravadas pelo script quando o parse do
#'   arquivo nao as encontra (default: deteccao automatica na verificacao)
#' @param raiz raiz do projeto dono do lote
#' @param con conexao aberta opcional
#' @return invisivel TRUE
#' @export
definir_dependencias <- function(nome_script, deps,
                                 series_proprias = character(0),
                                 raiz = .aedi_raiz(), con = NULL) {
  if (is.character(deps))
    deps <- data.frame(serie = deps,
                       regua = "proprio", acao = "pular",
                       stringsAsFactors = FALSE)
  if (!is.data.frame(deps) ||
      !all(c("serie", "regua", "acao") %in% names(deps)))
    stop("deps deve ser data.frame(serie, regua, acao) ou vetor de nomes",
         call. = FALSE)
  if (nrow(deps)) {
    if (any(!deps$regua %in% .reguas_dependencia))
      stop(sprintf("regua invalida; use %s",
                   paste(.reguas_dependencia, collapse = "|")), call. = FALSE)
    if (any(!deps$acao %in% .acoes_dependencia))
      stop(sprintf("acao invalida; use %s",
                   paste(.acoes_dependencia, collapse = "|")), call. = FALSE)
  }
  payload <- jsonlite::toJSON(
    list(series_proprias = as.character(series_proprias),
         deps = deps[, c("serie", "regua", "acao"), drop = FALSE]),
    auto_unbox = TRUE)
  fechar <- is.null(con)
  if (fechar) { con <- AEDi:::controle_con(); on.exit(DBI::dbDisconnect(con)) }
  DBI::dbExecute(con, "
    INSERT INTO controle_execucao (projeto, nome_script, dependencias_json)
    VALUES ($1, $2, $3::jsonb)
    ON CONFLICT (projeto, nome_script) DO UPDATE
       SET dependencias_json = EXCLUDED.dependencias_json",
    params = list(AEDi:::.nome_projeto(raiz), nome_script,
                  as.character(payload)))
  invisible(TRUE)
}

#' Carrega a semente de dependencias (coleta/dependencias.csv) para o DW
#'
#' Um `definir_dependencias()` por script do manifesto. Depois de carregada,
#' a verificacao em execucao le direto do DW (o CSV segue como fallback e
#' como registro versionado no git do projeto).
#'
#' @param arquivo caminho do CSV (default: `<raiz>/coleta/dependencias.csv`)
#' @param raiz raiz do projeto dono do lote
#' @param con conexao aberta opcional
#' @return invisivel data.frame (script, n_deps, n_series_proprias)
#' @export
carregar_dependencias <- function(arquivo, raiz = .aedi_raiz(), con = NULL) {
  if (missing(arquivo)) arquivo <- file.path(raiz, "coleta", "dependencias.csv")
  if (!file.exists(arquivo))
    stop("manifesto de dependencias nao encontrado: ", arquivo, call. = FALSE)
  tab <- .ler_manifesto_arquivo(arquivo)
  fechar <- is.null(con)
  if (fechar) { con <- AEDi:::controle_con(); on.exit(DBI::dbDisconnect(con)) }
  resumo <- data.frame()
  for (nm in unique(tab$script)) {
    linhas <- tab[tab$script == nm, , drop = FALSE]
    definir_dependencias(
      nm,
      deps = data.frame(serie = linhas$dep_orig_name,
                        regua = linhas$regua, acao = linhas$acao,
                        stringsAsFactors = FALSE),
      series_proprias = .series_proprias_manifesto(linhas),
      raiz = raiz, con = con)
    resumo <- rbind(resumo, data.frame(
      script = nm, n_deps = nrow(linhas),
      n_series_proprias = length(.series_proprias_manifesto(linhas))))
  }
  rownames(resumo) <- NULL
  invisible(resumo)
}
