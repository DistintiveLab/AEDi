# Planilhas de download da aba "Baixar" do painel ---------------------------------
#
# Construtores puros (sem banco de dados, sem Shiny): recebem data.frames ja
# lidos do DW e gravam o .xlsx — testaveis isoladamente e reusaveis pelo
# esqueleto gerado por deploy_panel(). Layout inspirado no modulo Download do
# labourvaluesdatapanel (save_my_xlsx): bloco de cabecalho, aba de dados com
# painel congelado e aba de metadados.

#' Slug ascii para nomes de arquivo (acentos fora, resto vira hifen)
#' @keywords internal
painel_slug <- function(texto) {
  texto <- trimws(as.character(texto)[1])
  if (is.na(texto) || !nzchar(texto)) return("dados")
  convertido <- tryCatch(
    iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT"),
    error = function(e) texto)
  if (is.na(convertido) || !nzchar(convertido)) convertido <- texto
  convertido <- tolower(convertido)
  convertido <- gsub("[^a-z0-9]+", "-", convertido)
  sub("^-+|-+$", "", convertido)
}

#' Ultima observacao finita de cada chave em cada ano (a mesma regua do mapa,
#' painel_valores_ano: ultimo refdate do ano; NaN/NA/Inf ficam fora e a celula
#' da planilha vem vazia)
#' @keywords internal
painel_ultimo_por_ano <- function(valores, chave) {
  vazio <- data.frame(ano = integer(0), chave = integer(0),
                      valor = numeric(0))
  names(vazio) <- c("ano", chave, "valor")
  if (!is.data.frame(valores) || !nrow(valores) ||
      !all(c(chave, "refdate", "value") %in% names(valores))) return(vazio)
  valores <- valores[is.finite(valores$value), , drop = FALSE]
  if (!nrow(valores)) return(vazio)
  ano <- as.integer(format(valores$refdate, "%Y"))
  ordem <- order(valores[[chave]], ano, valores$refdate)
  saida <- valores[ordem, c(chave, "value"), drop = FALSE]
  saida$ano <- ano[ordem]
  saida <- saida[!duplicated(saida[c(chave, "ano")], fromLast = TRUE), ]
  saida <- saida[, c(chave, "ano", "value")]
  names(saida) <- c(chave, "ano", "valor")
  saida
}

# Bloco de cabecalho de uma aba: pares rotulo (negrito) + valor nas linhas
# iniciais; devolve a linha seguinte a ultima usada
.painel_xlsx_cabecalho <- function(wb, aba, pares) {
  bloco <- data.frame(rotulo = names(pares), valor = unname(pares),
                      stringsAsFactors = FALSE)
  openxlsx::writeData(wb, aba, bloco, startRow = 1, colNames = FALSE)
  negrito <- openxlsx::createStyle(textDecoration = "bold", halign = "right")
  openxlsx::addStyle(wb, aba, rows = seq_along(pares), cols = 1,
                     style = negrito, gridExpand = TRUE)
  length(pares) + 1L
}

.painel_xlsx_estilo_tabela <- function(wb, aba, n_colunas, linha_header,
                                       congelar_col = 0L) {
  header <- openxlsx::createStyle(textDecoration = "bold", halign = "center",
                                  valign = "center")
  openxlsx::addStyle(wb, aba, rows = linha_header, cols = seq_len(n_colunas),
                     style = header, gridExpand = TRUE)
  if (congelar_col > 0L)
    openxlsx::freezePane(wb, aba, firstActiveRow = linha_header + 1L,
                         firstActiveCol = congelar_col + 1L)
  else
    openxlsx::freezePane(wb, aba, firstActiveRow = linha_header + 1L)
  openxlsx::setColWidths(wb, aba, cols = seq_len(n_colunas), widths = "auto")
}

# Aba de aviso para downloads sem dados (o arquivo sempre nasce, fail-visible)
.painel_xlsx_aba_vazia <- function(wb, mensagem) {
  aba <- "dados"
  i <- 1L
  while (aba %in% names(wb)) {
    i <- i + 1L
    aba <- paste0("dados_", i)
  }
  openxlsx::addWorksheet(wb, aba)
  openxlsx::writeData(wb, aba, mensagem, colNames = FALSE)
}

#' Nome de exibicao do indicador (data_name com fallback para orig_name)
#' @keywords internal
painel_nome_indicador <- function(mdata) {
  nome <- ifelse(is.na(mdata$data_name) | !nzchar(mdata$data_name),
                 mdata$orig_name, mdata$data_name)
  trimws(nome)
}

#' Planilha "todos os indicadores de uma regiao": aba "dados" com uma linha
#' por indicador (codigo e nome) e uma coluna por ano — celula = ultima
#' observacao do ano na localidade — e aba "metadados" com a ficha de cada
#' indicador incluido
#'
#' @param arquivo caminho do .xlsx a gravar
#' @param valores data.frame mdata_id/refdate/value de UMA localidade
#'   ([painel_valores_local_todos()])
#' @param mdata data.frame mdata_id/orig_name/data_name/data_desc
#'   ([painel_mdata()])
#' @param local_rotulo,nivel_rotulo rotulos da localidade e do nivel
#'   territorial escolhidos (cabecalho e nome do arquivo)
#' @param titulo titulo do painel (cabecalho)
#' @keywords internal
painel_xlsx_regiao <- function(arquivo, valores, mdata, local_rotulo,
                               nivel_rotulo, titulo = "Painel de Indicadores") {
  wb <- openxlsx::createWorkbook()
  md <- mdata[mdata$mdata_id %in% unique(valores$mdata_id), , drop = FALSE]
  md <- md[order(md$orig_name), , drop = FALSE]
  ultimo <- painel_ultimo_por_ano(valores, "mdata_id")
  if (!nrow(md) || !nrow(ultimo)) {
    .painel_xlsx_aba_vazia(wb, paste0(
      "Sem observacoes para esta localidade no banco de dados do painel ",
      "(confira nivel territorial e localidade)."))
    openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
    return(invisible(arquivo))
  }
  anos <- sort(unique(ultimo$ano))
  matriz <- matrix(NA_real_, nrow = nrow(md), ncol = length(anos),
                   dimnames = list(NULL, as.character(anos)))
  idx <- cbind(match(ultimo$mdata_id, md$mdata_id),
               match(as.character(ultimo$ano), colnames(matriz)))
  matriz[idx] <- ultimo$valor

  openxlsx::addWorksheet(wb, "dados")
  proxima <- .painel_xlsx_cabecalho(wb, "dados", c(
    "Painel" = titulo,
    "Localidade" = local_rotulo,
    "Nível territorial" = nivel_rotulo,
    "Gerado em" = format(Sys.time(), "%Y-%m-%d %H:%M")))
  linha_header <- proxima + 1L
  tabela <- cbind(data.frame(Código = md$orig_name,
                             Indicador = painel_nome_indicador(md),
                             check.names = FALSE), as.data.frame(matriz))
  openxlsx::writeData(wb, "dados", tabela, startRow = linha_header)
  .painel_xlsx_estilo_tabela(wb, "dados", ncol(tabela), linha_header,
                             congelar_col = 2L)

  openxlsx::addWorksheet(wb, "metadados")
  openxlsx::writeData(wb, "metadados", data.frame(
    id = md$mdata_id, Código = md$orig_name,
    Nome = painel_nome_indicador(md), Descrição = md$data_desc,
    check.names = FALSE))
  openxlsx::setColWidths(wb, "metadados", cols = seq_len(4), widths = "auto")
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
  invisible(arquivo)
}

#' Planilha "um indicador por ano": uma aba por ano, cada uma com todas as
#' localidades do nivel territorial com dados naquele ano (codigo, nome e
#' valor da ultima observacao do ano), mais aba "metadados" com o contexto
#'
#' @param arquivo caminho do .xlsx a gravar
#' @param por_ano lista nomeada (ano em texto) de data.frames
#'   local_id/refdate/value JA restritos ao nivel territorial escolhido e JA
#'   na regua de ultima observacao do ano ([painel_valores_ano()])
#' @param locais vetor nomeado local_id -> rotulo do nivel
#'   ([painel_locais_nivel()])
#' @param rotulo_indicador,nivel_rotulo rotulos do indicador e do nivel
#'   territorial escolhidos (cabecalho e nome do arquivo)
#' @param titulo titulo do painel (cabecalho)
#' @param codigos vetor nomeado local_id (texto) -> codigo de exibicao da
#'   localidade (ex.: codigo IBGE dos municipios,
#'   [painel_codigo_mun_cache()]); localidades fora do vetor mantem o
#'   local_id, e a coluna inteira vira texto
#' @keywords internal
painel_xlsx_indicador <- function(arquivo, por_ano, locais,
                                  rotulo_indicador, nivel_rotulo,
                                  titulo = "Painel de Indicadores",
                                  codigos = NULL) {
  wb <- openxlsx::createWorkbook()
  anos <- names(por_ano)[vapply(por_ano, nrow, integer(1)) > 0]
  if (!length(anos)) {
    .painel_xlsx_aba_vazia(wb, paste0(
      "Sem observacoes deste indicador no nivel territorial escolhido ",
      "no banco de dados do painel."))
    openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
    return(invisible(arquivo))
  }
  openxlsx::addWorksheet(wb, "metadados")
  .painel_xlsx_cabecalho(wb, "metadados", c(
    "Painel" = titulo,
    "Indicador" = rotulo_indicador,
    "Nível territorial" = nivel_rotulo,
    "Anos" = paste(range(as.integer(anos)), collapse = " a "),
    "Gerado em" = format(Sys.time(), "%Y-%m-%d %H:%M")))
  abas <- character(0)
  for (ano in sort(as.integer(anos))) {
    v <- por_ano[[as.character(ano)]]
    v <- v[order(v$local_id), , drop = FALSE]
    rotulo <- names(locais)[match(v$local_id, unlist(locais, use.names = FALSE))]
    sem_rotulo <- is.na(rotulo)
    if (any(sem_rotulo)) rotulo[sem_rotulo] <- paste0("local ", v$local_id[sem_rotulo])
    aba <- as.character(ano)
    codigo <- if (is.null(codigos)) v$local_id else {
      traduz <- unname(codigos[as.character(v$local_id)])
      ifelse(is.na(traduz), as.character(v$local_id), traduz)
    }
    openxlsx::addWorksheet(wb, aba)
    openxlsx::writeData(wb, aba, data.frame(
      Código = codigo, Localidade = rotulo, Valor = v$value,
      check.names = FALSE))
    .painel_xlsx_estilo_tabela(wb, aba, 3L, 1L, congelar_col = 0L)
    abas <- c(abas, aba)
  }
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
  invisible(arquivo)
}
