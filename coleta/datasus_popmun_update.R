# datasus_popmun (mdata 66): estimativas populacionais municipais (TABNET/DATASUS)
# A tabela oficial e ano-sufixada (popt2024br.def etc.) e cada release acrescenta
# o ano seguinte ao seletor de periodos; ibge_popt2024br_mun(periodo="last")
# retorna o mais recente. Este script APENAS acrescenta o ultimo refdate
# disponivel (modo append), preservando o historico.

suppressMessages(library(datasus))

# ano do ultimo periodo disponivel na tabela oficial
p <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popt2024br.def")
ano_ref <- max(p |> rvest::html_nodes("#A option") |> rvest::html_text() |>
                 trimws() |> as.numeric(), na.rm = TRUE)

# aviso: enquanto a origem nao publica o ano corrente, todo indicador que usa
# populacao como denominador (objetivo2_2, objetivo2_3, primazia populacional)
# fica sem base no ano corrente -- falha visivel, em vez de zero silencioso.
ano_atual <- as.numeric(format(Sys.Date(), "%Y"))
if (ano_ref < ano_atual)
  warning("datasus_popmun: TABNET ainda nao publica ", ano_atual,
          " (ultimo periodo disponivel: ", ano_ref, ")")

# periodo explicito (o ano que sera gravado): evita depender da ordem do
# seletor em `periodo = "last"` e do rotulo ano_ref divergir do dado trazido
d <- datasus::ibge_popt2024br_mun(periodo = as.character(ano_ref), municipio = "all")
d <- d[d$Município != "TOTAL", ]

serie <- data.frame(
  local = as.numeric(sub("^([0-9]+) .*$", "\\1", d$Município)),
  periodo = as.Date(paste0(ano_ref, "-07-01")),
  valor = as.numeric(d$`População estimada`)
)

stopifnot(nrow(serie) > 5000, abs(sum(serie$valor) - 212e6) < 15e6)

AEDi:::gravar_serie_dw("datasus_popmun", serie, modo = "append")
