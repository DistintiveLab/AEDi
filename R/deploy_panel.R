#' Deploy do painel de indicadores no projeto corrente
#'
#' Materializa uma app Shiny do painel em `diretorio` do projeto onde for
#' chamada (default `painel/`), pronta para hospedar — Shiny Server
#' (aponte o site para o diretorio) ou `rsconnect::deployApp()`
#' (shinyapps.io/Posit Connect) — ou rodar localmente com
#' `shiny::runApp(diretorio)`. Dois modos:
#'
#' - `esqueleto = TRUE` (default): copia **minimamente funcional e
#'   autonoma** do painel — o projeto passa a ser dono do codigo e
#'   adaptar/criar/mudar abas, layout e marca e o esperado. Sao criados
#'   `app.R`, `R/` (modulos das abas, blocos de UI componiveis, camada de
#'   dados `painel_dw.R` e marca configuravel `branding.R`), `www/`
#'   (CSS/JS/logo), um `README.md` de instrucoes e
#'   `esqueleto_manifest.json` (versao do AEDi gerador + SHA-256 de cada
#'   arquivo). A copia nao depende do pacote AEDi em execucao — so das
#'   dependencias comuns (shiny, leaflet, plotly, DBI/RPostgres e
#'   shinyGovBRstyle; ver README gerado).
#' - `esqueleto = FALSE`: app launcher fina — `app.R` de uma linha
#'   chamando [panel_app()] e README. Toda a logica continua no pacote
#'   AEDi instalado, e atualizacoes do painel chegam com o reinstall do
#'   pacote. Ideal quando o projeto nao pretende customizar.
#'
#' Esqueletos ja gerados absorvem melhorias do AEDi com
#' [atualizar_painel()], que preserva arquivos com edicoes locais.
#'
#' @param diretorio caminho do diretorio da app, relativo ao projeto
#'   (default "painel")
#' @param titulo titulo do topbar da app gerada
#' @param paleta paleta inicial da app gerada: `"govbr"` (padrao, azul
#'   Gov.br) ou `"pb"` (preto e branco com o roxo da Distintive); o
#'   visitante pode trocar no botao do topo em qualquer caso
#' @param esqueleto modo de geracao: TRUE (default) copia o esqueleto
#'   adaptavel; FALSE gera so a launcher sobre o pacote AEDi
#' @param sobrescrever substitui arquivos ja existentes no diretorio
#'   (default FALSE)
#'
#' @return caminho absoluto do diretorio da app (invisivel)
#' @export
deploy_panel <- function(diretorio = "painel",
                         titulo = "Painel de Indicadores",
                         paleta = c("govbr", "pb"),
                         esqueleto = TRUE,
                         sobrescrever = FALSE) {
  paleta <- match.arg(paleta)
  if (isTRUE(esqueleto))
    .deploy_panel_esqueleto(diretorio, titulo, paleta, sobrescrever)
  else
    .deploy_panel_launcher(diretorio, titulo, paleta, sobrescrever)
  invisible(normalizePath(diretorio))
}

# Substitui %%NOME%% por valor em um vetor de linhas (fixed = TRUE)
.gsub_placeholders <- function(linhas, valores) {
  for (n in names(valores))
    linhas <- gsub(paste0("%%", n, "%%"), valores[[n]], linhas,
                   fixed = TRUE, useBytes = TRUE)
  linhas
}

# Escreve texto em caminho relativo ao diretorio da app (UTF-8)
.gravar_utf8 <- function(diretorio, rel, linhas) {
  f <- file.path(diretorio, rel)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines(linhas, f, useBytes = TRUE)
  rel
}

# Resolve todo o conteudo canonico do esqueleto a partir dos templates
# instalados: arquivos-texto (lista nomeada rel -> linhas, placeholders ja
# substituidos) e assets binarios (caminhos absolutos). Unica fonte para
# [deploy_panel()] e [atualizar_painel()].
.fontes_esqueleto <- function(titulo, paleta) {
  tpl_dir <- system.file("painel_esqueleto", package = "AEDi")
  if (!nzchar(tpl_dir))
    stop("templates do esqueleto ausentes nesta instalacao do AEDi — ",
         "reinstale o pacote")
  ler_tpl <- function(rel)
    readLines(file.path(tpl_dir, rel), encoding = "UTF-8", warn = FALSE)
  exatas <- c("R/branding.R" = "painel_branding.R",
              "R/painel_basemap.R" = "painel_basemap.R",
              "R/painel_ui.R" = "painel_ui.R",
              "R/painel_cache.R" = "painel_cache.R",
              "R/painel_dw.R" = "painel_dw.R",
              "R/painel_xlsx.R" = "painel_xlsx.R",
              "R/mod_panel_globe.R" = "mod_panel_globe.R",
              "R/mod_panel_map.R" = "mod_panel_map.R",
              "R/mod_panel_regiao.R" = "mod_panel_regiao.R",
              "R/mod_panel_baixar.R" = "mod_panel_baixar.R",
              "R/mod_panel_sobre.R" = "mod_panel_sobre.R")
  conteudo <- list()
  # app.R e README (texto puro)
  valores_txt <- c(VERSAO = as.character(utils::packageVersion("AEDi")),
                   DATA = format(Sys.Date(), "%Y-%m-%d"), TITULO = titulo)
  for (rel in c("app.R", "README.md"))
    conteudo[[rel]] <- .gsub_placeholders(ler_tpl(rel), valores_txt)
  # UI com defaults de marca (valores como literais R)
  valores_r <- c(TITULO = deparse(titulo), PALETA = deparse(paleta),
                 SUBTITULO = deparse("AEDi — banco de dados do painel"))
  conteudo[["R/app_ui.R"]] <- .gsub_placeholders(
    ler_tpl("R/app_ui.R"), valores_r)
  # copias exatas dos fontes compartilhados com o pacote + server
  for (rel in c(names(exatas), "R/app_server.R"))
    conteudo[[rel]] <- ler_tpl(rel)
  assets <- list.files(system.file("painel", package = "AEDi"),
                       full.names = TRUE)
  logo <- system.file("app", "www", "aedi-Wide.png", package = "AEDi")
  list(conteudo = conteudo, assets = assets, logo = logo)
}

# sha256 de um vetor de linhas como se gravado por .gravar_utf8()
.sha_linhas <- function(linhas) {
  f <- tempfile(fileext = ".aedi")
  on.exit(unlink(f), add = TRUE)
  writeLines(linhas, f, useBytes = TRUE)
  digest::digest(f, file = TRUE, algo = "sha256")
}

# hashes do manifest como vetor nomeado (aceita lista ou data.frame)
.manifest_hashes <- function(arquivos) {
  if (is.data.frame(arquivos))
    return(stats::setNames(as.character(arquivos$sha256),
                           rownames(arquivos)))
  stats::setNames(vapply(arquivos, \(x) x$sha256, character(1)),
                  names(arquivos))
}

# Parse-check de todo .R da app; fatal = TRUE aborta (deploy), FALSE so
# avisa (update — erro de sintaxe em arquivo preservado e do projeto)
.verificar_parse <- function(diretorio, fatal = TRUE) {
  rels_r <- file.path("R", list.files(file.path(diretorio, "R"),
                                      pattern = "[.]R$"))
  for (rel in c("app.R", rels_r)) {
    tryCatch(parse(file.path(diretorio, rel)),
             error = function(e) {
               msg <- paste0("arquivo nao parseia (", rel, "): ",
                             conditionMessage(e))
               if (fatal) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
             })
  }
}

# Grava o manifest com o vetor nomeado rel -> sha256 fornecido
.gravar_manifest <- function(diretorio, titulo, paleta, hashes) {
  manifest <- list(
    gerador = "AEDi::deploy_panel",
    versao_aedi = as.character(utils::packageVersion("AEDi")),
    data = format(Sys.Date(), "%Y-%m-%d"),
    titulo = titulo,
    paleta = paleta,
    arquivos = stats::setNames(lapply(hashes, \(h) list(sha256 = h)),
                               names(hashes)))
  .gravar_utf8(diretorio, "esqueleto_manifest.json",
               jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE,
                                null = "null"))
}

#' @keywords internal
.deploy_panel_esqueleto <- function(diretorio, titulo, paleta, sobrescrever) {
  versao <- as.character(utils::packageVersion("AEDi"))
  f <- .fontes_esqueleto(titulo, paleta)
  previstos <- c(names(f$conteudo),
                 file.path("www", basename(c(f$assets, f$logo))))

  existentes <- previstos[file.exists(file.path(diretorio, previstos))]
  if (length(existentes) && !isTRUE(sobrescrever))
    stop(length(existentes), " arquivo(s) ja existem em ",
         normalizePath(diretorio),
         " (use sobrescrever = TRUE): ",
         paste(existentes, collapse = ", "))

  gravados <- character()
  for (rel in names(f$conteudo))
    gravados <- c(gravados, .gravar_utf8(diretorio, rel, f$conteudo[[rel]]))
  dir.create(file.path(diretorio, "www"), recursive = TRUE,
             showWarnings = FALSE)
  ok <- file.copy(c(f$assets, f$logo), file.path(diretorio, "www"),
                  overwrite = TRUE)
  gravados <- c(gravados, file.path("www", basename(c(f$assets, f$logo))[ok]))

  .verificar_parse(diretorio)
  .gravar_manifest(
    diretorio, titulo, paleta,
    stats::setNames(
      vapply(gravados, \(rel) digest::digest(file.path(diretorio, rel),
                                             file = TRUE, algo = "sha256"),
             character(1)), gravados))

  cat("Esqueleto do painel criado em", normalizePath(diretorio),
      "(AEDi", versao, "-", length(gravados), "arquivos + manifest).\n",
      "Codigo do projeto: adapte R/app_ui.R, R/mod_*.R e R/branding.R",
      "a vontade — o esqueleto nao depende do AEDi em execucao.\n")
}

#' Atualiza um esqueleto de painel ja gerado, preservando edicoes locais
#'
#' Companheira de [deploy_panel()] com `esqueleto = TRUE`: quando uma
#' versao nova do AEDi traz melhorias no painel (bug fix de modulo, novo
#' asset), `atualizar_painel()` absorve essas mudancas no esqueleto ja
#' materializado no projeto **sem destruir o que o projeto adaptou**.
#'
#' O mecanismo usa o `esqueleto_manifest.json` gerado junto com a app:
#' para cada arquivo previsto nesta versao,
#'
#' - **intacto** (hash local igual ao hash do manifest) — substituido
#'   pela versao nova do AEDi;
#' - **modificado localmente** — preservado como esta e reportado no
#'   fim (o hash upstream esperado continua no manifest, entao a
#'   divergencia segue detectavel na proxima atualizacao); mescle as
#'   mudancas manualmente ou repontue com `forcar = TRUE`;
#' - **novo nesta versao** — adicionado; arquivo previsto antes e
#'   retirado do esqueleto — apenas reportado, nunca apagado.
#'
#' `forcar = TRUE` substitui inclusive os arquivos com edicoes locais
#' (destructivo — descarta as adaptacoes do projeto).
#'
#' @param diretorio caminho do diretorio da app (default "painel")
#' @param forcar substitui tambem arquivos modificados localmente
#'   (default FALSE)
#'
#' @return caminho absoluto do diretorio da app (invisivel)
#' @export
atualizar_painel <- function(diretorio = "painel", forcar = FALSE) {
  manifest_path <- file.path(diretorio, "esqueleto_manifest.json")
  if (!file.exists(manifest_path))
    stop("sem esqueleto_manifest.json em ", diretorio,
         " — atualizar_painel() vale para app gerada com ",
         "deploy_panel(esqueleto = TRUE)")
  m_antigo <- jsonlite::fromJSON(manifest_path)
  hash_antigo <- .manifest_hashes(m_antigo$arquivos)
  versao_antiga <- as.character(m_antigo$versao_aedi)
  versao <- as.character(utils::packageVersion("AEDi"))

  f <- .fontes_esqueleto(m_antigo$titulo, m_antigo$paleta)
  copiar <- stats::setNames(
    as.list(c(f$assets, f$logo)),
    file.path("www", basename(c(f$assets, f$logo))))
  todos <- c(names(f$conteudo), names(copiar))

  # fonte upstream de cada arquivo: linhas em memoria ou asset binario
  hash_upstream <- function(rel) {
    if (rel %in% names(f$conteudo)) .sha_linhas(f$conteudo[[rel]])
    else digest::digest(copiar[[rel]], file = TRUE, algo = "sha256")
  }
  gravar_fonte <- function(rel) {
    if (rel %in% names(f$conteudo))
      .gravar_utf8(diretorio, rel, f$conteudo[[rel]])
    else {
      dir.create(file.path(diretorio, dirname(rel)), recursive = TRUE,
                 showWarnings = FALSE)
      file.copy(copiar[[rel]], file.path(diretorio, rel),
                overwrite = TRUE)
      rel
    }
  }

  # nada a fazer se a versao e o conteudo upstream nao mudaram (forcar
  # sempre roda: seu proposito e repontar edicoes locais)
  hashes_upstream <- stats::setNames(vapply(todos, hash_upstream,
                                            character(1)), todos)
  if (!isTRUE(forcar) && identical(versao_antiga, versao) &&
      setequal(todos, names(hash_antigo)) &&
      all(vapply(names(hash_antigo),
                 \(rel) identical(hashes_upstream[[rel]],
                                  unname(hash_antigo[[rel]])),
                 logical(1)))) {
    cat("Esqueleto do painel em", normalizePath(diretorio),
        "ja esta na versao mais recente do AEDi (", versao, ").\n")
    return(invisible(normalizePath(diretorio)))
  }

  hashes_novo <- stats::setNames(character(0), character(0))
  atualizados <- preservados <- adicionados <- character(0)
  for (rel in todos) {
    caminho <- file.path(diretorio, rel)
    if (!file.exists(caminho) || !rel %in% names(hash_antigo)) {
      # ausente em disco ou fora do manifest anterior (ex.: propagacao
      # anterior interrompida deixou arquivo sem registro): sem edicao
      # local rastreavel, vale o upstream como arquivo adicionado
      gravar_fonte(rel)
      adicionados <- c(adicionados, rel)
      hashes_novo[[rel]] <- digest::digest(caminho, file = TRUE,
                                           algo = "sha256")
      next
    }
    hash_local <- digest::digest(caminho, file = TRUE, algo = "sha256")
    hash_registrado <- unname(hash_antigo[[rel]])
    intacto <- !is.na(hash_registrado) &&
      identical(hash_local, hash_registrado)
    if (intacto || isTRUE(forcar)) {
      gravar_fonte(rel)
      atualizados <- c(atualizados, rel)
      hashes_novo[[rel]] <- digest::digest(caminho, file = TRUE,
                                           algo = "sha256")
    } else {
      preservados <- c(preservados, rel)
      # mantem o hash upstream esperado: a edicao local continua sendo
      # detectada (e preservada) na proxima atualizacao
      hashes_novo[[rel]] <- hash_registrado
    }
  }
  removidos <- setdiff(names(hash_antigo), todos)

  .verificar_parse(diretorio, fatal = FALSE)
  .gravar_manifest(diretorio, m_antigo$titulo, m_antigo$paleta, hashes_novo)

  cat("Esqueleto do painel em", normalizePath(diretorio), "atualizado",
      "para o AEDi", versao, ":\n",
      "  -", length(atualizados), "arquivo(s) atualizados;",
      length(adicionados), "adicionado(s)\n")
  if (length(preservados))
    cat("  - EDICOES LOCAIS PRESERVADAS (", length(preservados),
        "): ", paste(preservados, collapse = ", "), "\n",
        "    mescle manualmente as mudancas do upstream ou reponte",
        "com atualizar_painel(forcar = TRUE)\n", sep = "")
  if (length(removidos))
    cat("  - removidos do esqueleto nesta versao (mantidos",
        "localmente):", paste(removidos, collapse = ", "), "\n")
  invisible(normalizePath(diretorio))
}

#' @keywords internal
.deploy_panel_launcher <- function(diretorio, titulo, paleta, sobrescrever) {
  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  app_r <- file.path(diretorio, "app.R")
  readme <- file.path(diretorio, "README.md")
  if (file.exists(app_r) && !isTRUE(sobrescrever))
    stop("app.R ja existe em ", normalizePath(diretorio),
         " — use sobrescrever = TRUE para substituir")
  writeLines(c(
    "# Painel de indicadores do banco de dados do painel — app gerada por AEDi::deploy_panel()",
    "# Credenciais do banco de dados (variaveis de ambiente): user, password, host, dbname",
    sprintf("AEDi::panel_app(titulo = %s, paleta = %s)",
            deparse(titulo), deparse(paleta)),
    ""), app_r)
  writeLines(c(
    "# Painel de indicadores",
    "",
    "App Shiny autonoma de consulta ao banco de dados do painel (series por nivel",
    "territorial, mapa municipal dinamico com slider de ano animado e globo",
    "interativo das UFs), gerada por `AEDi::deploy_panel()` e",
    "montada sobre o pacote AEDi. A paleta de cores pode ser Gov.br (azul) ou",
    "preto e branco com o roxo da Distintive — o botao no topo troca a qualquer",
    "momento e a escolha fica salva no navegador.",
    "",
    "## Rodar localmente",
    "",
    "```r",
    'shiny::runApp("painel")',
    "```",
    "",
    "## Hospedar",
    "",
    "- Shiny Server: aponte a configuracao do site para este diretorio;",
    "- shinyapps.io / Posit Connect:",
    "",
    "```r",
    'rsconnect::deployApp("painel")',
    "```",
    "",
    "## Credenciais do banco de dados",
    "",
    "A conexao usa as variaveis de ambiente `user`, `password`, `host` e",
    "`dbname` (defaults: aedi@127.0.0.1/aedidb). Em hospedagem, configure-as",
    "no painel da plataforma ou no `.Renviron` lido pelo processo do Shiny.",
    "",
    "Requisito: pacote AEDi instalado",
    "('remotes::install_github(\"DistintiveLab/AEDi\")').",
    ""), readme)
  cat("App launcher do painel criada em", normalizePath(diretorio), "\n")
}
