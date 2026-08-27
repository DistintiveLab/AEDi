# Instalacao do agendamento da atualizacao dos indicadores do AEDi
# (fase C5 do roadmap_aedi_agendamento.md; adaptado do
#  conecta_turismo/setup_inicializacao.R)
#
# Cria a tarefa agendada que roda R/executa_atualizacao.R
# (atualizar_indicadores()) diariamente as 01:00, a partir da raiz do AEDi.
#
# Uso:  source("setup_agendamento.R")   # na raiz do AEDi

horario <- "01:00"
nome_tarefa <- "aedi_atualizacao_indicadores"
caminho_script <- file.path(getwd(), "R", "executa_atualizacao.R")

# o alvo agendado chama atualizar_indicadores() e registra log em coleta/logs
comando <- sprintf(
  'pkgload::load_all("%s"); AEDi:::atualizar_indicadores()',
  getwd())
arquivo_run <- file.path(getwd(), "coleta", "run_agendado.R")
dir.create(dirname(arquivo_run), recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  "# gerado por setup_agendamento.R - execucao agendada",
  comando), arquivo_run)

if (.Platform$OS.type == "windows") {
  if (!requireNamespace("taskscheduleR", quietly = TRUE))
    install.packages("taskscheduleR")
  suppressWarnings(
    tarefas <- taskscheduleR::taskscheduler_ls())
  if (grepl(nome_tarefa, paste(tarefas, collapse = " "))) {
    taskscheduleR::taskscheduler_delete(taskname = nome_tarefa)
  }
  taskscheduleR::taskscheduler_create(
    taskname = nome_tarefa,
    rscript = arquivo_run,
    schedule = "DAILY",
    starttime = horario,
    startdate = format(Sys.Date(), "%d/%m/%Y"))
} else {
  if (!requireNamespace("cronR", quietly = TRUE))
    install.packages("cronR")
  cmd <- cronR::cron_rscript(arquivo_run, workdir = getwd())
  crons <- cronR::cron_ls()
  if (grepl(nome_tarefa, crons, fixed = TRUE)) {
    id <- cronR::cron_get_jobs() |>
      (\(x) attr(x, "job_ids")[grepl(nome_tarefa, x)])()
    if (length(id)) cronR::cron_rm(id, ask = FALSE)
  }
  hhmm <- strsplit(horario, ":")[[1]]
  cronR::cron_add(
    cmd, frequency = "daily",
    at = sprintf("%s:%s:0", hhmm[1], hhmm[2]),
    id = nome_tarefa, tags = "AEDi",
    description = "Atualizacao diaria dos indicadores (coleta/*.R)")
}

cat("Tarefa criada:", nome_tarefa, "as", horario, "\n")
cat("Log padrao: ver funcoes flog em R/executa_atualizacao.R e",
    "coleta/logs/ (cronR salva saidas junto ao job)\n")
