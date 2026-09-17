# Worker do download infra4 2025 (DCA-Anexo I-E). Uso:
#   Rscript coleta/infra4_dl2025_worker.R <i> <K>
# processa os codigos com (index mod K) == i. Resumivel.

args <- commandArgs(trailingOnly = TRUE)
wi <- as.integer(args[1]); K <- as.integer(args[2])
suppressMessages({library(siconfir); library(data.table)})

dir <- "coleta/cache/infra4_aedi_s"
codigos <- readRDS("coleta/cache/governativas4_aedi/ifsmparcial.rds")$codmun |>
  unique() |> sort()
meus <- codigos[seq_along(codigos) %% K == wi]
ja <- list.files(dir, pattern = "mun_.*_2025_") |>
  gsub("mun_(.*)_2025.*", "\\1", x = _) |> as.numeric()
todo <- setdiff(meus, ja)
cat("[worker", wi, "]", length(meus), "codigos |", length(todo), "faltam\n")

for (i in seq_along(todo)) {
  cod <- todo[i]
  dest <- file.path(dir, sprintf("mun_%d_2025_annex_dca_i_e.csv", cod))
  if (file.exists(dest)) next
  tryCatch({
    d <- siconfir::get_annual_acc(year = 2025, cod = cod, annex = "DCA-Anexo I-E")
    if (is.null(d) || nrow(d) == 0) {
      file.create(file.path(dir, sprintf("vazio_%d_2025", cod)))
    } else {
      fwrite(d, dest)
    }
    if (i %% 100 == 0) cat(format(Sys.time(), "%H:%M:%S"), "[w", wi, "]", i, "/", length(todo), "\n")
  }, error = function(e) NULL)
  Sys.sleep(0.2)
}
cat("[worker", wi, "] FIM:", length(list.files(dir, pattern = "mun_.*_2025_")), "arquivos total\n")
