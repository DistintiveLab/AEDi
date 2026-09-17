# Download SICONFI 2025 por municipio (anexo DCA-I-E, despesas por funcao)
# para infra4. Salva em coleta/cache/infra4_aedi_s/mun_<cod>_2025_annex_dca_i_e.csv
# ~5570 municipios; pausa 0.3s = ~30 min. Padrao governativas4_dl2025.R.

suppressMessages({library(siconfir); library(data.table)})

dir <- "coleta/cache/infra4_aedi_s"
codigos <- readRDS("coleta/cache/governativas4_aedi/ifsmparcial.rds")$codmun |>
  unique() |> sort()
ja <- list.files(dir, pattern = "mun_.*_2025_") |>
  gsub("mun_(.*)_2025.*", "\\1", x = _) |> as.numeric()
todo <- setdiff(codigos, ja)
cat("total:", length(codigos), "| ja baixados:", length(ja), "| faltam:", length(todo), "\n")

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
    if (i %% 100 == 0) cat(format(Sys.time(), "%H:%M:%S"), cod, "(", i, "/", length(todo), ")\n")
  }, error = function(e) NULL)
  Sys.sleep(0.3)
}
cat("FIM:", length(list.files(dir, pattern = "mun_.*_2025_")), "arquivos 2025\n")
