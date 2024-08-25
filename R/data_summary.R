utils::globalVariables(
  c("fk", "geometry", "local_name", "n_pk", "name",
    "nids", "ownpk", "refdate","size", "value"))

quick_data_summary <- function(data) {
  print(dfSummary(data, graph.magnif = 0.8),
        method = 'render',
        headings = FALSE,
        bootstrap.css = FALSE)
}
