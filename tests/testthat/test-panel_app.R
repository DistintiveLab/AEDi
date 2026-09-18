test_that("panel_app constroi a app em ambas as paletas", {
  app <- panel_app()
  expect_s3_class(app, "shiny.appobj")

  app_pb <- panel_app(paleta = "pb")
  expect_s3_class(app_pb, "shiny.appobj")

  expect_error(panel_app(paleta = "x"), "govbr")
})

test_that("run_panel define a porta e propaga titulo e paleta", {
  app <- run_panel(port = 4599, titulo = "Meu Painel", paleta = "pb")
  expect_s3_class(app, "shiny.appobj")
  expect_identical(app$options$port, 4599L)
})
