context("golem tests")

library(golem)

test_that("app ui", {
  ui <- app_ui()
  expect_shinytaglist(ui)
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

# Configure this test to fit your need
test_that(
  "app launches",{
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    path=gsub("\\.Rcheck","",getwd())
    x <- processx::process$new(
      R.home("bin/R"),
      c(
        "-e",
        paste0("sink(file = 'sink.txt', split = TRUE);setwd('",path,"/../../'); pkgload::load_all();run_app()")

      ), stdout="|",stderr="|"
    )

#    Sys.sleep(1)
    print(x$poll_io(-1))
    print(x$read_output_lines())
    Sys.sleep(5)
    print(x$read_error_lines())
    expect_true(x$is_alive())
    x$kill()
  }
)








