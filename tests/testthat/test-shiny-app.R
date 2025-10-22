test_that("app launches without error", {
  skip_on_cran()
  app <- run_ggstrat_app(example_data_strat)
  expect_s3_class(app, "shiny.appobj")
})
