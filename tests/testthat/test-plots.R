test_that("ggstrat layer data is stable for section 21LSHD02", {
  data("example_data_strat", package = "avstrat")

  p <- ggstrat(df = example_data_strat, stratsection_name = "21LSHD02", grainsize_direction = "increasing")
  result <- ggplot2::get_layer_data(p, 1)

  ref <- readRDS(test_path("testdata/plot_test_layer_results.rds"))

  expect_equal(result, ref, tolerance = 1e-6)
})
