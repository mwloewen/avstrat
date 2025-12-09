test_that("ggstrat runs without error", {
  p <- ggstrat(example_data_strat,
               section_name = "21LSHD02")
  expect_s3_class(p, "ggplot")
})

test_that("ggstrat with decreasing grainsize option runs without error", {
  p <- ggstrat(example_data_strat,
               section_name = "21LSHD02",
               grainsize_direction = "decreasing")
  expect_s3_class(p, "ggplot")
})

test_that("ggstrat with different palette runs without error", {
  p <- ggstrat(example_data_strat,
               section_name = "21LSHD02",
               layer_fill_color = "stratpal_grays")
  expect_s3_class(p, "ggplot")
})

test_that("ggstrat_column runs without error", {
  p <- ggstrat_column(example_data_strat,
                      section_name = "21LSHD02")
  expect_s3_class(p, "ggplot")
})

test_that("ggstrat_label runs without error", {
  p <- ggstrat_label(example_data_strat,
                     section_name = "21LSHD02")
  expect_s3_class(p, "ggplot")
})


test_that("ggstrat_samples runs without error", {
  p <- ggstrat_samples(example_data_strat,
                       section_name = "21LSHD02")
  expect_s3_class(p, "ggplot")
})

test_that("ggstrat layer data is stable for section 21LSHD02", {
  data("example_data_strat", package = "avstrat")

  p <- ggstrat(df = example_data_strat,
               section_name = "21LSHD02",
               grainsize_direction = "increasing")
  result <- ggplot2:::get_layer_data(p, 1)

  ref <- readRDS(test_path("testdata/plot_test_layer_results.rds"))

  expect_equal(result, ref, tolerance = 1e-6)
})


