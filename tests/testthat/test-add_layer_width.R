test_that("after add_layer_width() and add_depths() plotting-critical columns are stable", {
  data("example_data_strat", package = "avstrat")

  result <- example_data_strat |>
    add_depths() |>
    add_layer_width() |>
    dplyr::select(grainsize, depth, size_loc, stratlayer_order, stratsection_name) |>
    dplyr::arrange(size_loc, stratlayer_order, stratsection_name)

  ref <- readRDS(test_path("testdata/example_width_increasing.rds"))

  expect_equal(
    result[, c("grainsize", "depth")],
    ref[, c("grainsize", "depth")]
  )
})

test_that("after add_layer_width() and add_depths() plotting-critical columns are stable with reverse grainsize", {
  data("example_data_strat", package = "avstrat")

  result <- example_data_strat |>
    add_depths() |>
    add_layer_width(grainsize_direction = "decreasing") |>
    dplyr::select(grainsize, depth, size_loc, stratlayer_order, stratsection_name) |>
    dplyr::arrange(size_loc, stratlayer_order, stratsection_name)

  ref <- readRDS(test_path("testdata/example_width_decreasing.rds"))

  expect_equal(
    result[, c("grainsize", "depth")],
    ref[, c("grainsize", "depth")]
  )
})
