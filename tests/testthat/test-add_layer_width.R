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

test_that("invalid depth ranges trigger error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    Depth_top = 20,
    Depth_bottom = 10,
    grainsize_top = "silt",
    grainsize_bottom = "silt"
  )
  expect_error(add_layer_width(df), "Invalid depth ranges")
})

test_that("invalid grain size triggers error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    Depth_top = 0,
    Depth_bottom = 10,
    grainsize_top = "sand",   # not in valid list
    grainsize_bottom = "silt"
  )
  expect_error(add_layer_width(df), "Invalid grain size")
})

test_that("valid grain sizes pass", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    Depth_top = 0,
    Depth_bottom = 10,
    grainsize_top = "silt",
    grainsize_bottom = "fine sand/ash"
  )
  expect_silent(add_layer_width(df))
})
