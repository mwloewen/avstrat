test_that("plotting-critical columns are stable", {
  data("example_data_strat", package = "avstrat")

  result <- example_data_strat |>
    add_depths() |>
    add_layer_width() |>
    dplyr::select(grainsize, depth, size_loc, stratlayer_order, stratsection_name) |>
    dplyr::arrange(size_loc, stratlayer_order, stratsection_name)

  ref <- readRDS(test_path("testdata/example_add_layer_width.rds"))

  expect_equal(
    result[, c("grainsize", "depth")],
    ref[, c("grainsize", "depth")]
  )
})
