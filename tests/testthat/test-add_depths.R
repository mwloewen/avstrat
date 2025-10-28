test_that("add_depths computes correct maximum depths for known sections", {
  data("example_data_strat", package = "avstrat")

  result <- add_depths(example_data_strat)

  # Define expected values
  expected <- c(
    "17KWLCI025" = 326.4,
    "21DVHD01"   = 1232,
    "21DVML05"   = 100,
    "21DVML04"   = 1757,
    "21DVML08"   = 2701.5,
    "21LSHD02"   = 285.3,
    "fake1"      = 84,
    "fake2"      = 84,
    "fake3"      = 84
  )

  # Compute actual max Depth_bottom per section
  actual <- result |>
    dplyr::filter(.data[["stratsection_name"]] %in% names(expected)) |>
    dplyr::group_by(.data[["stratsection_name"]]) |>
    dplyr::summarise(max_depth = max(.data[["Depth_bottom"]]), .groups = "drop") |>
    tibble::deframe()

  # Compare with tolerance for floating point
  expect_equal(actual[names(expected)], expected, tolerance = 1e-6)
})



test_that("missing required columns triggers error", {
  df <- data.frame(stratsection_name = "s1") # missing stratlayer_name, stratmeasuremethod
  expect_error(add_depths(df), "Missing required columns")
})

test_that("rows with stratsection_name but no stratmeasuremethod error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = NA
  )
  expect_error(add_depths(df), "must define stratmeasuremethod")
})

test_that("invalid stratmeasuremethod triggers error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = "nonsense"
  )
  expect_error(add_depths(df), "Invalid stratmeasuremethod")
})

test_that("invalid units trigger error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = "order and thickness",
    stratlayer_order = 1,
    thickness_units = "yards",  # invalid
    thickness_typical = 10
  )
  expect_error(add_depths(df), "Invalid units")
})

test_that("missing depth_top/depth_bottom for start/stop triggers error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = "start and stop depth",
    depth_units = "cm",
    depth_top = NA,
    depth_bottom = 10
  )
  expect_error(add_depths(df), "must define depth_top, depth_bottom")
})

test_that("missing thickness/order for order+thickness triggers error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = "order and thickness",
    stratlayer_order = NA,
    thickness_units = NA,
    thickness_typical = NA,
    thickness_min = NA,
    thickness_max = NA
  )
  expect_error(add_depths(df), "must define at least one of thickness_typical")
})

test_that("duplicate stratlayer_order triggers error", {
  df <- data.frame(
    stratsection_name = c("s1", "s1"),
    stratlayer_name = c("l1", "l2"),
    stratmeasuremethod = "order and thickness",
    stratlayer_order = c(1, 1),  # duplicate
    thickness_units = "cm",
    thickness_typical = c(10, 20),
    stratlayer_order_start_at_top = TRUE
  )
  expect_error(add_depths(df), "Duplicate stratlayer_order")
})

test_that("depth_top >= depth_bottom triggers error", {
  df <- data.frame(
    stratsection_name = "s1",
    stratlayer_name = "l1",
    stratmeasuremethod = "start and stop depth",
    depth_units = "cm",
    depth_top = 20,
    depth_bottom = 10
  )
  expect_error(add_depths(df), "Invalid depth ranges")
})
