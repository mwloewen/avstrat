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
