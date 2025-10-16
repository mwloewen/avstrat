test_that("load_geodiva_forms reproduces example_data_strat", {
  # Skip if readxl is not available (so CRAN checks don’t fail)
  skip_if_not_installed("readxl")

  # Locate the example Excel files shipped with the package
  path_samples <- system.file("extdata", "example_samples_stations_upload_2024.xlsx",
                              package = "avstrat")
  path_layers  <- system.file("extdata", "example_layers_upload_2024.xlsx",
                              package = "avstrat")

  # Read them
  station_sample_upload <- readxl::read_xlsx(path_samples, sheet = "Data")
  layer_upload          <- readxl::read_xlsx(path_layers, sheet = "Data")

  # Run your function
  result <- load_geodiva_forms(station_sample_upload, layer_upload)

  # Load the reference dataset
  data("example_data_strat", package = "avstrat")

  # Compare
  expect_equal(result, example_data_strat)
})
