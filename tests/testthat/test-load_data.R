

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

#### Tests for load_data_indiv() ####

test_that("load_stratdata_indiv joins and collapses correctly", {

  stations <- tibble::tibble(
    station_id = c("ST01", "ST02"),
    latdd = c(61.1, 61.2),
    longdd = c(-149.9, -150.0)
  )

  sections <- tibble::tibble(
    stratsection_name = c("SEC_A", "SEC_B"),
    station_id = c("ST01", "ST02"),
    stratmeasuremethod = c("order and thickness", "start and stop depth"),
    stratlayer_order_start_at_top = c(TRUE, TRUE)
  )

  layers <- tibble::tibble(
    stratsection_name = c("SEC_A", "SEC_A", "SEC_B"),
    stratlayer_name   = c("L1", "L2", "L3"),
    layer_type        = c("volcanic", "sediment", "lava"),
    grainsize_top     = c("fine sand/ash", "silt", "clay"),
    grainsize_bottom  = c("medium sand/ash", "silt", "silt"),
    stratlayer_order  = c(1, 2, 1)  # for SEC_A
  )

  samples <- tibble::tibble(
    stratlayer_name = c("L1", "L1", "L2", "L3"),
    SampleID        = c("S1", "S2", "S3", "S4")
  )


  result <- load_stratdata_indiv(
    stations_upload = stations,
    sections_upload = sections,
    layers_upload   = layers,
    samples_upload  = samples
  )

  # Basic structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("station_id", "stratsection_name", "stratlayer_name",
                    "stratlayer_sample", "SampleID") %in% names(result)))

  # Check sample collapsing
  l1 <- result[result$stratlayer_name == "L1", ]
  expect_equal(l1$stratlayer_sample, "S1|S2")
  expect_equal(unlist(l1$SampleID), c("S1", "S2"))

  # Check single sample layer
  l3 <- result[result$stratlayer_name == "L3", ]
  expect_equal(l3$stratlayer_sample, "S4")
  expect_equal(unlist(l3$SampleID), "S4")
})

test_that("load_stratdata_indiv handles pre-merged sections", {

  stations <- tibble::tibble(
    station_id = c("ST01", "ST02"),
    latdd = c(61.1, 61.2),
    longdd = c(-149.9, -150.0)
  )

  sections <- tibble::tibble(
    stratsection_name = c("SEC_A", "SEC_B"),
    station_id = c("ST01", "ST02"),
    stratmeasuremethod = c("order and thickness", "start and stop depth"),
    stratlayer_order_start_at_top = c(TRUE, TRUE)
  )

  layers <- tibble::tibble(
    stratsection_name = c("SEC_A", "SEC_A", "SEC_B"),
    stratlayer_name   = c("L1", "L2", "L3"),
    layer_type        = c("volcanic", "sediment", "lava"),
    grainsize_top     = c("fine sand/ash", "silt", "clay"),
    grainsize_bottom  = c("medium sand/ash", "silt", "silt"),
    stratlayer_order  = c(1, 2, 1)  # for SEC_A
  )

  samples <- tibble::tibble(
    stratlayer_name = c("L1", "L1", "L2", "L3"),
    SampleID        = c("S1", "S2", "S3", "S4")
  )

  # Merge sections into layers first
  layers_merged <- dplyr::left_join(layers, sections, by = "stratsection_name")

  result <- load_stratdata_indiv(
    stations_upload = stations,
    sections_upload = sections,
    layers_upload   = layers_merged,
    samples_upload  = samples
  )

  expect_s3_class(result, "data.frame")
  expect_true("stratlayer_sample" %in% names(result))
})

test_that("load_stratdata_indiv warns and replaces on stratlayer_sample mismatch", {
  stations <- tibble::tibble(
    station_id = "ST01",
    latdd = 61.1,
    longdd = -149.9
  )

  sections <- tibble::tibble(
    stratsection_name = "SEC_A",
    station_id = "ST01",
    stratmeasuremethod = "order and thickness",
    stratlayer_order_start_at_top = TRUE
  )

  layers <- tibble::tibble(
    stratsection_name = "SEC_A",
    stratlayer_name   = "L1",
    layer_type        = "volcanic",
    grainsize_top     = "silt",
    grainsize_bottom  = "sand",
    stratlayer_order  = 1,
    # Inject a deliberately wrong stratlayer_sample
    stratlayer_sample = "WRONG"
  )

  samples <- tibble::tibble(
    stratlayer_name = "L1",
    SampleID        = c("S1", "S2")
  )

  expect_warning(
    result <- load_stratdata_indiv(
      stations_upload = stations,
      sections_upload = sections,
      layers_upload   = layers,
      samples_upload  = samples
    ),
    regexp = "Existing stratlayer_sample values differ"
  )

  # After recomputation, the wrong value should be replaced
  expect_equal(result$stratlayer_sample, "S1|S2")
  expect_equal(unlist(result$SampleID), c("S1", "S2"))
})

test_that("example_data_indiv matches example_data_strat on shared columns", {
  indiv_cols <- colnames(example_data_indiv)
  strat_cols <- colnames(example_data_strat)

  # 1. Column names: all indiv columns must exist in strat
  expect_true(all(indiv_cols %in% strat_cols))

  # 2. Same number of rows
  expect_equal(nrow(example_data_indiv), nrow(example_data_strat))

  # 3. Values in each shared column are identical
  for (col in indiv_cols) {
    expect_equal(
      example_data_indiv[[col]],
      example_data_strat[[col]],
      info = paste("Mismatch in column:", col)
    )
  }
})


test_that("extract_sample_depths with remove_layer_metadata = TRUE matches saved fixture", {
  expected <- readRDS(test_path("testdata/expected_samples_min.rds"))
  result <- extract_sample_depths(example_data_strat, remove_layer_metadata = TRUE)

  expect_equal(result, expected)
})

test_that("extract_sample_depths with different column matches saved fixture", {
  expected <- readRDS(test_path("testdata/expected_samples_difcol.rds"))
  result <- extract_sample_depths(
    example_data_strat,
    sample_column = "stratlayer_sample",
    remove_layer_metadata = TRUE
  )

  expect_equal(result, expected)
})
