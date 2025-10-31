## code to prepare `data-and-test-data` dataset goes here

#### Build example data set from upload form ####

# Load packages
library(readxl)
library(avstrat)

# Locate the example Excel files shipped with the package
path_samples <- system.file("extdata", "example_samples_stations_upload_2024.xlsx",
                            package = "avstrat"
)
path_layers <- system.file("extdata", "example_layers_upload_2024.xlsx",
                           package = "avstrat"
)

# Read them with readxl (only if readxl is available)
if (requireNamespace("readxl", quietly = TRUE)) {
  station_sample_upload <- readxl::read_xlsx(path_samples, sheet = "Data")
  layer_upload <- readxl::read_xlsx(path_layers, sheet = "Data")

  example_data_strat <- load_geodiva_forms(station_sample_upload, layer_upload)
}

usethis::use_data(example_data_strat, overwrite = TRUE)

#### Build example data set for add_layer_width ####
library(avstrat)
data("example_data_strat", package = "avstrat")

example_width_increasing <- example_data_strat |>
  add_depths() |>
  add_layer_width(grainsize_direction = "increasing") |>
  dplyr::select(grainsize, depth, size_loc, stratlayer_order, stratsection_name) |>
  dplyr::arrange(size_loc, stratlayer_order, stratsection_name)

saveRDS(
  example_width_increasing,
  file = "tests/testthat/testdata/example_width_increasing.rds"
)

example_width_decreasing <- example_data_strat |>
  add_depths() |>
  add_layer_width(grainsize_direction = "decreasing") |>
  dplyr::select(grainsize, depth, size_loc, stratlayer_order, stratsection_name) |>
  dplyr::arrange(size_loc, stratlayer_order, stratsection_name)

saveRDS(
  example_width_decreasing,
  file = "tests/testthat/testdata/example_width_decreasing.rds"
)


# Build individual upload example data result
path_indiv <- system.file("extdata", "example_inputs.xlsx",
                            package = "avstrat")

 if (requireNamespace("readxl", quietly = TRUE)) {
   stations <- readxl::read_xlsx(path_indiv, sheet = "stations")
   sections <- readxl::read_xlsx(path_indiv, sheet = "sections")
   layers <- readxl::read_xlsx(path_indiv, sheet = "layers")
   samples <- readxl::read_xlsx(path_indiv, sheet = "samples_layer")
   }

   example_data_indiv <- load_stratdata_indiv(stations_upload = stations,
                        sections_upload = sections,
                        layers_upload = layers,
                        samples_upload = samples)
   usethis::use_data(example_data_indiv, overwrite = TRUE)

   # Extract sample data example

   expected_samples_min <- extract_sample_depths(example_data_strat,
                                                 remove_layer_metadata = TRUE)
   saveRDS(
     expected_samples_min,
     file = "tests/testthat/testdata/expected_samples_min.rds"
   )

   expected_samples_difcol <- extract_sample_depths(example_data_strat,
                                                 sample_column = "stratlayer_sample",
                                                 remove_layer_metadata = TRUE)
   saveRDS(
     expected_samples_difcol,
     file = "tests/testthat/testdata/expected_samples_difcol.rds"
   )
