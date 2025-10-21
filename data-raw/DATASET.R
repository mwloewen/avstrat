## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)


# Temporary scripts for package dev

# Dev session

library(dplyr)
library(ggplot2)

data("example_data_strat", package = "avstrat")
df <- example_data_strat |>
  add_depths()

SelectStation <- "21LSHD02"

p1 <- ggstrat(df = df, stratsection_name = SelectStation)
p2 <- ggstrat_sampleID(df = df, stratsection_name = SelectStation)

library(patchwork)
p1 + p2


ggstrat_column(df = df, stratsection_name = SelectStation)

width_test_reverse <- example_data_strat |>
  filter(stratsection_name == SelectStation) |>
  add_depths() |>
  add_layer_width(grainsize_direction = "decreasing") |>
  select(stratlayer_name, grainsize_top, grainsize_bottom, Depth_top, Depth_bottom, size_loc, size_text,
         depth, grainsize_base, grainsize)



plot_test_layer_results <- ggplot2::get_layer_data()

p <- ggstrat(df = df, stratsection_name = "21LSHD02", grainsize_direction = "increasing")
result <- ggplot2::get_layer_data(p, 1)

saveRDS(result,
        file = testthat::test_path("testdata/plot_test_layer_results.rds"))


# Create a clean sample list
samples <- station_sample_upload |>
  dplyr::select(
    .data[["StationID"]], .data[["SampleID"]], .data[["sample_parent_id"]],
    .data[["at_num"]], .data[["possible_source_volcanoes"]],
    .data[["EruptionID"]], .data[["SampType1"]], .data[["SampType2"]],
    .data[["SampleDesc"]], .data[["SampUnit"]],
    .data[["ReasonforCollection"]]
  ) |>
  dplyr::filter(!is.na(.data[["SampleID"]]))



#Test bulk save
ggstrat_bulk_save(df = example_data_strat,
                  plotfunction= ggstrat_label)
