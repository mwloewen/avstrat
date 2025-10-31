#### Language for add_depths() and add_layers() inputs ####

# We need certain info identified to run add_depths():
#   station_id: Unique identity of the location of the section, not strictly required for add_depths() but expected for other functions.
#   stratsection_name: Unique identity of the section (repeated for each layer). Must be present for each row!
#   stratlayer_name: Unique identiy of the layer. Must be present for each row!
#   stratlayermethod = c("order and thickness", "start and stop depth") Must be present for each row! Can be joined from a simplified section metadata table.
#     If stratlayermethod = "order and thickness":
#         stratlayer_order_start_at_top = TRUE/FALSE: Does ordering start at the top (TRUE) or bottom (FALSE)? Must be present for each row!
#         stratlayer_order: integer order of layers within section. Must be present for each row!
#         thickness_units= C("meters", "centimeters", "millimeters:): units of thickness, maybe different for each layer. Must be present for each row!
#         thickness_typical: typical thickness of each layer (overrides min or max in plotting). Can be NA, but one of 3 thickness values must be present.
#         thickness_min: minimum thickness of each layer, will plot as average of min and max if no typical provided. Can be NA, but one of 3 thickness values must be present.
#         thickness_max: maximum thickness of each layer, will plot as average of min and max if no typical provided. Can be NA, but one of 3 thickness values must be present.
#     If stratlayermethod = "start and stop depth":
#         depth_units= C("meters", "centimeters", "millimeters:): units of depth, maybe different for each layer but best practice to be the same for each section. Must be present for each row!
#         depth_top: absolute depth of top of layer. Must be present for each row!
#         depth_bottom: absolute depth of bottom of layer. Must be present for each row!
# Note other columns supplied to add_depths() come along for the ride.
#


# We need certain info identified to run add_layer_width() and ggstrat() [ggstrat_column() and ggstrat_label() only needs add_depths()]:
#   stratsection_name: Unique identity of the section (repeated for each layer). Must be present for each row!
#   stratlayer_name: Unique identiy of the layer. Must be present for each row!
#   Depth_top and Depth_bottom: returned from add_depths(). Must be present for each row!
#   grainsize_top: grain size at top of layer, must be chosen from validated list, following terminology from White and Houghton (2006, Geology v. 34, p. 677-680):
#     "clay": grains <1/256 mm
#     "silt": grains 1/256 mm to 1/16 mm
#     "very fine sand/ash": grains 1/16 mm to 1/8 mm
#     "fine sand/ash": grains 1/8 mm to 1/4 mm
#     "medium sand/ash": grains 1/4 mm to 1/2 mm
#     "coarse sand/ash": grains 1/2 mm to 1 mm
#     "very coarse sand/ash": grains 1 mm to 2 mm
#     "granule/fine lapilli": grains 2 mm to 4 mm
#     "pebble/medium lapilli": grains 4 mm to 8 mm
#     "cobble/coarse lapilli": grains 8 mm to 64 mm
#     "blocks/bombs/boulders": grains >64 mm
#     NA: no data, okay if no grainsize described for a layer
#   grainsize_bottom: grain size at bottom of layer, must be chosen from validated list (same as grainsize_top)
# Note other columns supplied to add_layer_width() come along for the ride.
# The code currently accepts several legacy grainsize terms, such as "vca" or "vsc" for "very coarse sand/ash", this maybe simplified in future code and should not be relied on.



#### Building new load functions ####

# Package Dependencies

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(avstrat)

# Load example data

stations <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "stations")
sections <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "sections")
stations_sections <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "stations_sections")
layers <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "layers")
layers_sample <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "layers_sample")
samples <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "samples")
samples_layer <- read_xlsx("inst/extdata/example_inputs.xlsx", sheet = "samples_layer")



# 2. Explode samples and add depth info
library(avstrat)
extract_sample_depths <- function(strat_data, sample_column = SampleID) {
  strat_data |>
    add_depths() |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = {{ sample_column }}) |>
    tidyr::drop_na({{ sample_column }})
}
sampledf <- extract_sample_depths(
  strat_data = example_data_strat)

sampledf <- extract_sample_depths(
  strat_data = example_data_strat,
  sample_column = stratlayer_sample)


  samples_upload |>
    dplyr::left_join(
      dplyr::select(extract_samples, "SampleID", "Depth_top", "Depth_middle", "Depth_bottom"),
      by = "SampleID"
    )
}


# Test Layers
result1 <- load_layers_with_samples(stations_upload = stations,
                                    sections_upload = sections,
                                    layers_upload = layers,
                                    samples_upload = samples_layer)
result1 |> ggstrat_samples(stratsection_name = "17KWLCI025", label = "stratlayer_sample")

# Test Samples
samples_depth <- extract_samples_with_depths(layers_with_samples = result1,
                                             samples_upload = samples)

samples_depth |> filter(station_id == "17KWLCI025") |>
  ggplot() +
  geom_point(aes(x= (Depth_top + 0.5), y = CalibAge))



#### 2. stations_sections = Sample - Layer join ####

load_stationsection_layer_sample_data <- function(
    stations_upload = stations,
    sections_upload = sections,
    layers_upload = layers,
    samples_upload = samples_layer) {

  # collapse samples per layer
  samples_collapsed <- samples_upload |>
    dplyr::group_by(.data$stratlayer_name) |>
    dplyr::summarise(
      stratlayer_sample = paste(.data$SampleID, collapse = "|"),
      samples_nested = list(.data$SampleID),
      .groups = "drop"
    )


  layers_upload |>
    dplyr::left_join(
      sections_upload,
      by = "stratsection_name"
    ) |>
    dplyr::left_join(
      stations_upload,
      by = "station_id"
    ) |>
    dplyr::left_join(
      samples_collapsed,
      by = c("stratlayer_name")
    )
}

