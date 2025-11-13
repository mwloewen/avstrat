#' Load stratigraphic data from individual tables
#'
#' `load_stratdata_indiv()` loads necessary data for `avstrat` from separate
#' station (location), section (section metadata), stratlayer, and sample data.
#' Allows upload of smaller number of tables if data are already joined together
#' (e.g., stations-sections combined, or layers-samples combined).
#'
#' @param stations_upload A data frame with "station" metadata. The
#'   following columns are required in order to work with `avstrat` functions:
#'   - `station_id`: UniqueID for the station.
#'   - `latdd`: Location in decimal degrees, in WGS84 datum.
#'   - `longdd`: Longitude in decimal degrees, in WGS84 datum.
#'
#' @param sections_upload A data frame with "section" metadata (point to same
#'   file as station_upload if already joined). The following columns are required
#'   in order to work with `avstrat` functions:
#'   - `station_id`: UniqueID for the station, must match an existing value in station_upload.
#'   - `stratsection_name`: Unique identity of the section.
#'   - `stratmeasuremethod`: One of `"order and thickness"` or `"start and stop depth"`.
#'   - `stratlayer_order_start_at_top`: Logical, does ordering start at the top (`TRUE`) or bottom (`FALSE`)? For "start and stop depth", this defines if the reference "depth" is the top or bottom of the section.
#' @param layers_upload A data frame with "layer" metadata. The following columns are required
#'   in order to work with `avstrat` functions:
#'   - `stratsection_name`: Unique identity of the section, must match an existing value in section_upload.
#'   - `stratlayer_name`: Unique identifier for the layer.
#'   - `layer_type`: A character value from a list for plotting default layer symbols (color). If not provided,
#'      plotting functions will need to override default layer_fill mapping. Must be chosen from a validated list:
#'      \itemize{
#'        \item `volcanic`
#'        \item `tephra fall`
#'        \item `lava`
#'        \item `intrusion`
#'        \item `tuff`
#'        \item `sediment`
#'        \item `soil`
#'        \item `peat`
#'        \item `lacustrine`
#'        \item `fluvial`
#'        \item `eolian`
#'        \item `diamict`
#'        \item `clay`
#'        \item `pyroclastic density current`
#'        \item `pyroclastic surge`
#'        \item `pyroclastic flow`
#'        \item `mass wasting`
#'        \item `debris flow`
#'        \item `lahar`
#'        \item `landslide`
#'        \item `hyperconcentrated flow`
#'        \item `debris avalanche`
#'        \item `frozen water`
#'        \item `ice`
#'        \item `snow`
#'        \item `dirty snow`
#'        \item `plant`
#'        \item `rock`
#'        \item `other`
#'        \item `undifferentiated/undescribed`
#'        }
#'   - `grainsize_top`: Grain size at the top of the layer.
#'   - `grainsize_bottom`: Grain size at the bottom of the layer.
#'
#'   Grain size values must be chosen from the validated list (White & Houghton,
#'   2006, *Geology* 34:677–680):
#'   \itemize{
#'     \item `"clay"` (<1/256 mm)
#'     \item `"silt"` (1/256-1/16 mm)
#'     \item `"very fine sand/ash"` (1/16-1/8 mm)
#'     \item `"fine sand/ash"` (1/8-1/4 mm)
#'     \item `"medium sand/ash"` (1/4-1/2 mm)
#'     \item `"coarse sand/ash"` (1/2-1 mm)
#'     \item `"very coarse sand/ash"` (1-2 mm)
#'     \item `"granule/fine lapilli"` (2-4 mm)
#'     \item `"pebble/medium lapilli"` (4-16 mm)
#'     \item `"cobble/coarse lapilli"` (1.6-6.4 cm)
#'     \item `"blocks/bombs/boulders"` (>6.4 cm)
#'     \item `NA` (no data)
#'   }
#'   **If `stratmeasuremethod == "order and thickness"`**
#'   - `stratlayer_order`: Integer order of layers within the section.
#'   - `thickness_units`: One of `"meters"`, `"centimeters"`, `"millimeters"`.
#'   - `thickness_typical`: Numeric value of the typical thickness of a layer.
#'   - `thickness_min`: Numeric value of the minimum thickness of a layer.
#'   - `thickness_max`: Numeric value of the maximum thickness of a layer.
#'   **If `stratmeasuremethod == "start and stop depth"`**
#'   - `depth_units`: One of `"meters"`, `"centimeters"`, `"millimeters"`.
#'   - `depth_top`: Absolute depth of the top of the layer.
#'   - `depth_bottom`: Absolute depth of the bottom of the layer.
#'
#' @param samples_upload A data frame with "sample" metadata.
#'   - `stratlayer_name`: Unique identifier for the layer, must match an existing value in layer_upload.
#'   - `SampleID`: Unique identifier for the sample.
#'
#' @return A data frame of layers joined with section and station metadata,
#'   plus collapsed sample information:
#'   - `stratlayer_sample`: concatenated `SampleID`s per layer (separated by `"|"`).
#'   - `SampleID`: list column of `SampleID`s per layer.
#'
#' @examples
#' # Locate the example Excel files shipped with the package
#' path <- system.file("extdata", "example_inputs.xlsx",
#'   package = "avstrat"
#' )
#'
#' # Read them with readxl
#' library(readxl)
#'   stations <- readxl::read_xlsx(path, sheet = "stations")
#'   sections <- readxl::read_xlsx(path, sheet = "sections")
#'   layers <- readxl::read_xlsx(path, sheet = "layers")
#'   samples <- readxl::read_xlsx(path, sheet = "samples_layer")
#'
#'   load_stratdata_indiv(stations_upload = stations,
#'                        sections_upload = sections,
#'                        layers_upload = layers,
#'                        samples_upload = samples)
#'
#' @importFrom rlang .data
#' @export
#'
load_stratdata_indiv <- function(stations_upload,
                                 sections_upload,
                                 layers_upload,
                                 samples_upload = NULL) {
  # start with layers
  out <- layers_upload

  # join sections if not already merged
  if (!"station_id" %in% names(out)) {
    out <- dplyr::left_join(out, sections_upload, by = "stratsection_name")
  }

  # join stations if not already merged
  station_cols <- setdiff(names(stations_upload), "station_id")
  missing_station_cols <- setdiff(station_cols, names(out))
  if (length(missing_station_cols) > 0) {
    out <- dplyr::left_join(out, stations_upload, by = "station_id")
  }

  # handle samples logic
  if (!is.null(samples_upload)) {
    # collapse samples per layer
    samples_collapsed <- samples_upload |>
      dplyr::group_by(.data$stratlayer_name) |>
      dplyr::summarise(
        stratlayer_sample = paste(.data$SampleID, collapse = "|"),
        SampleID = list(.data$SampleID),
        .groups = "drop"
      )

    if ("stratlayer_sample" %in% names(out)) {
      # compare with recomputed
      check <- dplyr::left_join(
        dplyr::select(out, "stratlayer_name", "stratlayer_sample"),
        samples_collapsed,
        by = "stratlayer_name",
        suffix = c("_existing", "_computed")
      )

      mismatches <- dplyr::filter(
        check,
        .data$stratlayer_sample_existing != .data$stratlayer_sample_computed
      )

      if (nrow(mismatches) > 0) {
        warn_layers <- paste(unique(mismatches$stratlayer_name), collapse = ", ")
        warning(
          "Existing stratlayer_sample values differ from recomputed values in ",
          nrow(mismatches), " layer(s): ",
          warn_layers,
          ". Using recomputed values."
        )
        out <- dplyr::select(out, -dplyr::all_of("stratlayer_sample")) |>
          dplyr::left_join(samples_collapsed, by = "stratlayer_name")
      } else {
        # they match, so just add the nested list column if missing
        if (!"SampleID" %in% names(out)) {
          out <- dplyr::left_join(
            out,
            dplyr::select(samples_collapsed, "stratlayer_name", "SampleID"),
            by = "stratlayer_name"
          )
        }
      }
    } else {
      # no existing stratlayer_sample, so join fresh
      out <- dplyr::left_join(out, samples_collapsed, by = "stratlayer_name")
    }
  } else {
    # no samples_upload provided
    if ("stratlayer_sample" %in% names(out) && !"SampleID" %in% names(out)) {
      # derive nested SampleID from stratlayer_sample string
      out <- out |>
        dplyr::mutate(
          SampleID = strsplit(.data$stratlayer_sample, "\\|")
        )
    }
  }

  # Final normalization: replace NULL/length-0 with NA_character_ in SampleID
  if ("SampleID" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      SampleID = lapply(.data$SampleID, function(x) {
        if (is.null(x) || length(x) == 0) NA_character_ else x
      })
    )
  }

  out
}


#' Load stratigraphic data from GeoDIVA upload forms
#'
#' `load_geodiva_forms()` processes and cleans stratigraphic data from GeoDIVA
#' upload forms, specifically a form that includes Station and Sample data and
#' another form that includes the Layer data. It merges these datasets,
#' resolves any conflicts in key fields, and prepares a consolidated dataset
#' for further analysis. The function also extracts and prints a list of unique
#' stratigraphic sections.
#'
#' @param station_sample_upload data frame created from uploaded GeoDIVA format
#'  Station/Sample upload sheet, usually uploaded with `readxl::read_xlsx()`
#'  linked to filepath.
#' @param layer_upload data frame created from uploaded GeoDIVA format
#'  Layers upload sheet, usually uploaded with `readxl::read_xlsx()`
#'  linked to filepath.
#'
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @return
#' A data frame containing the merged and cleaned stratigraphic
#'     data, ready for further analysis.
#' @export
#'
#' @examples
#' # Locate the example Excel files shipped with the package
#' path_samples <- system.file("extdata", "example_samples_stations_upload_2024.xlsx",
#'   package = "avstrat"
#' )
#' path_layers <- system.file("extdata", "example_layers_upload_2024.xlsx",
#'   package = "avstrat"
#' )
#'
#' # Read them with readxl
#' library(readxl)
#'   station_sample_upload <- readxl::read_xlsx(path_samples, sheet = "Data")
#'   layer_upload <- readxl::read_xlsx(path_layers, sheet = "Data")
#'
#'   result <- load_geodiva_forms(station_sample_upload, layer_upload)
#'   head(result)  # result is a data frame
load_geodiva_forms <- function(station_sample_upload,
                               layer_upload) {
  # Get a clean station list
  stations <- station_sample_upload |>
    dplyr::select(
      dplyr::all_of(c("StationID", "Latdd", "Longdd", "Date", "LocationDesc"))
    ) |>
    dplyr::filter(!is.na(.data[["StationID"]])) |>
    dplyr::group_by(.data[["StationID"]]) |>
    dplyr::summarise(
      Latdd = {
        vals <- stats::na.omit(unique(.data[["Latdd"]]))
        if (length(vals) > 1) stop(paste("Conflict in Latdd for StationID", unique(.data[["StationID"]])))
        if (length(vals) == 0) NA else vals
      },
      Longdd = {
        vals <- stats::na.omit(unique(.data[["Longdd"]]))
        if (length(vals) > 1) stop(paste("Conflict in Longdd for StationID", unique(.data[["StationID"]])))
        if (length(vals) == 0) NA else vals
      },
      LocationDesc = {
        vals <- stats::na.omit(unique(.data[["LocationDesc"]]))
        if (length(vals) > 1) stop(paste("Conflict in LocationDesc for StationID", unique(.data[["StationID"]])))
        if (length(vals) == 0) NA else vals
      },
      .groups = "drop"
    )
  # Get a clean section list
  sections <- layer_upload |>
    dplyr::select(
      dplyr::all_of(c(
        "stratsection_name",
        "station_id",
        "stratmeasuremethod",
        "stratlayer_order_start_at_top",
        "section_notes"
      ))
      ) |>
    dplyr::filter(!is.na(.data[["stratsection_name"]])) |>
    dplyr::group_by(.data[["stratsection_name"]]) |>
    dplyr::summarise(
      station_id = {
        vals <- stats::na.omit(unique(.data[["station_id"]]))
        if (length(vals) > 1) stop(paste("Conflict in station_id for stratsection_name", unique(.data[["stratsection_name"]])))
        if (length(vals) == 0) NA else vals
      },
      stratmeasuremethod = {
        vals <- stats::na.omit(unique(.data[["stratmeasuremethod"]]))
        if (length(vals) > 1) stop(paste("Conflict in stratmeasuremethod for stratsection_name", unique(.data[["stratsection_name"]])))
        if (length(vals) == 0) NA else vals
      },
      stratlayer_order_start_at_top = {
        vals <- stats::na.omit(unique(.data[["stratlayer_order_start_at_top"]]))
        if (length(vals) > 1) stop(paste("Conflict in stratlayer_order_start_at_top for stratsection_name", unique(.data[["stratsection_name"]])))
        if (length(vals) == 0) NA else vals
      },
      section_notes = {
        vals <- stats::na.omit(unique(.data[["section_notes"]]))
        if (length(vals) > 1) stop(paste("Conflict in section_notes for stratsection_name", unique(.data[["stratsection_name"]])))
        if (length(vals) == 0) NA else vals
      },
      .groups = "drop"
    )

  # Create a clean layer list
  layers <- layer_upload |>
    dplyr::select(
      -dplyr::all_of(c(
        "station_id",
        "stratmeasuremethod",
        "stratlayer_order_start_at_top",
        "section_notes"
      ))
    ) |>
    dplyr::filter(!is.na(.data[["stratlayer_order"]]))

  # Join the tables
  data_strat <- layers |>
    dplyr::left_join(sections, by = "stratsection_name") |>
    dplyr::left_join(stations, by = c("station_id" = "StationID")) |>
   # dplyr::rename(StationID = dplyr::all_of("station_id")) |>
    dplyr::mutate(
      SampleID = strsplit(.data$stratlayer_sample, "\\|")
    )

  # Print the section list
  SectionList <- sort(unique(data_strat[["stratsection_name"]]))
  cat(paste(SectionList, collapse = "\n"))

  # Return dataframe
  return(data_strat)
}

#' Extract unnested samples with stratigraphic depths
#'
#' `extract_sample_depths()` takes a stratigraphic dataset that has already been merged
#' (e.g. from [load_geodiva_forms()] or [load_stratdata_indiv()]) and
#' applies [add_depths()] to compute absolute depths. It then expands a
#' nested sample column (by default `"SampleID"`) so that each sample is
#' represented as its own row, and drops rows where the chosen column is
#' missing. Optionally, you can strip away all other layer metadata and
#' return only the sample IDs and depth columns. Can be used on any nested or
#' unested column.
#'
#' @param strat_data A data frame ready for applying [add_depths()],
#'   containing `"SampleID"` or another column you want to expand to
#'   sample-level rows.
#' @param sample_column A string giving the name of the column to extract
#'   and unnest. Defaults to `"SampleID"`.
#' @param remove_layer_metadata Logical. If `TRUE`, only the selected
#'   sample column and the depth columns (`Depth_top`, `Depth_middle`,
#'   `Depth_bottom`) are returned. Defaults to `FALSE`.
#' @importFrom rlang .data
#'
#' @returns A data frame with one row per sample, including the depth
#'   information and associated layer metadata (unless
#'   `remove_layer_metadata = TRUE`).
#' @export
#'
#' @examples
#' # Default: expand the SampleID column
#' extract_sample_depths(example_data_strat)
#'
#' # Expand a different column (here "stratlayer_sample")
#' extract_sample_depths(example_data_strat, sample_column = "stratlayer_sample")
#'
#' # Return only SampleID and depth columns
#' extract_sample_depths(example_data_strat, remove_layer_metadata = TRUE)
extract_sample_depths <- function(strat_data,
                                  sample_column = "SampleID",
                                  remove_layer_metadata = FALSE) {
  out <- strat_data |>
    add_depths() |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = dplyr::all_of(sample_column)) |>
    tidyr::drop_na(dplyr::all_of(sample_column))

  if (isTRUE(remove_layer_metadata)) {
    out <- dplyr::select(
      out,
      dplyr::all_of(sample_column),
      dplyr::all_of(c("Depth_top", "Depth_middle", "Depth_bottom"))
    )
  }

  out
}


