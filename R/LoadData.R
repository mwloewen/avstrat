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
#' @import dplyr
#' @importFrom stats na.omit
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
#' # Read them with readxl (only if readxl is available)
#' if (requireNamespace("readxl", quietly = TRUE)) {
#'   station_sample_upload <- readxl::read_xlsx(path_samples, sheet = "Data")
#'   layer_upload <- readxl::read_xlsx(path_layers, sheet = "Data")
#'
#'   result <- load_geodiva_forms(station_sample_upload, layer_upload)
#'   head(result)  # result is a data frame
#' }
load_geodiva_forms <- function(station_sample_upload,
                               layer_upload) {
  # Get a clean station list
  stations <- station_sample_upload |>
    dplyr::select(
      .data[["StationID"]],
      .data[["Latdd"]],
      .data[["Longdd"]],
      .data[["Date"]],
      .data[["LocationDesc"]]
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
      .data[["stratsection_name"]],
      .data[["station_id"]],
      .data[["stratmeasuremethod"]],
      .data[["stratlayer_order_start_at_top"]],
      .data[["section_notes"]]
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
      -.data[["station_id"]],
      -.data[["stratmeasuremethod"]],
      -.data[["stratlayer_order_start_at_top"]],
      -.data[["section_notes"]]
    ) |>
    dplyr::filter(!is.na(.data[["stratlayer_order"]]))

  # Join the tables
  data_strat <- layers |>
    dplyr::left_join(sections, by = "stratsection_name") |>
    dplyr::left_join(stations, by = c("station_id" = "StationID")) |>
    dplyr::rename(StationID = .data[["station_id"]]) |>
    dplyr::rename(SampleID = .data[["stratlayer_sample"]])

  # Print the section list
  SectionList <- sort(unique(data_strat[["stratsection_name"]]))
  cat(paste(SectionList, collapse = "\n"))

  # Return dataframe
  return(data_strat)
}
