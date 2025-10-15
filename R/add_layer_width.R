#' Reformat stratigraphic layer data for polygon plotting
#'
#' This function reshapes stratigraphic layer data into a "long" format
#' suitable for plotting polygons (e.g., with \code{geom_polygon}) in a
#' stratigraphic section diagram. It constructs left/right grain size
#' boundaries, gathers them into long format, assigns depth coordinates,
#' and converts grain size text labels into numeric values for plotting.
#'
#' @param df A data frame containing stratigraphic layer information.
#'   Expected columns include:
#'   \itemize{
#'     \item \code{grainsize_top}, \code{grainsize_bottom}: grain size labels.
#'     \item \code{Depth_top}, \code{Depth_bottom}: numeric depths for each layer.
#'   }
#'
#' @return A tibble in long format with columns:
#'   \itemize{
#'     \item \code{size_loc}: identifier for polygon vertex location.
#'     \item \code{size_text}: original grain size text.
#'     \item \code{depth}: numeric depth coordinate.
#'     \item \code{grainsize}: numeric grain size code for plotting.
#'   }
#'
#' @details
#' Grain size text values are mapped to numeric codes (e.g., clay = 1,
#' silt = 1, very fine sand = 2, ..., blocks/bombs/boulders = 10).
#' The mapping is designed for consistent plotting across multiple
#' stratigraphic sections.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' df <- tidyr::tibble(
#'   grainsize_top = c("clay", "silt"),
#'   grainsize_bottom = c("silt", "sand"),
#'   Depth_top = c(0, 10),
#'   Depth_bottom = c(10, 20)
#' )
#' add_layer_width(df)
add_layer_width <- function(df) {
  df |>
    dplyr::mutate(
      a_LeftTop    = "axis",
      d_LeftBottom = "axis",
      b_SizeTop    = .data[["grainsize_top"]],
      c_SizeBottom = .data[["grainsize_bottom"]]
    ) |>
    tidyr::pivot_longer(
      cols = c("a_LeftTop":"c_SizeBottom"),
      names_to = "size_loc",
      values_to = "size_text"
    ) |>
    dplyr::mutate(
      depth = dplyr::case_when(
        .data[["size_loc"]] == "b_SizeTop"  ~ .data[["Depth_top"]],
        .data[["size_loc"]] == "a_LeftTop"  ~ .data[["Depth_top"]],
        TRUE                                ~ .data[["Depth_bottom"]]
      )
    ) |>
    dplyr::arrange(.data[["size_loc"]]) |>
    dplyr::mutate(
      grainsize = dplyr::case_when(
        .data[["size_text"]] == "axis" ~ "-1",
        .data[["size_text"]] %in% c("clay (<1/256 mm)", "clay", "c") ~ "1",
        .data[["size_text"]] %in% c("silt (1/256-1/16 mm)", "silt", "s") ~ "1",
        .data[["size_text"]] %in% c("very fine ash/sand (1/16-1/8 mm)",
                                    "very_fine_ash_sand", "very fine sand/ash",
                                    "very fine ash", "vfa", "very fine sand", "vfs") ~ "2",
        .data[["size_text"]] %in% c("fine ash/sand (1/8-1/4 mm)",
                                    "fine_ash_sand", "fine sand/ash",
                                    "fine ash", "fa", "fine sand", "fs") ~ "3",
        .data[["size_text"]] %in% c("medium ash/sand (1/4-1/2 mm)",
                                    "medium_ash_sand", "medium sand/ash",
                                    "medium ash", "ma", "medium sand", "ms") ~ "4",
        .data[["size_text"]] %in% c("coarse ash/sand (1/2-1 mm)",
                                    "coarse_ash_sand", "coarse sand/ash",
                                    "coarse ash", "ca", "coarse sand", "cs") ~ "5",
        .data[["size_text"]] %in% c("very coarse ash/sand (1-2 mm)",
                                    "very_coarse_ash_sand", "very coarse sand/ash",
                                    "very coarse ash", "vca", "very coarse sand", "vcs") ~ "6",
        .data[["size_text"]] %in% c("fine lapilli/granule (2-4 mm)",
                                    "fine_lapilli_granule", "granule/fine lapilli",
                                    "fine lapilli", "fl", "granule", "g") ~ "7",
        .data[["size_text"]] %in% c("medium lapilli/pebble (4-16 mm)",
                                    "medium_lapilli_pebble", "pebble/medium lapilli",
                                    "medium lapilli", "ml", "pebble", "p") ~ "8",
        .data[["size_text"]] %in% c("coarse lapilli/cobble (1.6-6.4 cm)",
                                    "coarse_lapilli_cobble", "cobble/coarse lapilli",
                                    "coarse lapilli", "cl", "cobble", "co") ~ "9",
        .data[["size_text"]] %in% c("blocks/bombs/boulders (>6.4 cm)",
                                    "block_bomb_boulder", "blocks/bombs/boulders",
                                    "block", "bomb", "boulder", "b") ~ "10",
        is.na(.data[["size_text"]]) ~ "0",
        .default = "-1"
      ),
      grainsize = as.numeric(.data[["grainsize"]])
    )
}
