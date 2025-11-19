#' Reformat stratigraphic layer data for polygon plotting
#'
#' `add_layer_width()` reshapes stratigraphic layer grainsize data into a "long"
#' format suitable for plotting polygons in a stratigraphic section diagram. It
#' constructs left/right grain size boundaries, gathers them into long format,
#' assigns depth coordinates, and converts grain size text labels into numeric
#' values for plotting with `ggstrat()`.
#'
#' @param df A data frame containing stratigraphic layer information. The
#'   following columns are required:
#'
#'   - `stratsection_name`: Unique identity of the section (repeated for each layer).
#'   - `stratlayer_name`: Unique identity of the layer.
#'   - `Depth_top`, `Depth_bottom`: Depths in centimeters, as returned by `add_depths()`.
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
#'
#'   Several legacy abbreviations (e.g. `"vca"`, `"vcs"`) are currently accepted
#'   but may be deprecated in future versions.
#'
#' @param grainsize_direction Character string, one of \code{"increasing"} or
#'   \code{"decreasing"}. Controls the numeric mapping of grain sizes:
#'   \itemize{
#'     \item \code{"increasing"} (default): clay/silt = 1, ..., blocks/bombs/boulders = 10.
#'     \item \code{"decreasing"}: clay/silt = 10, ..., blocks/bombs/boulders = 1.
#'   }
#'   Increasing will show coarser units as bigger polygons (more prominent)
#'   which is espeically useful for emphasizing more energentic volcanic deposits.
#'   Decreasing will show finer (typically more resistive) units as bigger
#'   which may better match observed erosional profiles.
#'
#' @return A tibble in long format with original data plus:
#'   \itemize{
#'     \item \code{size_loc}: identifier for polygon vertex locations.
#'     \item \code{size_text}: original grain size text.
#'     \item \code{depth}: numeric depth coordinate.
#'     \item \code{grainsize}: numeric grain size code for plotting.
#'   }
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' df <- tidyr::tibble(
#' stratlayer_order = c(1,2),
#' grainsize_top = c("clay", "coarse sand/ash"),
#' grainsize_bottom = c( "silt", "medium sand/ash"),
#' Depth_top = c(0, 10),
#' Depth_bottom = c(10, 20)
#' )
#' add_layer_width(df)
#'
#' @importFrom rlang .data
#' @export

add_layer_width <- function(df, grainsize_direction = c("increasing", "decreasing")) {
  grainsize_direction <- match.arg(grainsize_direction)
    # validate input dataframe
    ## Depth consistency
    bad_depths <- df |>
    dplyr::filter(!is.na(.data$Depth_top) & !is.na(.data$Depth_bottom) &
                    .data$Depth_top > .data$Depth_bottom)
  if (nrow(bad_depths) > 0) {
    stop("Invalid depth ranges (Depth_top >= Depth_bottom):\n",
         paste(bad_depths$stratsection_name, bad_depths$stratlayer_name, collapse = "\n"))
  }

  ## Valid grain size terms
  valid_grains <- c(
    "clay", "silt", "very fine sand/ash", "fine sand/ash", "medium sand/ash",
    "coarse sand/ash", "very coarse sand/ash", "granule/fine lapilli",
    "pebble/medium lapilli", "cobble/coarse lapilli", "blocks/bombs/boulders"
  )
  legacy <- c("clay (<1/256 mm)", "clay",
              "silt (1/256-1/16 mm)", "silt", "s",
              "very fine ash/sand (1/16-1/8 mm)", "very_fine_ash_sand", "very fine sand/ash", "very fine ash", "vfa", "very fine sand", "vfs",
              "fine ash/sand (1/8-1/4 mm)", "fine_ash_sand", "fine sand/ash", "fine ash", "fa", "fine sand", "fs",
              "medium ash/sand (1/4-1/2 mm)", "medium_ash_sand", "medium sand/ash", "medium ash", "ma", "medium sand", "ms",
              "coarse ash/sand (1/2-1 mm)", "coarse_ash_sand", "coarse sand/ash", "coarse ash", "ca", "coarse sand", "cs",
              "very coarse ash/sand (1-2 mm)", "very_coarse_ash_sand", "very coarse sand/ash", "very coarse ash", "vca", "very coarse sand", "vcs",
              "fine lapilli/granule (2-4 mm)", "fine_lapilli_granule", "granule/fine lapilli", "fine lapilli", "fl", "granule", "g",
              "medium lapilli/pebble (4-16 mm)", "medium_lapilli_pebble", "pebble/medium lapilli", "medium lapilli", "ml", "pebble", "p",
              "coarse lapilli/cobble (1.6-6.4 cm)","coarse_lapilli_cobble", "cobble/coarse lapilli", "coarse lapilli", "cl", "cobble", "co",
              "blocks/bombs/boulders (>6.4 cm)", "block_bomb_boulder", "blocks/bombs/boulders", "block", "bomb", "boulder", "b")
  # legacy terms work for now but may be removed in future to clean up code

  bad_grains <- df |>
    dplyr::filter(!is.na(.data$grainsize_top) & !.data$grainsize_top %in% c(valid_grains, legacy) |
                    !is.na(.data$grainsize_bottom) & !.data$grainsize_bottom %in% c(valid_grains, legacy))
  if (nrow(bad_grains) > 0) {
    stop("Invalid grain size terms detected:\n",
         paste(bad_grains$stratsection_name, bad_grains$stratlayer_name, collapse = "\n"))
  }

  # End of validation

  df |>

    # Prepare data for geom_polygon layer of a strat section.
    # Add depth coordinates to construct the left side of layer polygons along the y-axis
    dplyr::mutate(
      a_LeftTop    = "axis",
      b_SizeTop    = .data[["grainsize_top"]],
      c_SizeBottom = .data[["grainsize_bottom"]],
      d_LeftBottom = "axis"
    ) |>
    # Gather data into long format for making layer polygons as a new dataframe:
    # 1) Move grainsize into a single column for plotting on the x-axis.
    tidyr::pivot_longer(cols = c("a_LeftTop":"d_LeftBottom"),
                 names_to = "size_loc",
                 values_to = "size_text") |>

    # 2) Move the depth and grainsize components into a single column for plotting on the y-axis.
    dplyr::mutate(
      depth = dplyr::case_when(
        size_loc %in% c("a_LeftTop", "b_SizeTop") ~ .data[["Depth_top"]],
        size_loc %in% c("c_SizeBottom", "d_LeftBottom") ~ .data[["Depth_bottom"]]
      ),
      size_loc = factor(
        .data[["size_loc"]],
        levels = c("a_LeftTop", "b_SizeTop", "c_SizeBottom", "d_LeftBottom")
      )
    ) |>
    dplyr::arrange(.data$Depth_top, .data$size_loc) |>
    dplyr::mutate(
      grainsize_base = dplyr::case_when(
        .data[["size_text"]] == "axis" ~ -1,
        .data[["size_text"]] %in% c("clay (<1/256 mm)", "clay", "c") ~ 1,
        .data[["size_text"]] %in% c("silt (1/256-1/16 mm)", "silt", "s") ~ 1,
        .data[["size_text"]] %in% c("very fine ash/sand (1/16-1/8 mm)",
                                    "very_fine_ash_sand", "very fine sand/ash",
                                    "very fine ash", "vfa", "very fine sand", "vfs") ~ 2,
        .data[["size_text"]] %in% c("fine ash/sand (1/8-1/4 mm)",
                                    "fine_ash_sand", "fine sand/ash",
                                    "fine ash", "fa", "fine sand", "fs") ~ 3,
        .data[["size_text"]] %in% c("medium ash/sand (1/4-1/2 mm)",
                                    "medium_ash_sand", "medium sand/ash",
                                    "medium ash", "ma", "medium sand", "ms") ~ 4,
        .data[["size_text"]] %in% c("coarse ash/sand (1/2-1 mm)",
                                    "coarse_ash_sand", "coarse sand/ash",
                                    "coarse ash", "ca", "coarse sand", "cs") ~ 5,
        .data[["size_text"]] %in% c("very coarse ash/sand (1-2 mm)",
                                    "very_coarse_ash_sand", "very coarse sand/ash",
                                    "very coarse ash", "vca", "very coarse sand", "vcs") ~ 6,
        .data[["size_text"]] %in% c("fine lapilli/granule (2-4 mm)",
                                    "fine_lapilli_granule", "granule/fine lapilli",
                                    "fine lapilli", "fl", "granule", "g") ~ 7,
        .data[["size_text"]] %in% c("medium lapilli/pebble (4-16 mm)",
                                    "medium_lapilli_pebble", "pebble/medium lapilli",
                                    "medium lapilli", "ml", "pebble", "p") ~ 8,
        .data[["size_text"]] %in% c("coarse lapilli/cobble (1.6-6.4 cm)",
                                    "coarse_lapilli_cobble", "cobble/coarse lapilli",
                                    "coarse lapilli", "cl", "cobble", "co") ~ 9,
        .data[["size_text"]] %in% c("blocks/bombs/boulders (>6.4 cm)",
                                    "block_bomb_boulder", "blocks/bombs/boulders",
                                    "block", "bomb", "boulder", "b") ~ 10,
        is.na(.data[["size_text"]]) ~ 0,
        .default = -1
      ),
      grainsize = dplyr::case_when(
        grainsize_base %in% 0:10 & grainsize_direction == "decreasing" ~ 10 - grainsize_base,
        TRUE ~ grainsize_base
      )
    )
}
