#' Reformat stratigraphic layer data for polygon plotting
#'
#' This function reshapes stratigraphic layer grainsize data into a "long" format suitable
#' for plotting polygons in a stratigraphic section diagram. It constructs left/right grain size boundaries, gathers them
#' into long format, assigns depth coordinates, and converts grain size text
#' labels into numeric values for plotting.
#'
#' @param df A data frame containing stratigraphic layer information. Expected
#'   columns include:
#'   \itemize{
#'     \item \code{grainsize_top}, \code{grainsize_bottom}: grain size labels.
#'     If data are imported using \pkg{avstrat} load functions, correct data should
#'     be in the correct format. If not, grainsize values must match expected
#'     terms (see details).
#'     \item \code{Depth_top}, \code{Depth_bottom}: numeric depths for each
#'     layer. These should be determined after running [add_depths()].
#'   }
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
#' @return A tibble in long format with columns:
#'   \itemize{
#'     \item \code{size_loc}: identifier for polygon vertex locations.
#'     \item \code{size_text}: original grain size text.
#'     \item \code{depth}: numeric depth coordinate.
#'     \item \code{grainsize}: numeric grain size code for plotting.
#'   }
#'
#' @details Grain size text values are mapped to numeric codes (e.g., clay = 1,
#' silt = 1, very fine sand = 2, ..., blocks/bombs/boulders = 10). The mapping
#' is designed for stragigraphic plots plotting grainsize on the x-axis. Requires
#' grainsize values to match list:
#' \itemize{
#'  \item clay (<1/256 mm), clay, c
#'  \item silt (1/256-1/16 mm), silt, s
#'  \item very fine ash/sand (1/16-1/8 mm
#'  \item very_fine_ash_sand, very fine sand/ash, very fine ash, vfa, very fine sand, vfs
#'  \item fine ash/sand (1/8-1/4 mm), fine_ash_sand, fine sand/ash, fine ash, fa, fine sand, fs
#'  \item medium ash/sand (1/4-1/2 mm), medium_ash_sand, medium sand/ash, medium ash, ma, medium sand, ms
#'  \item coarse ash/sand (1/2-1 mm), coarse_ash_sand, coarse sand/ash, coarse ash, ca, coarse sand, cs
#'  \item very coarse ash/sand (1-2 mm), very_coarse_ash_sand, very coarse sand/ash, very coarse ash, vca, very coarse sand, vcs
#'  \item fine lapilli/granule (2-4 mm), fine_lapilli_granule, granule/fine lapilli, fine lapilli, fl, granule, g
#'  \item medium lapilli/pebble (4-16 mm), medium_lapilli_pebble, pebble/medium lapilli, medium lapilli, ml, pebble, p
#'  \item coarse lapilli/cobble (1.6-6.4 cm), coarse_lapilli_cobble, cobble/coarse lapilli, coarse lapilli, cl, cobble, co
#'  \item blocks/bombs/boulders (>6.4 cm), block_bomb_boulder, blocks/bombs/boulders, block, bomb, boulder, b
#'  }
#'
#' @importFrom rlang .data
#' @export
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
add_layer_width <- function(df, grainsize_direction = c("increasing", "decreasing")) {
  grainsize_direction <- match.arg(grainsize_direction)
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
    dplyr::arrange(.data[["stratlayer_order"]], .data[["size_loc"]]) |>
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
      ),
      grainsize = as.numeric(.data[["grainsize"]])
    )
}
