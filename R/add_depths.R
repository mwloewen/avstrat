#' Add standardized depth information to stratigraphic layer data
#'
#' This function takes a data frame of stratigraphic layer information and
#' calculates standardized thickness and depth values. It ensures required
#' columns are present, converts thickness and depth units to centimeters,
#' derives a plotting thickness, and computes top, bottom, and middle depths
#' for each layer within a stratigraphic section. The function is designed to
#' handle input where layers are defined either by thickness. The function is
#' not currently configured to work with absolute depth values.
#'
#' @param df A data frame containing stratigraphic layer information. Expected
#'   columns include:
#'   \itemize{
#'     \item \code{stratlayer_order_start_at_top} Logical or numeric indicator
#'       of whether ordering starts at the top.
#'     \item \code{thickness_units}, \code{thickness_typical},
#'       \code{thickness_min}, \code{thickness_max} Thickness information.
#'     \item \code{depth_units}, \code{depth_top}, \code{depth_bottom} Absolute
#'       depth information, not currently used.
#'     \item \code{stratsection_name}, \code{stratlayer_name} Identifiers for
#'       sections and layers.
#'   }
#'   Missing columns are added automatically and filled with \code{NA}.
#'
#' @importFrom rlang .data
#'
#' @return A tibble with the original data plus additional columns:
#'   \itemize{
#'     \item \code{thickness_defining_cm}, \code{thickness_min_cm},
#'       \code{thickness_max_cm}: thickness values converted to centimeters.
#'     \item \code{depth_top_cm}, \code{depth_bottom_cm}: depth values converted
#'       to centimeters.
#'     \item \code{thickness_plot}: a single representative thickness for
#'       plotting, derived from available thickness information.
#'     \item \code{thickness_plot_warning}: warning message if no thickness was
#'       available.
#'     \item \code{Depth_top}, \code{Depth_bottom}, \code{Depth_middle}:
#'       calculated depths (cm) for each layer.
#'   }
#'   Rows without sufficient information are dropped.
#'
#' @details
#' The function groups data by \code{stratsection_name} and orders layers
#' according to \code{stratlayer_order_start_at_top}. Depths are then computed
#' cumulatively (if only thickness is provided) or taken directly from absolute
#' depth columns (if available).
#'
#' @export
#'
add_depths <- function(df) {
  expected_headers <- c(
    "stratlayer_order_start_at_top", "thickness_units", "thickness_typical",
    "thickness_min", "thickness_max", "depth_units", "depth_top",
    "depth_bottom", "stratsection_name", "stratlayer_name", "stratlayer_order"
  )
  missing <- setdiff(expected_headers, names(df))
  df[missing] <- NA

  convert_to_cm <- function(value, units) {
    dplyr::case_when(
      units %in% c("cm", "centimeters") ~ as.numeric(value),
      units %in% c("mm", "millimeters") ~ as.numeric(value) / 10,
      units %in% c("m", "meters") ~ as.numeric(value) * 100,
      TRUE ~ NA_real_
    )
  }

  df |>
    tidyr::drop_na(.data[["stratlayer_order_start_at_top"]]) |>
    dplyr::mutate(
      thickness_defining_cm = convert_to_cm(.data[["thickness_typical"]], .data[["thickness_units"]]),
      thickness_min_cm      = convert_to_cm(.data[["thickness_min"]], .data[["thickness_units"]]),
      thickness_max_cm      = convert_to_cm(.data[["thickness_max"]], .data[["thickness_units"]]),
      depth_top_cm          = convert_to_cm(.data[["depth_top"]], .data[["depth_units"]]),
      depth_bottom_cm       = convert_to_cm(.data[["depth_bottom"]], .data[["depth_units"]])
    ) |>
    dplyr::mutate(
      thickness_plot = dplyr::case_when(
        !is.na(.data[["thickness_defining_cm"]]) ~ as.numeric(.data[["thickness_defining_cm"]]),
        is.na(.data[["thickness_defining_cm"]]) & !is.na(.data[["thickness_min_cm"]]) & !is.na(.data[["thickness_max_cm"]]) ~
          (as.numeric(.data[["thickness_min_cm"]]) + as.numeric(.data[["thickness_max_cm"]])) / 2,
        is.na(.data[["thickness_defining_cm"]]) & !is.na(.data[["thickness_min_cm"]]) & is.na(.data[["thickness_max_cm"]]) ~
          as.numeric(.data[["thickness_min_cm"]]),
        is.na(.data[["thickness_defining_cm"]]) & is.na(.data[["thickness_min_cm"]]) & !is.na(.data[["thickness_max_cm"]]) ~
          as.numeric(.data[["thickness_max_cm"]]),
        TRUE ~ NA_real_
      ),
      thickness_plot_warning = dplyr::if_else(
        is.na(.data[["thickness_plot"]]),
        paste0("No thickness to plot for ", .data[["stratlayer_name"]]),
        NA_character_
      )
    ) |>
    tidyr::drop_na(.data[["thickness_plot"]]) |>
    dplyr::group_by(.data[["stratsection_name"]]) |>
    dplyr::mutate(stratlayer_order_start_at_top = as.logical(.data[["stratlayer_order_start_at_top"]])) |>
    dplyr::arrange(
      dplyr::if_else(
        .data[["stratlayer_order_start_at_top"]],
        as.numeric(.data[["stratlayer_order"]]),
        -as.numeric(.data[["stratlayer_order"]])   # negate to reverse
      ),
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      Depth_bottom = dplyr::case_when(
        is.numeric(.data[["thickness_plot"]]) & is.na(.data[["depth_bottom_cm"]]) ~ cumsum(.data[["thickness_plot"]]),
        .data[["stratlayer_order_start_at_top"]] & is.numeric(.data[["depth_bottom_cm"]]) ~ .data[["depth_bottom_cm"]],
        !.data[["stratlayer_order_start_at_top"]] & is.numeric(.data[["depth_bottom_cm"]]) ~ max(.data[["depth_top_cm"]]) - .data[["depth_bottom_cm"]],
        .default = NA
      ),
      Depth_top = dplyr::case_when(
        is.numeric(.data[["thickness_plot"]]) & is.na(.data[["depth_top_cm"]]) ~ .data[["Depth_bottom"]] - .data[["thickness_plot"]],
        .data[["stratlayer_order_start_at_top"]] & is.numeric(.data[["depth_top_cm"]]) ~ .data[["depth_top_cm"]],
        !.data[["stratlayer_order_start_at_top"]] & is.numeric(.data[["depth_top_cm"]]) ~ max(.data[["depth_top_cm"]]) - .data[["depth_top_cm"]],
        .default = NA
      ),
      Depth_middle = .data[["Depth_top"]] + (.data[["Depth_bottom"]] - .data[["Depth_top"]]) / 2
    ) |>
    tidyr::drop_na(.data[["Depth_middle"]])
}
