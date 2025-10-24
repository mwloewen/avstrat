#' Add standardized depth information to stratigraphic layer data
#'
#' This function takes a data frame of stratigraphic layer information and
#' calculates standardized thickness and depth values. It ensures required
#' columns are present, converts thickness and depth units to centimeters,
#' derives a plotting thickness, and computes top, bottom, and middle depths
#' for each layer within a stratigraphic section. The function is designed to
#' handle input where layers are defined either by thickness or with  absolute
#' start and stop depth values.
#'
#' @param df A data frame containing stratigraphic layer information. The
#'  function works with two types of data input. If \code{stratlayermethod} is
#'  "order and thickness", expected columns include:
#'   \itemize{
#'     \item \code{stratlayer_order_start_at_top} Logical or numeric indicator
#'       of whether ordering starts at the top.
#'     \item \code{thickness_units}, \code{thickness_typical},
#'       \code{thickness_min}, \code{thickness_max} Thickness information.
#'     \item \code{stratsection_name}, \code{stratlayer_order} Identifiers for
#'       sections and the relative order of each layer.
#'   }
#'  If \code{stratlayermethod} is "start and stop depth", expected columns
#'  include:
#'  \itemize{
#'    \item \code{depth_units}, \code{depth_top}, \code{depth_bottom} Absolute
#'       depth information.
#'    }
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
    "stratmeasuremethod",
    "stratlayer_order_start_at_top", "thickness_units", "thickness_typical",
    "thickness_min", "thickness_max", "depth_units", "depth_top",
    "depth_bottom", "stratsection_name", "stratlayer_name", "stratlayer_order"
  )
  # Add missing expected columns with NA values
  missing <- setdiff(expected_headers, names(df))
  df[missing] <- NA

  # error if stratsection_name present but no stratmeasuremethod
  if (any(!is.na(df$stratsection_name) & is.na(df$stratmeasuremethod))) {
    stop("Rows with stratsection_name must define stratmeasuremethod.")
  }

  # error if depth_top and depth_bottom not present when stratmeasuremethod == "start and stop depth"
  bad_rows1 <- df |>
    dplyr::filter(
      .data$stratmeasuremethod == "start and stop depth" &
        (is.na(.data$depth_top) | is.na(.data$depth_bottom) | is.na(.data$depth_units))
    )

  if (nrow(bad_rows1) > 0) {
    bad_sections <- unique(bad_rows1$stratsection_name)
    stop(
      "Rows with stratmeasuremethod 'start and stop depth' must define depth_top, depth_bottom, and depth_units.\n",
      "Problematic stratsection_name(s): ",
      paste(bad_sections, collapse = ", ")
    )
  }

  # error if thickness and order values not present when stratmeasuremethod == "order and thickness"
  bad_rows2 <- df |>
    dplyr::filter(
      .data$stratmeasuremethod == "order and thickness" &
        (
          # all three thickness fields missing
          (is.na(.data$thickness_typical) & is.na(.data$thickness_min) & is.na(.data$thickness_max)) |
            # or missing units
            is.na(.data$thickness_units) |
            # or missing order
            is.na(.data$stratlayer_order)
        )
    )

  if (nrow(bad_rows2) > 0) {
    bad_sections <- unique(bad_rows2$stratsection_name)
    stop(
      "Rows with stratmeasuremethod 'order and thickness' must define at least one of thickness_typical, thickness_min, or thickness_max, ",
      "and must define thickness_units and stratlayer_order.\n",
      "Problematic stratsection_name(s): ",
      paste(bad_sections, collapse = ", ")
    )
  }

  # internal function to convert to centimeters for consistency
  convert_to_cm <- function(value, units) {
    dplyr::case_when(
      units %in% c("cm", "centimeters") ~ as.numeric(value),
      units %in% c("mm", "millimeters") ~ as.numeric(value) / 10,
      units %in% c("m", "meters") ~ as.numeric(value) * 100,
      TRUE ~ NA_real_
    )
  }

  # branch by stratmeasuremethod
  dplyr::bind_rows(

  # --- Method 1: order and thickness ---
  df |>
    dplyr::filter(.data$stratmeasuremethod == "order and thickness") |>
    tidyr::drop_na(dplyr::all_of("stratlayer_order_start_at_top")) |>
    dplyr::mutate(
      thickness_defining_cm = convert_to_cm(.data[["thickness_typical"]], .data[["thickness_units"]]),
      thickness_min_cm      = convert_to_cm(.data[["thickness_min"]], .data[["thickness_units"]]),
      thickness_max_cm      = convert_to_cm(.data[["thickness_max"]], .data[["thickness_units"]])
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
    tidyr::drop_na(dplyr::all_of("thickness_plot")) |>
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
      Depth_bottom = cumsum(.data[["thickness_plot"]]),
      Depth_top    = .data[["Depth_bottom"]] - .data[["thickness_plot"]],
      Depth_middle = .data[["Depth_top"]] + (.data[["Depth_bottom"]] - .data[["Depth_top"]]) / 2
    ) |>
    tidyr::drop_na(dplyr::all_of("Depth_middle")),

  # --- Method 2: start and stop depth ---
  df |>
    dplyr::filter(.data$stratmeasuremethod == "start and stop depth") |>
    dplyr::mutate(
      depth_top_cm    = convert_to_cm(.data[["depth_top"]], .data[["depth_units"]]),
      depth_bottom_cm = convert_to_cm(.data[["depth_bottom"]], .data[["depth_units"]]),
      Depth_top       = .data[["depth_top_cm"]],
      Depth_bottom    = .data[["depth_bottom_cm"]],
      Depth_middle    = .data[["Depth_top"]] + (.data[["Depth_bottom"]] - .data[["Depth_top"]]) / 2
    )
  ) |>
  # Clean up columns not needed going forward
  dplyr::select(
    -dplyr::any_of(c(
      "thickness_defining_cm", "thickness_min_cm", "thickness_max_cm", "thickness_plot",
      "depth_top_cm", "depth_bottom_cm"
    ))
  )
}
