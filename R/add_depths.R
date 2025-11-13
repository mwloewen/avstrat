#' Add standardized depth information to stratigraphic layer data
#'
#' `add_depths()` takes a data frame of stratigraphic layer information and
#'  calculates standardized thickness and depth values. It ensures required
#'  columns are present, converts thickness and depth units to centimeters,
#'  derives a plotting thickness, and computes top, bottom, and middle depths
#'  for each layer within a stratigraphic section. The function is designed to
#'  handle input where layers are defined either by order and thickness or with
#'  absolute start and stop depth values.
#'
#' @param df A data frame containing stratigraphic layer information. The
#'   following columns are required depending on the method:
#'
#'   **Always required**
#'   - `stratsection_name`: Unique identity of the section (repeated for each layer).
#'   - `stratlayer_name`: Unique identity of the layer.
#'   - `stratmeasuremethod`: One of `"order and thickness"` or `"start and stop depth"`.
#'   - `stratlayer_order_start_at_top`: Logical, does ordering start at the top (`TRUE`) or bottom (`FALSE`)? For "start and stop depth", this defines if the reference "depth" is the top or bottom of the section.
#'
#'   **If `stratmeasuremethod == "order and thickness"`**
#'   - `stratlayer_order`: Integer order of layers within the section.
#'   - `thickness_units`: One of `"meters"`, `"centimeters"`, `"millimeters"`.
#'   - At least one of `thickness_typical`, `thickness_min`, or `thickness_max`.
#'
#'   **If `stratmeasuremethod == "start and stop depth"`**
#'   - `depth_units`: One of `"meters"`, `"centimeters"`, `"millimeters"`.
#'   - `depth_top`: Absolute depth of the top of the layer.
#'   - `depth_bottom`: Absolute depth of the bottom of the layer.
#'
#'   Other columns are carried through unchanged. Missing expected columns are
#'   added automatically and filled with `NA`.
#'
#' @return A tibble with the original data plus:
#'   - `thickness_min_cm`, `thickness_max_cm`: thickness values converted to cm.
#'   - `depth_top_cm`, `depth_bottom_cm`: depth values converted to cm.
#'   - `thickness_plot`: representative thickness used for plotting.
#'   - `thickness_plot_warning`: message if no thickness was available.
#'   - `Depth_top`, `Depth_bottom`, `Depth_middle`: calculated depths (cm).
#'   Rows without sufficient information are dropped.
#'
#' @details
#' The function groups data by `stratsection_name` and orders layers according
#'  to `stratlayer_order_start_at_top`. Depths are computed cumulatively if only
#'  thickness is provided, or taken directly from absolute depth columns if
#'  available.
#'
#' @examples
#' # Example data is included with the package
#' data("example_data_strat", package = "avstrat")
#'
#' # Order + thickness method (section "fake1")
#' df1 <- subset(example_data_strat, stratsection_name == "fake1")
#' add_depths(df1)
#'
#' # Start/stop depth method (section "fake3")
#' df2 <- subset(example_data_strat, stratsection_name == "fake3")
#' add_depths(df2)
#'
#' @importFrom rlang .data
#' @export
#'
add_depths <- function(df) {
  # Add required columns check
  required_always <- c("stratsection_name", "stratlayer_name", "stratmeasuremethod", "stratlayer_order_start_at_top")
  missing_always <- setdiff(required_always, names(df))
  if (length(missing_always) > 0) {
    stop("Missing required columns: ", paste(missing_always, collapse = ", "))
  }

  # Add missing expected columns with NA values
  other_expected_headers <- c(
    "thickness_units", "thickness_typical",
    "thickness_min", "thickness_max", "depth_units", "depth_top",
    "depth_bottom", "stratlayer_order"
  )
  missing <- setdiff(other_expected_headers, names(df))
  df[missing] <- NA

  # error if stratsection_name present but no stratmeasuremethod
  if (any(!is.na(df$stratsection_name) & is.na(df$stratmeasuremethod))) {
    stop("Rows with stratsection_name must define stratmeasuremethod.")
  }

  # error if invalid stratmeasuremethod
  valid_methods <- c("order and thickness", "start and stop depth")
  bad_methods <- setdiff(unique(na.omit(df$stratmeasuremethod)), valid_methods)
  if (length(bad_methods) > 0) {
    stop("Invalid stratmeasuremethod values: ", paste(bad_methods, collapse = ", "))
  }

  # error if stratsection_name present but no stratlayer_order_start_at_top
  if (any(!is.na(df$stratsection_name) & is.na(df$stratlayer_order_start_at_top))) {
    stop("Rows with stratsection_name must define stratlayer_order_start_at_top.")
  }

  # error if invalid stratlayer_order_start_at_top
  valid_flags <- c(TRUE, FALSE)  # logical values
  bad_start_at_top <- setdiff(unique(na.omit(df$stratlayer_order_start_at_top)), valid_flags)
  if (length(bad_start_at_top) > 0) {
    stop("Invalid stratlayer_order_start_at_top values: ",
         paste(bad_start_at_top, collapse = ", "))
  }

  # error if invalid units
  valid_units <- c("m", "meter", "meters", "cm", "centimeter", "centimeters",
                   "mm", "millimeter", "millimeters")
  bad_units <- df |>
    dplyr::filter(!is.na(.data$thickness_units) & !.data$thickness_units %in% valid_units |
                    !is.na(.data$depth_units) & !.data$depth_units %in% valid_units)
  if (nrow(bad_units) > 0) {
    stop("Invalid units detected in thickness_units or depth_units.\n",
         "Problematic rows:\n",
         paste(bad_units$stratsection_name, bad_units$stratlayer_name, sep = ":", collapse = "\n"))
  }

  # error if depth_top and depth_bottom not present when stratmeasuremethod == "start and stop depth"
  bad_rows1 <- df |>
    dplyr::filter(
      .data$stratmeasuremethod == "start and stop depth" &
        (is.na(.data$depth_top) | is.na(.data$depth_bottom) | is.na(.data$depth_units))
    )

  if (nrow(bad_rows1) > 0) {
    stop(
      "Rows with stratmeasuremethod 'start and stop depth' must define depth_top, depth_bottom, and depth_units.\n",
      "Problematic rows(s): ",
      paste(bad_rows1$stratsection_name, bad_rows1$stratlayer_name, collapse = "\n")
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
    stop(
      "Rows with stratmeasuremethod 'order and thickness' must define at least one of thickness_typical, thickness_min, or thickness_max, ",
      "and must define thickness_units and stratlayer_order.\n",
      "Problematic rows(s): ",
      paste(bad_rows2$stratsection_name, bad_rows2$stratlayer_name, collapse = "\n")
    )
  }

  # Duplicate orders
  dup_orders <- df |>
    dplyr::filter(.data$stratmeasuremethod == "order and thickness") |>
    dplyr::count(.data$stratsection_name, .data$stratlayer_order) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup_orders) > 0) {
    stop("Duplicate stratlayer_order values within sections:\n",
         paste(dup_orders$stratsection_name, dup_orders$stratlayer_order, sep = ":", collapse = "\n"))
  }

  # Depth range validation
  bad_depths <- df |>
    dplyr::filter(.data$stratmeasuremethod == "start and stop depth") |>
    tidyr::drop_na(dplyr::all_of(c("depth_top", "depth_bottom", "depth_units"))) |>
    dplyr::mutate(start_at_top = as.logical(.data[["stratlayer_order_start_at_top"]])) |>
    dplyr::filter(
      (is.na(.data[["start_at_top"]]) & .data[["depth_top"]] >= .data[["depth_bottom"]]) |
        (!is.na(.data[["start_at_top"]]) & .data[["start_at_top"]] & .data[["depth_top"]] > .data[["depth_bottom"]]) |
        (!is.na(.data[["start_at_top"]]) & !.data[["start_at_top"]] & .data[["depth_top"]] < .data[["depth_bottom"]])
    )

  if (nrow(bad_depths) > 0) {
    stop("Invalid depth ranges:\n",
         paste(bad_depths$stratsection_name, bad_depths$stratlayer_name, collapse = "\n"))
  }


  # internal function to convert to centimeters for consistency
  convert_to_cm <- function(value, units) {
    dplyr::case_when(
      units %in% c("cm", "centimeters", "centimeter") ~ as.numeric(value),
      units %in% c("mm", "millimeters", "millimeter") ~ as.numeric(value) / 10,
      units %in% c("m", "meters", "meter") ~ as.numeric(value) * 100,
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
      start_at_top    = as.logical(.data[["stratlayer_order_start_at_top"]])
    ) |>
    dplyr::group_by(.data$stratsection_name) |>
    dplyr::mutate(
      # For inversion, use the maximum uploaded TOP depth per section
      section_height_cm = max(.data[["depth_top_cm"]], na.rm = TRUE),
      Depth_top = dplyr::if_else(
        .data[["start_at_top"]],
        .data[["depth_top_cm"]],
        .data[["section_height_cm"]] - .data[["depth_top_cm"]]
      ),
      Depth_bottom = dplyr::if_else(
        .data[["start_at_top"]],
        .data[["depth_bottom_cm"]],
        .data[["section_height_cm"]] - .data[["depth_bottom_cm"]]
      ),
      Depth_middle = .data[["Depth_top"]] + (.data[["Depth_bottom"]] - .data[["Depth_top"]]) / 2
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Final invariant: outputs must have Depth_top < Depth_bottom
      .depth_ok = .data[["Depth_top"]] < .data[["Depth_bottom"]]
    ) |>
    dplyr::filter(.data[[".depth_ok"]]) |>
    dplyr::select(-.data[[".depth_ok"]])
  ) |>
  # Clean up columns not needed going forward
  dplyr::select(
    -dplyr::any_of(c(
      "thickness_defining_cm", "thickness_min_cm", "thickness_max_cm", "thickness_plot",
      "depth_top_cm", "depth_bottom_cm"
    ))
  )
}
