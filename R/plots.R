#' Plot a grainsize-depth stratigraphic section
#'
#' Uses ggplot2 to create a grainsize vs. depth stratigraphic section plot.
#'
#' @param df A data frame containing stratigraphic data.
#'   Must include columns \code{stratsection_name}, \code{stratlayer_order},
#'   \code{grainsize}, \code{depth}, and \code{stratlayer_type}.
#' @param stratsection_name Character string giving the section name to filter.
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
#' @param grainsize_labs Character vector of labels for the x-axis. Several predefined options are available:
#'  \itemize{
#'    \item \code{gs_volc_abbr}: Volcanic grainsize abbreviations (default).
#'    \item \code{gs_sed_abbr}: Sedimentary grainsize abbreviations.
#'    \item \code{gs_volc_names}: Volcanic grainsize full names.
#'    \item \code{gs_sed_names}: Sedimentary grainsize full names.
#'    \item \code{gs_numeric}: Numeric grainsize labels.
#'  }
#' @param xlim Numeric vector of length 2 giving x-axis limits.
#' @param ylim Numeric vector of length 2 giving y-axis limits (optional).
#' @param ybreaks Number of breaks on the y-axis.
#' @param layer_fill Character string naming the column to use for fill. If
#'  using anything other than "layer_type" from the template, will need to make
#'  a new palette.
#' @param layer_fill_color Palette object to use for fill colors.
#' @param layer_border_color Border color for polygons.
#' @param layer_border_linewidth Border line width for polygons.
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_polygon aes scale_fill_manual scale_y_continuous
#'   scale_x_continuous coord_cartesian ggtitle theme element_text unit guides guide_legend
#'   element_blank
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' example_data_strat |>
#'  add_depths() |>
#'  ggstrat(stratsection_name = "21LSHD02")
ggstrat <- function(df,
                    stratsection_name,
                    grainsize_direction = c("increasing", "decreasing"),
                    grainsize_labs = gs_volc_abbr,
                    xlim = c(-1, 10),
                    ylim = NULL,
                    ybreaks = 7,
                    layer_fill = "layer_type",
                    layer_fill_color = "stratpal_rpg",
                    layer_border_color = 'black',
                    layer_border_linewidth = 0.2

) {

  # Axis label options
  gs_volc_abbr <- c("NA", "c/s", "vfa", "fa", "ma", "ca", "vca", "fl", "ml", "cl", "b")
  gs_sed_abbr <- c("NA", "c/s", "vfs", "fs", "ms", "cs", "vcs", "gra", "pbl", "cbl", "bldr")
  gs_volc_names <- c("NA", "clay/silt", "very fine ash", "fine ash", "medium ash",
                     "coarse ash", "very coarse ash", "fine lapilli", "medium lapilli",
                     "coarse lapilli", "blocks/bombs")
  gs_sed_names <- c("NA", "clay/silt", "very fine sand", "fine sand", "medium sand",
                    "coarse sanb", "very coarse sand", "granule", "pebble",
                    "cobble", "boulders")
  gs_numeric <- c("NA", "<1/16 mm", "1/16-1/8 mm", "1/8-1/4 mm", "1/4-1/2 mm",
                  "1/2-1 mm", "1-2 mm", "2-4 mm", "4-16 mm", "1.6-6.4 cm", ">6.4 cm")
  # grainsize_labs options
  grainsize_direction <- match.arg(grainsize_direction)
  x_breaks <- if (grainsize_direction == "increasing") 0:10 else 10:0
  # First, filter data to the station you want to plot
  data_plot <- df |>
    dplyr::filter({{ stratsection_name }} == stratsection_name) |>
    # Add depth info
    add_depths() |>
    # Prepare data for geom_polygon layer of a strat section.
    add_layer_width(grainsize_direction = grainsize_direction) |>
    dplyr::arrange(.data$stratlayer_order, .data$size_loc)

  plot <- ggplot(data_plot) +
    geom_polygon(
      aes(group = .data[["stratlayer_order"]],
          x = .data[["grainsize"]],
          y = .data[["depth"]],
          fill = .data[[layer_fill]]),
      rule = "winding",
      color = layer_border_color,
      linewidth = layer_border_linewidth) +
    scale_fill_stratpal(palette = layer_fill_color) +
    scale_y_continuous(name = "Depth (cm)",
                       n.breaks = ybreaks,
                       trans = "reverse") +
    scale_x_continuous(limits = xlim,
                       name = "Grainsize",
                       breaks = x_breaks,
                       labels = grainsize_labs) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    expand = F) +
    ggtitle(stratsection_name) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
          plot.margin = unit(c(0.1, 1, 0.1, 0.1), "cm")) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  return(plot)
}


#' Plot a simple stratigraphic column
#'
#' Uses ggplot2 to create a simple depth-only stratigraphic section plot with no
#' variable mapped to the x-axis. Each layer is drawn as a fixed-width rectangle.
#'
#'
#' @param df A data frame containing stratigraphic data.
#'   Must include columns \code{stratsection_name}, \code{stratlayer_order},
#'   \code{grainsize}, \code{depth}, and \code{stratlayer_type}.
#' @param stratsection_name Character string giving the section name to filter.
#' @param ylim Numeric vector of length 2 giving y-axis limits (optional).
#' @param ybreaks Number of breaks on the y-axis.
#' @param layer_fill Character string naming the column to use for fill. If
#'  using anything other than "layer_type" from the template, will need to make
#'  a new palette.
#' @param layer_fill_color Palette object to use for fill colors.
#' @param layer_border_color Border color for polygons.
#' @param layer_border_linewidth Border line width for polygons.
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_rect aes scale_fill_manual scale_y_continuous
#'   scale_x_continuous coord_cartesian ggtitle theme element_text unit guides guide_legend
#'   element_blank
#'
#' @returns A ggplot object showing a schematic stratigraphic column.
#' @export
#'
#' @examples
#' example_data_strat |>
#'   ggstrat_column(stratsection_name = "21LSHD02")
ggstrat_column <- function(df,
                           stratsection_name,
                           ylim = NULL,
                           ybreaks = 7,
                           layer_fill = "layer_type",
                           layer_fill_color = "stratpal_rpg",
                           layer_border_color = "black",
                           layer_border_linewidth = 0.2) {
  # First, filter data to the station you want to plot
  data_plot <- df |>
    dplyr::filter({{ stratsection_name }} == stratsection_name) |>
    # Add depth info
    add_depths()

  plot <- ggplot(data = data_plot) +
    geom_rect(
      aes(
        group = .data[["stratlayer_order"]], xmin = 0, xmax = 0.1,
        ymin = .data[["Depth_bottom"]], ymax = .data[["Depth_top"]],
        fill = factor(.data[[layer_fill]])
      ), # Fix: stratlayer_order might not be defined for depth explicit template
      color = layer_border_color,
      linewidth = layer_border_linewidth
    ) +
    scale_fill_stratpal(palette = layer_fill_color) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(
      name = "Depth (cm)",
      n.breaks = ybreaks,
      trans = "reverse"
    ) +
    coord_cartesian(
      ylim = ylim,
      expand = F
    ) +
    ggtitle(data_plot$stratsection_name) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x.bottom = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
      plot.margin = unit(c(0.1, 1.25, 0.1, 0.1), "cm")
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  return(plot)
}


#' Plots text lables alongside a stratigraphic section
#'
#' Uses ggplot2 to plot any character column associated with stratigraphic data
#' such as SampleID at the correct depths for their corresponding
#' layers. Connecting lines extend to the left of the plot to point to plotted
#' layers. It is designed to be combined with a stratigraphic section plot created by
#' [ggstrat()] using the [patchwork::patchwork] framework for arranging multiple
#' ggplot objects.
#'
#' @param df A data frame containing stratigraphic data.
#'   Must include columns \code{stratsection_name}, \code{stratlayer_order},
#'   \code{grainsize}, \code{depth}, and the column specified by \code{layer_fill}.
#' @param label Character strin gnaming the column to use for labels. Default is "SampleID".
#' @param stratsection_name Character string giving the section name to filter.
#' @param ylim Numeric vector of length 2 giving y-axis limits (optional).
#' @param ybreaks Number of breaks on the y-axis.
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_rect aes scale_fill_manual scale_y_continuous
#'   scale_x_continuous coord_cartesian ggtitle theme element_text unit guides guide_legend
#'   element_blank geom_text geom_segment
#'
#' @returns A ggplot object showing SampleIDs plotted by depth in section.
#' @export
#'
#' @examples
#' # Example 1: Basic usage
#' example_data_strat |>
#'   ggstrat_label(stratsection_name = "21LSHD02",
#'               label = "SampleID")
#'
#' # Example 2: Combine with a stratigraphic section plot using patchwork
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   stratsection <- example_data_strat |>
#'     ggstrat(stratsection_name = "21LSHD02")
#'
#'   samples <- example_data_strat |>
#'     ggstrat_label(stratsection_name = "21LSHD02",
#'               label = "SampleID")
#'
#'   stratsection + samples
#' }
#'
ggstrat_label <- function(df,
                          stratsection_name,
                          label = "SampleID",
                          ylim = NULL,
                          ybreaks = 7) {
  # First, filter data to the station you want to plot
  data_plot <- df |>
    dplyr::filter({{ stratsection_name }} == stratsection_name) |>
    # Add depth info
    add_depths()
  # Second, filter a data frame with only samples to plot
  label_plot <- data_plot |>
      add_depths() |>
    tidyr::drop_na(label)
  # Now we can make the plot
  plot <- ggplot(data = data_plot) +
    geom_text(data = label_plot,
              aes(x = 0.2,
                  y = .data[["Depth_middle"]],
                  label = .data[[label]]),
              size = 2, hjust = "left") +
    geom_segment(data = label_plot,
                 aes(x = -5,
                     xend = 0.1,
                     y = .data[["Depth_middle"]]),
                 linewidth = 0.3, alpha = 0.3) +
    ggtitle(label) +
    scale_y_continuous(
      trans = "reverse",
      n.breaks = ybreaks
    ) +
    coord_cartesian(
      ylim = c(max(data_plot$Depth_bottom), min(data_plot$Depth_top)), expand = F,
      xlim = c(0, 5),
      clip = "off"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 10, face = "bold"),
      legend.title = element_blank(),
      axis.title.x.bottom = element_blank(),
      axis.text.x.bottom = element_blank(),
      axis.title.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.line.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  return(plot)
}
