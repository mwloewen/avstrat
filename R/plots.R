#' Plot a grainsize-depth stratigraphic section
#'
#' Uses ggplot2 to create a grainsize vs. depth stratigraphic section plot.
#'
#' @param df A data frame containing stratigraphic data.
#'   Must include columns \code{stratsection_name}, \code{stratlayer_order},
#'   \code{grainsize}, \code{depth}, and the column specified by \code{layer_fill}.
#' @param stratsection_name Character string giving the section name to filter.
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

  # First, filter data to the station you want to plot
  data_plot <- df |>
    dplyr::filter({{ stratsection_name }} == stratsection_name) |>
    # Prepare data for geom_polygon layer of a strat section.
    add_layer_width() |>
    dplyr::arrange(.data[["stratlayer_order"]], .data[["size_loc"]])

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
                       breaks = c(0,1,2,3,4,5,6,7,8,9,10),
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
