#' Custom theme for stratigraphic plots
#'
#' A ggplot2 theme designed to work well with typical plot ouput from avstrat,
#' and matching the author's preferred aesthetics.
#'
#' @param base_size Base font size. Defaults to 11.
#' @param base_family Base font famil. Defaults to Arial.
#' @importFrom ggplot2 %+replace%
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' # Apply a custom theme to one plot
#' ggstrat(df = example_data_strat, stratsection_name = "21LSHD02") +
#'   theme_avstrat()
#'
#'   #' # Set the custom theme as default for all plots
#'   ggplot2::theme_set(theme_avstrat())
#'   ggstrat(df = example_data_strat, stratsection_name = "21LSHD02")
theme_avstrat <- function(base_size = 11, base_family = "arial") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      line = ggplot2::element_line(color = "black", linewidth = 0.5),
      text = ggplot2::element_text(color = "black"),
      axis.text.x.top = ggplot2::element_blank(),
      axis.title.x.top = ggplot2::element_blank(),
      axis.title.x.bottom = ggplot2::element_text(color = "black", size = 10, face = "bold"),
      axis.text.x.bottom = ggplot2::element_text(color = "black", size = 8, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y.right = ggplot2::element_blank(),
      axis.title.y.right = ggplot2::element_blank(),
      axis.title.y.left = ggplot2::element_text(color = "black", size = 10, face = "bold"),
      axis.text.y.left = ggplot2::element_text(color = "black", size = 8),
      axis.ticks.length = grid::unit(0.15, "cm")
    )
}
