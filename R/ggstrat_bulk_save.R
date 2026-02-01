#' Bulk save stratigraphic plots for all uploaded sections
#'
#' Generate and save a series of stratigraphic plots, one per unique
#' stratigraphic section in the input data frame. The user can supply
#' any plotting function that returns a ggplot object (e.g. one of the
#' package's plotting functions, or a custom function).
#'
#' @param df A data frame containing stratigraphic data.
#'   Must include columns \code{stratsection_name}, and any other variables needed
#'   for the plotfunction, such as: \code{stratlayer_order}, \code{grainsize},
#'   \code{depth}, and \code{stratlayer_type}.
#' @param plotfunction A function that generates a plot for a single
#'   section. It should accept at least two arguments: the full data
#'   frame (`df`) and a section identifier (`stratsection_name`).
#'   Defaults to [ggstrat()].
#' @param outdir Directory where plots will be saved. Suggest supplying a name
#'   such as `"StratSectionsPlotted"`, will create directory if it
#'   does not exist.
#' @param file_type File extension for saved plots (e.g. `"png"`,
#'   `"pdf"`). Defaults to `"png"`.
#' @param dpi Plot resolution in dots per inch. Can be a numeric value
#'   (e.g. `300`) or one of `"screen"`, `"print"`, or `"retina"`.
#'   Defaults to `300`.
#' @param width Plot width passed to [ggplot2::ggsave()]. Defaults to `4`.
#' @param height Plot height passed to [ggplot2::ggsave()]. Defaults to `8`.
#' @param units Units for `width` and `height`. One of `"in"`, `"cm"`,
#'   or `"mm"`. Defaults to `"in"`.
#' @param ask Logical. If `TRUE` (default) and running interactively,
#'   the function will prompt the user to confirm before generating
#'   and saving all plots.
#' @param ... Additional arguments passed on to `plotfunction`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of
#'   saving plot files to disk.
#' @export
#'
#' @examples
#' \donttest{
#' # Save plots for each section using the default ggstrat() function
#' td <- tempdir()
#' ggstrat_bulk_save(example_data_strat, outdir = td)
#'
#' # Save plots using a different plotting function
#' ggstrat_bulk_save(example_data_strat,
#'                   plotfunction = ggstrat_column,
#'                   outdir = td)
#'
#' # Save plots with higher resolution
#' ggstrat_bulk_save(example_data_strat,
#'                   outdir = td,
#'                   dpi = 600)
#'
#' # Optional cleanup
#' unlink(list.files(td, full.names = TRUE))
#' }
ggstrat_bulk_save <- function(df,
                              plotfunction = ggstrat,
                              outdir = NULL,
                              file_type = "png",
                              dpi = 300,
                              width = 4,
                              height = 8,
                              units = "in",
                              ask = TRUE,
                              ...) {
  # Require user to explicitly supply an output directory
  if (is.null(outdir)) {
    stop("Please supply 'outdir' to specify where plots should be saved.",
         call. = FALSE)
  }
    # Creates a folder to save plots in and save the plots as the StationID name.
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  # Get list of unique sections
  SectionList <- sort(c(unique(df$stratsection_name)))
  nplots <- length(SectionList)

  # Get full saving path
  full_outdir <- normalizePath(outdir, winslash = "/", mustWork = FALSE)

  # Ask for confirmation if interactive
  if (ask && interactive()) {
    message(sprintf("This will generate %d plots and save them into '%s'.", nplots, full_outdir))
    ans <- readline("Proceed? [Y/N]: ")
    if (!tolower(ans) %in% c("y", "yes")) {
      message("Operation cancelled by user.")
      return(invisible(NULL))
    }
  }

  for (i in SectionList) {
    # Call the plotting function, passing df and the current section
    p <- plotfunction(df,
      section_name = i,
      ...
    )

    # Save the plot
    ggplot2::ggsave(
      filename = paste0(i, ".", file_type),
      device = file_type,
      plot = p,
      path = outdir,
      dpi = dpi,
      width = width,
      height = height,
      units = units
    )
  }

  message("All plots saved successfully.")
}
