#' Example GeoDIVA forms data
#'
#' A dataset of forms loaded with \code{load_geodiva_forms()}.
#' Useful for demonstrating plotting and analysis functions in this package.
#'
#' @format ## `example_data_strat`
#' A data frame with 244 rows and 39 columns:
#' \describe{
#'   \item{stratsection_name}{Character. Name/identifier of the stratigraphic section.}
#'   \item{stratlayer_name}{Character. Name/identifier of the stratigraphic layer.}
#'   \item{date_described}{Date. Date the section was described.}
#'   \item{date_described_timezone}{Character. Time zone of the description date.}
#'   \item{stratlayer_desc}{Character. Free‑text description of the layer.}
#'   \item{stratlayer_order}{Integer. Order of the layer within the section.}
#'   \item{thickness_units}{Character. Units for thickness (e.g., "millimeters", "centimeters", "meters").}
#'   \item{thickness_typical}{Numeric. Typical thickness of the layer.}
#'   \item{thickness_min}{Numeric. Minimum thickness of the layer.}
#'   \item{thickness_max}{Numeric. Maximum thickness of the layer.}
#'   \item{depth_units}{Character. Units for depth (see thickness_units).}
#'   \item{depth_top}{Numeric. Absolute depth to the top of the layer.}
#'   \item{depth_bottom}{Numeric. Absolute depth to the bottom of the layer.}
#'   \item{depth_uncertainty_top}{Numeric. Uncertainty in top depth.}
#'   \item{depth_uncertainty_bottom}{Numeric. Uncertainty in bottom depth.}
#'   \item{layer_type}{Character/Factor. Lithology or depositional type of the layer (e.g., "tephra fall", "soil").}
#'   \item{stratlayer_color}{Character. Color description.}
#'   \item{grainsize_top}{Numeric/Character. Grain size at the top of the layer.}
#'   \item{grainsize_bottom}{Numeric/Character. Grain size at the bottom of the layer.}
#'   \item{stratlayer_grading}{Character. Grading description (e.g. normal, reverse).}
#'   \item{contact_lower}{Character. Description of the lower contact.}
#'   \item{contact_upper}{Character. Description of the upper contact.}
#'   \item{stratlayer_sorting}{Character. Sorting description.}
#'   \item{stratlayer_support}{Character. Support description (matrix or clast).}
#'   \item{tephra_concentration}{Numeric/Character. Tephra concentration.}
#'   \item{stratlayer_unit}{Character. Stratigraphic unit designation.}
#'   \item{tephra_name}{Character. Formal tephra name.}
#'   \item{tephra_guess}{Character. Tentative tephra identification.}
#'   \item{volcano_name}{Character. Source volcano name. If multiple source volcanoes separated by "|".}
#'   \item{eruption_name}{Character. Source eruption name.}
#'   \item{stratlayer_sample}{Character. Sample identifier. If multiple samples separated by "|".}
#'   \item{station_id}{Character. Station identifier.}
#'   \item{stratmeasuremethod}{Character. Method used for stratigraphic measurement.}
#'   \item{stratlayer_order_start_at_top}{Logical. Whether ordering starts at the top.}
#'   \item{section_notes}{Character. Free‑text notes about the section.}
#'   \item{Latdd}{Numeric. Latitude in decimal degrees.}
#'   \item{Longdd}{Numeric. Longitude in decimal degrees.}
#'   \item{LocationDesc}{Character. Location description.}
#'   \item{SampleID}{List. Nested list column of sample IDs per layer.}
#' }
#' @source <https://doi.org/10.14509/31084>
#' @source <https://doi.org/10.14509/31090>
"example_data_strat"

#' Example stratigraphic data from individual tables
#'
#' A dataset created by loading example inputs with \code{load_stratdata_indiv()}.
#' This demonstrates the structure of stratigraphic data when stations, sections,
#' layers, and samples are provided as separate tables and then merged. It shares
#' many column definitions with \code{\link{example_data_strat}} but contains a
#' reduced set of fields.
#'
#' @format ## `example_data_indiv`
#' A data frame with 244 rows and 23 columns:
#' \describe{
#'   \item{stratsection_name}{Character. Name/identifier of the stratigraphic section.}
#'   \item{stratlayer_name}{Character. Name/identifier of the stratigraphic layer.}
#'   \item{stratlayer_desc}{Character. Free‑text description of the layer.}
#'   \item{stratlayer_order}{Integer. Order of the layer within the section.}
#'   \item{thickness_units}{Character. Units for thickness (e.g., "millimeters", "centimeters", "meters").}
#'   \item{thickness_typical}{Numeric. Typical thickness of the layer.}
#'   \item{thickness_min}{Numeric. Minimum thickness of the layer.}
#'   \item{thickness_max}{Numeric. Maximum thickness of the layer.}
#'   \item{depth_units}{Character. Units for depth (see thickness_units).}
#'   \item{depth_top}{Numeric. Absolute depth to the top of the layer.}
#'   \item{depth_bottom}{Numeric. Absolute depth to the bottom of the layer.}
#'   \item{layer_type}{Character/Factor. Lithology or depositional type of the layer (e.g., "tephra fall", "soil").}
#'   \item{stratlayer_color}{Character. Color description.}
#'   \item{grainsize_top}{Numeric/Character. Grain size at the top of the layer.}
#'   \item{grainsize_bottom}{Numeric/Character. Grain size at the bottom of the layer.}
#'   \item{volcano_name}{Character. Source volcano name. If multiple, separated by "|".}
#'   \item{station_id}{Character. Station identifier (links back to station metadata).}
#'   \item{stratmeasuremethod}{Character. Method used for stratigraphic measurement.}
#'   \item{stratlayer_order_start_at_top}{Logical. Whether ordering starts at the top.}
#'   \item{Latdd}{Numeric. Latitude in decimal degrees.}
#'   \item{Longdd}{Numeric. Longitude in decimal degrees.}
#'   \item{stratlayer_sample}{Character. Collapsed sample identifiers per layer, separated by "|".}
#'   \item{SampleID}{List. Nested list column of sample IDs per layer.}
#' }
#'
#' @seealso \code{\link{example_data_strat}} for a more complete dataset
#'   including additional descriptive fields.
#'
"example_data_indiv"
