#' Required stratigraphic categories
#'
#' These are the canonical categories that all stratigraphic palettes
#' must cover. They are used by [validate_stratpal()] to ensure
#' palettes are complete.
#'
#' @keywords internal
.required_strat_categories <- c(
  "volcanic",
  "tephra fall",
  "lava",
  "intrusion",
  "tuff",
  "sediment",
  "soil",
  "peat",
  "lacusterine",
  "fluvial",
  "eolian",
  "diamict",
  "clay",
  "pyroclastic density current",
  "pyroclastic surge",
  "pyroclastic flow",
  "mass wasting",
  "debris flow",
  "lahar",
  "landslide",
  "hyperconcentrated flow",
  "debris avalanche",
  "frozen water",
  "ice",
  "snow",
  "dirty snow",
  "plant",
  "rock",
  "other",
  "undifferentiated/undescribed"
)

#' Validate a stratigraphic palette
#'
#' Ensures that a palette covers all required categories. By default,
#' missing categories trigger an error. If `allow_na = TRUE`, missing
#' categories are filled with a default color instead.
#'
#' @param pal Named character vector of colors.
#' @param allow_na Logical. If TRUE, missing categories are filled with
#'   `na_color` instead of erroring.
#' @param na_color Color to use for missing categories when
#'   `allow_na = TRUE`.
#'
#' @return A complete palette (named character vector) ordered to match
#'   the required categories.
#' @export
#'
#' @examples
#' # A complete palette passes validation
#' validate_stratpal(stratpal_rpg)
#'
#' # Allow missing categories to be filled with gray
#' validate_stratpal(stratpal_grays, allow_na = TRUE)
validate_stratpal <- function(pal, allow_na = FALSE, na_color = "gray90") {
  missing <- setdiff(.required_strat_categories, names(pal))

  if (length(missing) > 0) {
    if (allow_na) {
      filler <- stats::setNames(rep(na_color, length(missing)), missing)
      pal <- c(pal, filler)
    } else {
      stop(
        "Palette is missing required categories: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }

  pal[.required_strat_categories]
}

#' Stratigraphic palettes
#'
#' Named character vectors of hex colors for stratigraphic plotting.
#' These palettes can be passed to [scale_fill_stratpal()].
#'
#' @format Named character vectors
#' @examples
#' stratpal_rpg["volcanic"]
#' stratpal_grays["soil"]
#'
#' @name stratpalettes
NULL

#' @rdname stratpalettes
#' @export
stratpal_rpg <- c(
  `volcanic` = "red",
  `tephra fall` = "darkred",
  `lava` = "gray10",
  `intrusion` = "gray10",
  `tuff` = "red",
  `sediment` = "gray70",
  `soil` = "gray50",
  `peat` = "gray50",
  `lacusterine` = "gray70",
  `fluvial` = "gray70",
  `eolian` = "gray70",
  `diamict` = "gray70",
  `clay` = "gray70",
  `pyroclastic density current` = "mediumpurple",
  `pyroclastic surge` = "mediumpurple",
  `pyroclastic flow` = "mediumpurple",
  `mass wasting` = "tan",
  `debris flow` = "tan",
  `lahar` = "tan",
  `landslide` = "tan",
  `hyperconcentrated flow` = "tan",
  `debris avalanche` = "tan",
  `frozen water` = "lightblue",
  `ice` = "lightblue",
  `snow` = "white",
  `dirty snow` = "gray80",
  `plant` = "darkgreen",
  `rock` = "gray10",
  `other` = "gray30",
  `undifferentiated/undescribed` = "gray30"
  )

#' @rdname stratpalettes
#' @export
stratpal_grays <- c(
  `volcanic` = "gray25",
  `tephra fall` = "gray25",
  `lava` = "gray5",
  `intrusion` = "gray5",
  `tuff` = "gray25",
  `sediment` = "white",
  `soil` = "white",
  `peat` = "white",
  `lacusterine` = "white",
  `fluvial` = "white",
  `eolian` = "white",
  `diamict` = "white",
  `clay` = "white",
  `pyroclastic density current` = "gray50",
  `pyroclastic surge` = "gray50",
  `pyroclastic flow` = "gray50",
  `mass wasting` = "tan",
  `debris flow` = "tan",
  `lahar` = "tan",
  `landslide` = "tan",
  `hyperconcentrated flow` = "tan",
  `debris avalanche` = "tan",
  `frozen water` = "lightblue",
  `ice` = "lightblue",
  `snow` = "white",
  `dirty snow` = "gray80",
  `plant` = "darkgreen",
  `rock` = "gray5",
  `other` = "white",
  `undifferentiated/undescribed` = "white"
)

#' Internal list of stratigraphic palettes
#'
#' This list collects all palettes so they can be looked up by name.
#' Add new palettes here as you create them.
#'
#' @keywords internal
.stratpalettes <- list(
  stratpal_rpg   = stratpal_rpg,
  stratpal_grays = stratpal_grays
)

#' Stratigraphic fill scale
#'
#' A ggplot2 fill scale that uses one of the built-in stratigraphic palettes.
#'
#' @param palette Character string naming which palette to use.
#'   Options are names of palettes in `.stratpalettes`.
#' @param overrides Optional named character vector of colors to override
#'   entries in the chosen palette.
#' @param allow_na Logical. If TRUE, missing categories are filled with
#'   `na_color` instead of erroring.
#' @param na_color Color to use for missing categories when
#'   `allow_na = TRUE`.
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()].
#'
#' @return A ggplot2 scale object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_stratpal("stratpal_rpg")
#'
#' # Override one color
#' scale_fill_stratpal("stratpal_rpg", overrides = c("volcanic" = "orange"))
#'
#' # Allow missing categories to be filled with gray
#' scale_fill_stratpal("stratpal_grays", allow_na = TRUE)
scale_fill_stratpal <- function(palette = c("stratpal_rpg", "stratpal_grays"),
                                overrides = NULL,
                                allow_na = FALSE,
                                na_color = "gray90",
                                ...) {
  palette <- match.arg(palette)
  pal <- .stratpalettes[[palette]]

  # Apply overrides
  if (!is.null(overrides)) {
    stopifnot(is.character(overrides))
    pal[names(overrides)] <- overrides
  }

  # Validate against required categories
  pal <- validate_stratpal(pal, allow_na = allow_na, na_color = na_color)

  ggplot2::scale_fill_manual(values = pal, ...)
}
