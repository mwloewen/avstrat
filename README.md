
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avstrat: Tools for Generating Stratigraphic Sections

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/mwloewen/avstrat/graph/badge.svg)](https://app.codecov.io/gh/mwloewen/avstrat)
[![R-CMD-check](https://github.com/mwloewen/avstrat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mwloewen/avstrat/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package provides tools for data processing and generating
stratigraphic sections for volcanic deposits and tephrastratigraphy.
Package was developed for studies on Alaska volcanoes (“av”) where
stratigraphic (“strat”) figures are needed for interpreting eruptive
histories, but the methods are applicable to any sediment stratigraphy
project. The primary outputs are ggplot figures of stratigrahaphic
sections–`ggstrat()`, `ggstrat_column()`, `ggstrat_sampleID()`-but the
data processing logic `add_depths()` and `add_layer_widths()` enable
motivated users to create custom visualizations. Various load_data
functions facilitate ingesting from stratigraphic layer data templates.

## Installation

You can install the development version of avstrat from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mwloewen/avstrat")
```

This currently does not work universally, since the repo is currently
private. I will document installation instructions when it moves to the
USGS gitlab. This current code will only work once the package has USGS
software approval and can be listed as public.

## Example

To upload data from Geodiva submission templates (Stations-Samples and
Layers):

``` r
library(avstrat)
library(readxl)

data_strat <- load_geodiva_forms(
   station_sample_upload = 
     readxl::read_excel("path_samples.xlsx", sheet = "Data"),
   layer_upload = 
     readxl::read_excel("path_layers.xlsx", sheet = "Data")
 )
```

Once you have the data in your environment, you can make a basic
stratigraphic plot:

``` r
library(avstrat)
library(readxl)
# Load data
data_strat <- example_data_strat

# Produce a strat section
ggstrat(df = data_strat, stratsection_name = '21LSHD02')
```

<img src="man/figures/README-example_basic_grainsize_plot-1.png" width="100%" />

The default `ggstrat()` plot emphasizes coarser grained deposits as
larger bars, useful for recognizing significant tephra deposits. Many
prefer stratigraphic profiles to reflect the erosional profile of an
outcrop. This would be best achieved by recording a “erodability” metric
in the field, but a rough version of this is to plot the fine-grained
layers as thicker than coarse grained layers, as these typically are
less prone to erosion.

``` r
ggstrat(df = data_strat, stratsection_name = '21LSHD02', grainsize_direction = "decreasing")
```

<img src="man/figures/README-example_reverse_grainsize_plot-1.png" width="100%" />

You can also plot sample identification along side the section. and
combine them with the patchwork package.

``` r
library(patchwork)
p1 <- ggstrat(df = data_strat, stratsection_name = '21LSHD02')
p2 <- ggstrat_sampleID(df = data_strat, stratsection_name = '21LSHD02')

p1 + p2
```

<img src="man/figures/README-example_combined_sample_plot-1.png" width="100%" />

More examples and demonstration of how to create your own custom plots
will be provided \[eventually\] in vignettes!
