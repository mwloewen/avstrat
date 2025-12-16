
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avstrat <img src="man/figures/logo.png" align="right" height="200"/>

Tools for Generating Stratigraphic Sections

This package provides tools for data processing and generating
stratigraphic sections for volcanic deposits and tephrastratigraphy.
Package was developed for studies on Alaska volcanoes (“av”) where
stratigraphic (“strat”) figures are needed for interpreting eruptive
histories, but the methods are applicable to any sediment stratigraphy
project. The primary outputs are ggplot figures of stratigraphic
sections–`ggstrat()`, `ggstrat_column()`, `ggstrat_sampleID()`-but the
data processing logic `add_depths()` and `add_layer_widths()` enable
motivated users to create custom visualizations. Various load_data
functions facilitate ingesting from stratigraphic layer data templates.

## Installation

You can install avstrat from CRAN \[when/if it is accepted there\] or
from a downloaded source file off the GitLab approved repository (Code -
Download source code - tar.gz). For option 2, you’ll need to download
the source repo as a tar.gz and map the path to the location on your
computer.

``` r
{r eval=FALSE}
# Option 1: Install from CRAN:
# install.packages("avstrat") # this will only work once it is on CRAN

# Option 2: Install from source file:
install.packages("path/to/avstrat_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

To install the development version, use the ‘devtools’ package. \[This
also won’t work until the code is public\].

``` r
devtools::install_gitlab("vsc/tephra/tools/avstrat", 
                        host = "code.usgs.gov", 
                        build_vignettes = TRUE)
```

You can also install the development version from a mirrored repo on
Github \[This will work now and is the easiest way to install the
package until the repo is public\].

``` r
devtools::install_github(
  "https://github.com/mwloewen/avstrat",
  auth_token = "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", #The auth token will not be needed once this is reviwed and public, it is only here while the repo is private before approval
  build_vignettes = TRUE
)
```

## Example

To upload data from Geodiva submission templates (Stations-Samples and
Layers):

``` r
library(readxl)
library(avstrat)

data_strat <- load_geodiva_forms(
   station_sample_upload = 
     readxl::read_excel("path_samples.xlsx", sheet = "Data"),
   layer_upload = 
     readxl::read_excel("path_layers.xlsx", sheet = "Data")
 )
```

Once you have the data in your environment, you can make a basic
stratigraphic plot and use the avstrat theme:

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.5.2
library(readxl)
library(avstrat)
# Load data
data_strat <- example_data_strat

# Set theme
theme_set(theme_avstrat())

# Produce a strat section
ggstrat(df = data_strat, section_name = '21LSHD02')
```

<img src="man/figures/README-example_basic_grainsize_plot-1.png" width="31%" />

The default `ggstrat()` plot emphasizes coarser grained deposits as
larger bars, useful for recognizing significant tephra deposits. Many
prefer stratigraphic profiles to reflect the erosional profile of an
outcrop. This would be best achieved by recording a “erodability” metric
in the field, but a rough version of this is to plot the fine-grained
layers as thicker than coarse grained layers, as these typically are
less prone to erosion.

``` r
ggstrat(df = data_strat, section_name = '21LSHD02', grainsize_direction = "decreasing")
```

<img src="man/figures/README-example_reverse_grainsize_plot-1.png" width="31%" />

You can also plot sample identification along side the section. and
combine them with the patchwork package.

``` r
library(patchwork)
#> Warning: package 'patchwork' was built under R version 4.5.2
p1 <- ggstrat(df = data_strat, section_name = '21LSHD02')
p2 <- ggstrat_label(df = data_strat, section_name = '21LSHD02')

p1 + p2
#> Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
#> font family not found in Windows font database
```

<img src="man/figures/README-example_combined_sample_plot-1.png" width="62.5%" />

More examples and demonstration of how to create your own custom plots
will be provided \[eventually\] in vignettes! Currently, a more detailed
workflow can be accessed with the installed package at:

``` r
vignette("avstrat-workflow-examples", package = "avstrat")
```
