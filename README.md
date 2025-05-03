
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipestats <img src='data-raw/pipestats_sticker.png' align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ganesko/pipestats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ganesko/pipestats/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R Package for Gas Pipeline Incident Data

This package cleans, joins, and documents gas pipeline incident data
from 1970 to present day from the Pipeline and Hazardous Materials
Safety Administration.

## Installation

You can install the development version of pipestats from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ganesko/pipestats")
```

You can load the package with:

``` r
library(pipestats)
```

## Example

The cleaned and joined dataset capabilities provided by this package
make many different visual analyses of gas pipeline data possible. The
following visualization is a 2025 version of Fig. 1 in the 2022 Dixon
et. al. paper titled State-of-the-Art Review of Performance Objectives
for Legacy Gas Pipelines with Pipe-in-Pipe Rehabilitation Technologies.
With the load_recent_data() and merge_data() functions in this package,
users can recreate even more up-to-date versions of this visualization
and others for in-depth analysis of gas pipeline incident causes and
variable relationships.

``` r
# load packages
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(scales)
library(paletteer)

# filter for mains
data(cleaned_full_incident_data_1970_mar2025)
mains_df <- subset(cleaned_full_incident_data_1970_mar2025, select = c(CAUSE, SOURCE, SYSTEM_PART_INVOLVED, LOCAL_DATETIME, NATURAL_FORCE_TYPE)) %>%
  filter(SYSTEM_PART_INVOLVED == "MAIN") %>%
  mutate(LOCAL_DATETIME = ymd_hms(LOCAL_DATETIME))

# count total observations
main_n <- nrow(mains_df)

# reorder sources
mains_df$SOURCE <- factor(mains_df$SOURCE, levels = c("incident_gas_distribution_jan2010_present", "incident_gas_distribution_mar2004_dec2009", "incident_gas_distribution_mid1984_feb2004", "incident_gas_distribution_1970_mid1984"))

# bar plot
ggplot(mains_df, aes(x=CAUSE, fill=SOURCE)) + 
  geom_bar() +
  coord_flip() + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~./main_n, labels = percent)) +
  scale_fill_paletteer_d(name = NULL, labels = c("2010-2024", "2004-2009", "1984-2004", "1970-1984"), "ltc::fernande") + 
  labs(title = "Incident Causes for Gas Pipeline Mains",
       x = NULL, 
       y = "Incidents") + 
  theme_bw() + 
  theme(legend.position = "inside", 
        legend.position.inside = c(0.8, 0.2), 
        legend.box.background = element_rect(colour = "black"))
```

<img src="man/figures/README-example 1-1.png" width="100%" />
