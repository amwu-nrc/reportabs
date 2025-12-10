# The average of an indicator between two years

The average of an indicator between two years

## Usage

``` r
average_over(data, filter_with, between)
```

## Arguments

- data:

  a data frame of cleaned ABS Time Series data returned from readabs

- filter_with:

  a list of variables to filter the data frame on. Must include an
  indicator

- between:

  a date range `c(min, max)` to calculate the average over

## Value

numeric

## Examples

``` r
library(reportabs)
labour_force_briefing <- read_absdata("labour_force_briefing")
average_over(labour_force_briefing, list(indicator = "Employed total"), between = c(2010,2015))
#> [1] NaN
```
