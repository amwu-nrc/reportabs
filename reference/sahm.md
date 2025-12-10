# Plot the Sahm Recession Indicator for a given state. The Sahm indicator compares the 3 month average (seasonally adjusted) unemployment rate with the minimum over the last 12 months. Sahm values over 1 are indicative of a recession.

Plot the Sahm Recession Indicator for a given state. The Sahm indicator
compares the 3 month average (seasonally adjusted) unemployment rate
with the minimum over the last 12 months. Sahm values over 1 are
indicative of a recession.

## Usage

``` r
sahm(data, state = "Australia")
```

## Arguments

- data:

  A dataframe with at least the columns date, state, and value

- state:

  The state or territory to plot the indicator for. Accepts Australia

## Value

a ggplot2 object

## Examples

``` r
if (FALSE) sahm("Australia") # \dontrun{}
```
