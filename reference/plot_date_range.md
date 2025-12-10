# Create Date Range for Plots

Ensures the last period of data is shown on the plot.

## Usage

``` r
plot_date_range(plot_data)
```

## Arguments

- plot_data:

  The data

## Value

a vector of dates

## Examples

``` r
library(ggplot2)

ggplot(economics, aes(x = date, y = uempmed)) +
geom_line() +
scale_x_date(breaks = plot_date_range(economics))

```
