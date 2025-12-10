# Generate a time-series plot for ABS Time Series Data.

`abs_plot()` can automatically generate appropriate plots for ABS Time
Series indicators for both static display in documents, or RMarkdown, as
well as interactive plots through plotly.

`abs_plot_wpi()`, `abs_plot_labour_force()` can be called directly and
create plots for WPI and Labour Force data respectively.

## Usage

``` r
abs_plot(data = NULL, filter_with, type, years = 2015, ...)

abs_plot_wpi(
  data,
  filter_with,
  years,
  index_to = "2020-03-01",
  compare_aus = FALSE
)

abs_plot_labour_force(
  data,
  filter_with,
  years,
  compare_aus = TRUE,
  markdown = FALSE,
  facet = NULL,
  plotly = FALSE,
  void = FALSE
)
```

## Arguments

- data:

  ABS Time Series data

- filter_with:

  named list specifying what should be plot.

- type:

  One of "wpi" for Wage Price Index plots or "labour_force" for Labour
  Force plots.

- years:

  How many years of data to plot, defaults to years.

- ...:

  other arguments passed to various functions depending on type.

- index_to:

  Date as "yyyy-mm-dd" to re-index data to, defaults to "2020-03-01"

- compare_aus:

  Whether to automatically add Australian data to plot, defaults to
  FALSE

- markdown:

  Whether to use markdown formatting of titles and subtitles. NYI.

- facet:

  Whether to facet the plot to include additional dimensions. NYI.

- plotly:

  Whether to use `ggplotly` to create a plotly interactive plot. NYI.

- void:

  Whether to strip all theme elements from the plot.

## Value

A ggplot2 time-series plot or a plotly time-series plot if plotly = TRUE

## See also

- abs_plot_wpi for WPI plots.

- abs_plot_labour_force for Labour Force plots.

## Examples

``` r
if (FALSE) { # \dontrun{
#Plot employed total for Australia and South Australia
abs_plot(filter_with = list(indicator = "Employed total", state = c("South Australia", "Australia")), type = "labour_force")
} # }
```
