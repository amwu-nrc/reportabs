# Factory of the Future Theme

**\[deprecated\]** `theme_fof()` has been renamed
[`theme_nrc()`](https://amwu-nrc.github.io/reportabs/reference/theme_nrc.md)

## Usage

``` r
theme_fof(
  base_size = 14,
  colour = lifecycle::deprecated(),
  legend = "none",
  markdown = lifecycle::deprecated(),
  flipped = FALSE,
  legacy = lifecycle::deprecated()
)
```

## Arguments

- base_size:

  The base size of text elements of the plot. (default 12)

- colour:

  The background colour of the plot.

- legend:

  The position of the legend (default no legend)

- markdown:

  Whether to use markdown formatting for plot titles (default FALSE)

- flipped:

  TRUE to flip the y-axis guide lines to show on the x-axis instead.

- legacy:

  Whether to use legacy fonts

## Value

ggplot2 theme

## Details

Theme for plots in Factory of the Future publications, reports, Shiny
Apps.
