# ggplot2 theme for Australian Manufacturing Workers Union

Theme for plots in publications, reports, Shiny apps, etc. Anywhere
ggplot2 is used, you can use this theme!

## Usage

``` r
theme_nrc(
  base_size = 12,
  legend = "none",
  ink = midnight,
  paper = white,
  flipped = FALSE
)
```

## Arguments

- base_size:

  The base size of text elements of the plot. (default 12)

- legend:

  The position of the legend. (default "none")

- ink:

  Ink (foreground) colour. (default midnight)

- paper:

  Paper (background) colour. (default white)

- flipped:

  Whether to flip the y-axis guide lines to show on the x-axis instead
  (default FALSE)

## Value

a ggplot2 theme

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
df <- data.frame(x = c("One", "Two", "Three"), y = c(4, 2, 9))
p <- ggplot(df, aes(x = x, y = y, fill = x)) + geom_col()
p + theme_nrc()
} # }
```
