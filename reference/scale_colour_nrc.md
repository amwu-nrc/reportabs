# Colour Scale for Australian Manufacturing Workers Union

Colour Scale for Australian Manufacturing Workers Union

## Usage

``` r
scale_colour_nrc(palette = NULL, discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Name of the colour palette (default NULL)

- discrete:

  Whether the colours should be discrete or continuous (default TRUE)

- reverse:

  Whether the order of the colours should be reversed (default FALSE)

- ...:

  Additional arguments passed to ggplot scale

## Examples

``` r
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = disp, col = factor(cyl))) + geom_point()
p + scale_colour_nrc()
```
