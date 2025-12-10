# Fill scale for Australian Manufacturing Workers Union colours

Fill scale for Australian Manufacturing Workers Union colours

## Usage

``` r
scale_fill_nrc(palette = NULL, discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Name of the colour palette (default "main")

- discrete:

  Whether the colours should be discrete or continuous (default TRUE)

- reverse:

  Whether the order of the colours should be reversed (default FALSE)

- ...:

  Additional arguments passed to ggplot scale

## Value

ggplot2 fill scale

## Examples

``` r
library(ggplot2)
df <- data.frame(x = c("One", "Two", "Three"), y = c(4, 2, 9))
p <- ggplot(df, aes(x = x, y = y, fill = x)) + geom_col()
p + scale_fill_nrc()
```
