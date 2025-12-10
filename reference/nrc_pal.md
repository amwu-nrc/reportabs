# Generate Australian Manufacturing Workers Union colours from a pre-defined palette

Generate Australian Manufacturing Workers Union colours from a
pre-defined palette

## Usage

``` r
nrc_pal(palette = "main", reverse = FALSE, ...)
```

## Arguments

- palette:

  Name of colour palette. See
  [`palette_names()`](https://amwu-nrc.github.io/reportabs/reference/palette_names.md)
  for a list of available palettes.

- reverse:

  Whether the order of the colours should be reversed (default FALSE)

- ...:

  Other arguments passed to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html)

## Value

A colour palette

## Examples

``` r
# To generate a sequence of n colours from the 'main' palette:
n <- 2
nrc_pal("main")(n)
#> [1] "#FF5800" "#001D26"
```
