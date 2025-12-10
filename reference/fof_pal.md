# Generate Factory of the Future colours from a pre-defined palette

**\[deprecated\]** `fof_pal()` has been renamed
[`nrc_pal()`](https://amwu-nrc.github.io/reportabs/reference/nrc_pal.md)

## Usage

``` r
fof_pal(palette = "main", reverse = FALSE, ...)
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
