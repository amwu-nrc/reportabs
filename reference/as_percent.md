# Format a value as a pretty string, with a percentage sign printed

Format a value as a pretty string, with a percentage sign printed

## Usage

``` r
as_percent(string, scale = 1, digits = 1)
```

## Arguments

- string:

  Value to print

- scale:

  A value to multiply the number before converting to a string. Default
  of 1 assumes the value to be printed has already been multipled by
  100.

- digits:

  Number of digits to print. Defaults to 1

## Value

character

## Examples

``` r
as_percent(50)
#> [1] "50.0%"
as_percent(0.5, scale = 100)
#> [1] "50.0%"
```
