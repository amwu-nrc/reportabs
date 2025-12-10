# Format a value as a pretty string, with the percentage points suffix

Format a value as a pretty string, with the percentage points suffix

## Usage

``` r
as_percentage_point(string, scale = 1, digits = 1)
```

## Arguments

- string:

  the value to print

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
as_percentage_point(1.5)
#> [1] "1.5 percentage points"
```
