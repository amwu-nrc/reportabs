# Format a value as a pretty string, with comma separation

Format a value as a pretty string, with comma separation

## Usage

``` r
as_comma(string, group = NULL, value = NULL, suffix = NULL, digits = 0)
```

## Arguments

- string:

  Value to print

- group:

  A group over which to apply as_comma()

- value:

  The values to apply as_comma() to. Is NULL if group is NULL

- suffix:

  A string to print after the number

- digits:

  Number of digits to print. Defaults to 0

## Value

A comma formatted string of `string`

## Examples

``` r
as_comma(1000)
#> [1] "1,000"

```
