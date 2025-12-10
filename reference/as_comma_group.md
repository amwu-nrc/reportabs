# Format a group of values of different magnitudes with comma separation

Format a group of values of different magnitudes with comma separation

## Usage

``` r
as_comma_group(.data, group = NULL, value = NULL, suffix = NULL, digits = 0)
```

## Arguments

- .data:

  A dataframe which contains the group of values

- group:

  Character. A column in the supplied dataframe specifying which group
  the values belong. If NULL, row numbers will be used. See the example
  for more details.

- value:

  Numeric. A column in the supplied dataframe which contains the values
  to be comma separated.

- suffix:

  Character. An optional character to print at the end of each value,
  i.e. "persons".

- digits:

  Numeric. The number of digits

## Value

A character vector of the same length as the input dataframe.
