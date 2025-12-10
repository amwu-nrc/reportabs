# Growth of an ABS indicator.

Growth of an ABS indicator.

## Usage

``` r
growth(data = NULL, filter_with, ym = "year", at_year = NULL, at_month = NULL)
```

## Arguments

- data:

  A dataframe of tidyed ABS data

- filter_with:

  A list of which variables to filter the dataframe by. Must be a list.
  Can specify indicator, gender, state, age, and series_type. Indicator
  must be specified

- ym:

  ym = "year" to calculate the change over the year, or ym = "month" to
  calculate the change over the month

- at_year:

  By default, change() returns the difference over the past 12 months
  (to the current year). at_year and at_month can be specified to
  calculate the change between the current value, and the value as at
  at_year and at_month.

- at_month:

  By default, change() returns the difference over the past 12 months
  (to the current year). at_year and at_month can be specified to
  calculate the change between the current value, and the value as at
  at_year and at_month.

## Value

dataframe

## Examples

``` r
if (FALSE) growth(labour_force, filter_with = list(indicator = "Unemployment rate")) # \dontrun{}
```
