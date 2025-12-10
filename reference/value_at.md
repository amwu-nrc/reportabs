# Value of an ABS Time Series indicator for a specific year and month.

Value of an ABS Time Series indicator for a specific year and month.

## Usage

``` r
value_at(data, filter_with, at_year = NULL, at_month = NULL)
```

## Arguments

- data:

  a dataframe of ABS time series data.

- filter_with:

  a named list of at least an indicator to filter the dataframe on.

- at_year:

  a year (numeric). Defaults to the most recent year if NULL (the
  default) and at_month = NULL. If only at_year is specified, the value
  is averaged over the year (Not yet implemented)

- at_month:

  a month (name). Defaults to the most recent month if NULL (the
  default)

## Value

a number

## Examples

``` r
library(reportabs)
labour_force_briefing <- read_absdata("labour_force_briefing")
#> Reading labour_force_briefing file found in /tmp/RtmphAE3he
value_at(labour_force_briefing , filter_with = list(indicator = "Employed total"))
#> → No year or month specified. Returning data for 2025 October
#> # A tibble: 27 × 7
#>    date       sex     state             series_type       unit  indicator  value
#>    <date>     <chr>   <chr>             <chr>             <chr> <chr>      <dbl>
#>  1 2025-10-01 Persons Australia         Seasonally Adjus… 000   Employed… 14683.
#>  2 2025-10-01 Persons New South Wales   Seasonally Adjus… 000   Employed…  4509.
#>  3 2025-10-01 Persons Victoria          Seasonally Adjus… 000   Employed…  3809.
#>  4 2025-10-01 Persons Queensland        Seasonally Adjus… 000   Employed…  3009.
#>  5 2025-10-01 Persons South Australia   Seasonally Adjus… 000   Employed…   986.
#>  6 2025-10-01 Persons Western Australia Seasonally Adjus… 000   Employed…  1655.
#>  7 2025-10-01 Persons Tasmania          Seasonally Adjus… 000   Employed…   286.
#>  8 2025-10-01 Males   Australia         Seasonally Adjus… 000   Employed…  7637.
#>  9 2025-10-01 Males   New South Wales   Seasonally Adjus… 000   Employed…  2358.
#> 10 2025-10-01 Males   Victoria          Seasonally Adjus… 000   Employed…  1964.
#> # ℹ 17 more rows
```
