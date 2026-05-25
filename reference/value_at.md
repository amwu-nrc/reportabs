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
#> Reading labour_force_briefing file found in /tmp/Rtmp2eRRFm
value_at(labour_force_briefing , filter_with = list(indicator = "Employed total"))
#> → No year or month specified. Returning data for 2026 March
#> # A tibble: 27 × 7
#>    date       sex     state             series_type       unit  indicator  value
#>    <date>     <chr>   <chr>             <chr>             <chr> <chr>      <dbl>
#>  1 2026-03-01 Persons Australia         Seasonally Adjus… 000   Employed… 14768.
#>  2 2026-03-01 Persons New South Wales   Seasonally Adjus… 000   Employed…  4575.
#>  3 2026-03-01 Persons Victoria          Seasonally Adjus… 000   Employed…  3786.
#>  4 2026-03-01 Persons Queensland        Seasonally Adjus… 000   Employed…  3036.
#>  5 2026-03-01 Persons South Australia   Seasonally Adjus… 000   Employed…   995.
#>  6 2026-03-01 Persons Western Australia Seasonally Adjus… 000   Employed…  1679.
#>  7 2026-03-01 Persons Tasmania          Seasonally Adjus… 000   Employed…   280.
#>  8 2026-03-01 Males   Australia         Seasonally Adjus… 000   Employed…  7670.
#>  9 2026-03-01 Males   New South Wales   Seasonally Adjus… 000   Employed…  2381.
#> 10 2026-03-01 Males   Victoria          Seasonally Adjus… 000   Employed…  1960.
#> # ℹ 17 more rows
```
