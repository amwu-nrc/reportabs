# Return the most recent (current) value for an indicator

Return the most recent (current) value for an indicator

## Usage

``` r
current(data, filter_with, print = TRUE)
```

## Arguments

- data:

  a dataframe of cleaned ABS Time Series data returned from readabs

- filter_with:

  a list of variables to filter the dataframe on. Valid variables
  include gender, age, indicator, and series type.

- print:

  logical. If TRUE (default) current() returns a string. Specify FALSE
  to return a number.

## Value

String if print == TRUE. Number if print == FALSE

## Examples

``` r
library(reportabs)
labour_force_briefing <- read_absdata("labour_force_briefing")
#> Reading labour_force_briefing file found in /tmp/RtmphAE3he
current(labour_force_briefing, list(indicator = "Employed total",
series_type = "Seasonally Adjusted"))
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
