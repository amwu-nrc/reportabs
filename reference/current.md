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
#> Reading labour_force_briefing file found in /tmp/Rtmp2eRRFm
current(labour_force_briefing, list(indicator = "Employed total",
series_type = "Seasonally Adjusted"))
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
