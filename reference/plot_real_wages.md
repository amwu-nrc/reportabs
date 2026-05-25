# Plot Real Wages Over Time

Plot Real Wages Over Time

## Usage

``` r
plot_real_wages(
  cpi_data,
  wpi_data,
  forecast = list(years = 1, wage = 10, inflation = 3),
  ...
)
```

## Arguments

- cpi_data:

  data from `read_absdata`

- wpi_data:

  data from `read_absdata`

- forecast:

  (optional) forecast of inflation and wages
  `list(years, wage, inflation)`

- ...:

  other arguments passed on to functions

## Value

ggplot2 graph

## Examples

``` r
if (FALSE) { # \dontrun{
plot_real_wages(read_absdata("cpi_quarterly_group"),
                read_absdata("wpi_quarterly"),
                forecast = NULL,
                region = "Australia",
                state = "Australia",
                since = "2020-03-01",
                include_housing = FALSE)
                } # }
```
