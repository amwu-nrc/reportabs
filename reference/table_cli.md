# Change in Living Costs

A `gt` table of the change in living costs, by category, between the
most recent available data and the date specified in `since`. Includes
the change in Manufacturing industry wages over the same period for
comparison.

## Usage

``` r
table_cli(wpi_data, cli_data, since)
```

## Arguments

- wpi_data:

  Wage Price Index (wpi_quarterly) data from `read_absdata`

- cli_data:

  Living Cost Index data (cli) from `read_absdata`

- since:

  The date ("%Y-%m-%d) for comparison

## Value

A gt table

## Examples

``` r
if (FALSE) { # \dontrun{
wpi_data <- read_absdata("wpi_quarterly", export_dir = "data")
cli_data <- read_absdata("cli", export_dir = "data")
table_cli(wpi_data, cli_data, "2020-03-01")
} # }
```
