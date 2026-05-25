# Create Cumulative WPI data set

Create Cumulative WPI data set

## Usage

``` r
cumulative_wpi(
  data,
  state,
  indicator = "Total hourly rates of pay excluding bonuses",
  sector = "Private and Public",
  industry = "All industries",
  since
)
```

## Arguments

- data:

  WPI data from `read_absdata`

- state:

  The state, or Australia

- indicator:

  Indicator

- sector:

  Sector

- industry:

  Industry

- since:

  Date ("%Y-%m-%d") to benchmark
