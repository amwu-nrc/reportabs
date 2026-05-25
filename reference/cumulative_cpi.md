# Create Cumulative CPI Data Set

Create Cumulative CPI Data Set

## Usage

``` r
cumulative_cpi(data, region, since, include_housing = F)
```

## Arguments

- data:

  CPI or CLI data from `read_absdata`

- region:

  The capital city or Australia

- since:

  Date ("%Y-%m-%d") to benchmark

- include_housing:

  whether to include mortgage repayments (requires data = CLI)

## Value

dataframe
