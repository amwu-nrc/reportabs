# Convenience functions for creating charts for ABS and other

These functions are wrap-around functions for abs_plot() for most of the
indicators you should ever need to plot. For more information about what
options you can specify see
[`abs_plot`](https://amwu-nrc.github.io/reportabs/reference/abs_plot.md)

## Usage

``` r
plot_employed_total(states, ...)

plot_employed_full_time(states, ...)

plot_employed_part_time(states, ...)

plot_unemployed_total(states, ...)

plot_unemployed_looked_for_full_time_work(states, ...)

plot_unemployed_looked_for_part_time_work(states, ...)

plot_labour_force_total(states, ...)

plot_underemployed_total(states, ...)

plot_underutilised_total(states, ...)

plot_hours_worked(states, ...)

plot_hours_worked_full_time(states, ...)

plot_hours_worked_part_time(states, ...)

plot_not_in_the_labour_force(states, ...)

plot_working_population(states, ...)

plot_employment_population_ratio(states, ...)

plot_unemployment_rate(states, ...)

plot_unemployment_rate_looked_for_full_time_work(states, ...)

plot_unemployment_rate_looked_for_part_time_work(states, ...)

plot_participation_rate(states, ...)

plot_underemployment_ratio(states, ...)

plot_underemployment_rate(states, ...)

plot_underutilisation_rate(states, ...)

plot_sahm_recession_indicator(states, ...)
```

## Arguments

- states:

  The states to include in the plot.

- ...:

  Other options passed to abs_plot
