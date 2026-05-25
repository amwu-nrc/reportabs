#' Create Cumulative CPI Data Set
#'
#' @param data CPI or CLI data from `read_absdata`
#' @param region The capital city or Australia
#' @param since Date ("%Y-%m-%d") to benchmark
#' @param include_housing  whether to include mortgage repayments (requires data = CLI)
#'
#' @returns dataframe
cumulative_cpi <- function(data, region, since, include_housing = F) {

  if (include_housing && !"household_type" %in% names(data)) {
    stop("Living cost data is needed to include housing")
  }

  if (include_housing) {
    data |>
      dplyr::filter(data_type == "Index Numbers",
                    date >= {{since}},
                    household_type == "Employee households",
                    cpi_category == "All groups") |>
      dplyr::mutate(cumulative_prices = 100*(value / value[date == {{since}}] -1)) |>
      dplyr::select(date, cumulative_prices)
  } else {
    data |>
      dplyr::filter(data_type == "Index Numbers",
                    date >= {{since}},
                    cpi_category == "All groups CPI",
                    region == {{region}}) |>
      dplyr::mutate(cumulative_prices = 100*(value / value[date=={{since}}] - 1)) |>
      dplyr::select(date, cumulative_prices)
  }
}

#' Create Cumulative WPI data set
#'
#' @param data WPI data from `read_absdata`
#' @param state The state, or Australia
#' @param indicator Indicator
#' @param sector Sector
#' @param industry Industry
#' @param since Date ("%Y-%m-%d") to benchmark
#'
cumulative_wpi <- function(data,
                           state,
                           indicator = "Total hourly rates of pay excluding bonuses",
                           sector = "Private and Public",
                           industry = "All industries",
                           since) {
  data |>
    dplyr::filter(data_type == "Quarterly Index",
                  state == {{state}},
                  indicator == {{indicator}},
                  sector == {{sector}},
                  industry == {{industry}},
                  date >= since) |>
    dplyr::mutate(cumulative_wages = 100*(value / value[date=={{since}}] - 1))  |>
    dplyr::select(date, cumulative_wages)
}

#' Plot Real Wages Over Time
#'
#' @param cpi_data data from `read_absdata`
#' @param wpi_data data from `read_absdata`
#' @param forecast (optional) forecast of inflation and wages `list(years, wage, inflation)`
#' @param ... other arguments passed on to functions
#'
#' @returns ggplot2 graph
#' @export
#'
#' @examples \dontrun{
#' plot_real_wages(read_absdata("cpi_quarterly_group"),
#'                 read_absdata("wpi_quarterly"),
#'                 forecast = NULL,
#'                 region = "Australia",
#'                 state = "Australia",
#'                 since = "2020-03-01",
#'                 include_housing = FALSE)
#'                 }
plot_real_wages <- function(cpi_data, wpi_data,
                            forecast = list(years = 1, wage = 10, inflation = 3), ...) {

  if (is.null(forecast$years)) {
    forecast$years <- 0
  }

  if (is.null(forecast$wage)) {
    forecast$wage <- 0
  }

  if (is.null(forecast$inflation)) {
    forecast$inflation <- 0
  }

  args <- list(...)

  # Some things are shared, like since and region. Other things only apply to wpi

  cpi_args <- args[names(args) %in% c("region", "since", "include_housing")]
  cpi_args$data <- cpi_data
  wpi_args <- args[!names(args) %in% c("region", "include_housing")]
  wpi_args$data <- wpi_data



  cumulative_cpi_data <- do.call(cumulative_cpi, args = cpi_args)
  cumulative_wpi_data <- do.call(cumulative_wpi, args = wpi_args)

  cumulative <- dplyr::inner_join(cumulative_wpi_data,
                                  cumulative_cpi_data) |>
    dplyr::mutate(cumulative_real_wage = cumulative_wages - cumulative_prices) |>
    tidyr::pivot_longer(cols = -date,
                        values_to = "value",
                        names_to = "indicator") |>
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "cumulative_wages" ~ "Hourly rates of pay",
      indicator == "cumulative_prices" ~ "Prices",
      indicator == "cumulative_real_wage" ~ "Real hourly rates of pay"
    )) |>
    dplyr::mutate(value_label = ifelse(date == max(date),
                                       scales::label_percent(scale = 1, accuracy = 0.1)(value),
                                       NA))

  # Assume about the inputs. The forecast period is years.
  # The wage ask is averaged over the periods (ie years *4)
  # Estimated inflation is averaged over the years.

  forecast$periods = forecast$years * 4
  forecast$wage = ifelse(forecast$periods == 0, 0, forecast$wage/forecast$periods)
  forecast$inflation = ifelse(forecast$periods == 0, 0, forecast$inflation/forecast$periods)

  fc <- tibble::tibble(
    ix = 0:(forecast$years * 4),
    date = max(cumulative_wpi_data$date) + months(ix)*3, # 3 months per quarter
    cumulative_wages = dplyr::last(cumulative_wpi_data)$cumulative_wages + forecast$wage*ix,
    cumulative_prices = dplyr::last(cumulative_cpi_data)$cumulative_prices + forecast$inflation*ix
  ) |>
    dplyr::mutate(cumulative_real_wage = cumulative_wages - cumulative_prices) |>
    dplyr::select(-ix) |>
    tidyr::pivot_longer(cols = -date,
                        values_to = "value",
                        names_to = "indicator") |>
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "cumulative_wages" ~ "Hourly rates of pay",
      indicator == "cumulative_prices" ~ "Prices",
      indicator == "cumulative_real_wage" ~ "Real hourly rates of pay"
    ))

  if (args$include_housing) {
    # Average real wage growth when deflated by the CLI all series up to March 2020
    avg_real_growth <- 0.00337
    deflator = "Cost of Living Index"
  } else {
    # Average real wage growth when deflated by the CPI all series up to March 2020
    avg_real_growth <- 0.00116
    deflator = "Consumer Price Index"
  }


  seg <- tibble::tibble(date = dplyr::bind_rows(cumulative, fc)$date,
                        value = avg_real_growth*as.integer(date - lubridate::as_date(args$since)),
                        label = "Average Real Wage Growth")

  if (!forecast$years == 0) {
    subtitle <- glue::glue("Cumulative change in hourly rates of pay and prices<br>
       *Forecast **{forecast$wage*forecast$periods}%** pay rise by {scales::label_date(format = '%B %Y')(max(fc$date))} and **{forecast$inflation*forecast$periods}%** annual increase in inflation*")
  } else {
    subtitle <- "Cumulative change in hourly rates of pay and prices"
  }


  ggplot2::ggplot() +
    ggplot2::geom_line(data = cumulative, ggplot2::aes(x = date, y = value, col = indicator)) +
    ggplot2::geom_line(data = fc, ggplot2::aes(x = date, y = value, col = indicator), linetype = 2) +
    geomtextpath::geom_textpath(data = seg, ggplot2::aes(x = date, y = value, label = label),
                                linetype = 2,
                                lwd = 0.5,
                                colour = "#111a41") +
    ggplot2::scale_x_date(date_labels = "%e %b\n %Y",
                          breaks = plot_date_range(dplyr::bind_rows(cumulative,fc))) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_linetype_discrete() +
    scale_colour_nrc() +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = "Real Wages Have Not Kept Up With Prices",
                  caption = glue::glue("Nominal wages deflated with {deflator}"),
                  subtitle = subtitle) +
    theme_nrc(legend = "bottom") +
    ggplot2::guides(linetype = "none")

}
