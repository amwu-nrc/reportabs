#' Generate a time-series plot for ABS Time Series Data.
#'
#' @description
#' \code{abs_plot()} can automatically generate appropriate plots for ABS
#' Time Series indicators for both static display in documents, or RMarkdown,
#' as well as interactive plots through plotly.
#'
#' [abs_plot_wpi()], [abs_plot_labour_force()] can be called directly and
#' create plots for WPI and Labour Force data respectively.
#'
#' @param data ABS Time Series data
#' @param filter_with named list specifying what should be plot.
#' @param type One of "wpi" for Wage Price Index plots or "labour_force" for Labour Force plots.
#' @param years How many years of data to plot, defaults to years.
#' @param ... other arguments passed to various functions depending on type.
#'
#' @return A ggplot2 time-series plot or a plotly time-series plot if plotly = TRUE
#'
#' @name abs_plot
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' #Plot employed total for Australia and South Australia
#' abs_plot(filter_with = list(indicator = "Employed total", state = c("South Australia", "Australia")), type = "labour_force")
#' }
#'
#' @family abs plot functions
#' @seealso
#' * [abs_plot_wpi] for WPI plots.
#' * [abs_plot_labour_force] for Labour Force plots.
#'
abs_plot <- function(data = NULL,
                     filter_with,
                     type,
                     years = 2015,
                     ...) {

  dat <- switch(type,
                "wpi" = abs_plot_wpi(data = data, filter_with = filter_with, years = years, ...),
                "labour_force" = abs_plot_labour_force(data = data, filter_with = filter_with, years = years,...))

  return(dat)


}

#' @param index_to Date as "yyyy-mm-dd" to re-index data to, defaults to "2020-03-01"
#' @param compare_aus Whether to automatically add Australian data to plot, defaults to FALSE
#' @rdname abs_plot
#' @export
abs_plot_wpi <- function(data,
                         filter_with,
                         years,
                         index_to = "2020-03-01",
                         compare_aus = FALSE) {

  if (is.null(data)) {
    data <- read_absdata("wpi_quarterly")
  }

  over <- make_safe(data, filter_with, sw = "wpi")

  e <- check_valid_graph(over)
  col_var <- get_col_var(e)

  plot_data <- create_plot_data(data = data,
                                over = over,
                                index_to = index_to,
                                years = years)

  plot_parameters <- plot_parameters(plot_data = plot_data,
                                     over = over,
                                     sw = "wpi",
                                     col_var = col_var,
                                     markdown = F,
                                     compare_aus = F,
                                     facet = NULL)

  create_plot(plot_data = plot_data,
              plot_parameters = plot_parameters,
              void = FALSE,
              plotly = FALSE)

}
#' @param compare_aus Whether to automatically add Australian data to plot, defaults to FALSE
#' @param markdown Whether to use markdown formatting of titles and subtitles. NYI.
#' @param facet Whether to facet the plot to include additional dimensions. NYI.
#' @param plotly Whether to use `ggplotly` to create a plotly interactive plot. NYI.
#' @param void Whether to strip all theme elements from the plot.
#' @rdname abs_plot
#' @export
abs_plot_labour_force <- function(data,
                                  filter_with,
                                  years,
                                  compare_aus = TRUE,
                                  markdown = FALSE,
                                  facet = NULL,
                                  plotly = FALSE,
                                  void = FALSE) {

  if (is.null(data)) {
    data <- read_absdata("labour_force")
  }

  over <- make_safe(data, filter_with, sw = "labour_force")
  if ("state" %in% names(over) && !"Australia" %in% over$state  && compare_aus) {
    over$state = c(over$state, "Australia")
  } else {
    compare_aus <- FALSE
  }
  e <- check_valid_graph(over, facet = facet)
  col_var <- get_col_var(e)
  index_to <- ifelse(years >= lubridate::year(min(data$date)),
                     paste(years,  #this feels very unnecessary - but the dashboard data and lf briefing data is only 1/5 years prior to the most current
                           stringr::str_pad(width = 2, pad = "0", lubridate::month(min(data$date))),
                           stringr::str_pad(width = 2, pad = "0", lubridate::day(min(data$date))),
                           sep = "-"),
                     paste(lubridate::year(min(data$date)),
                           stringr::str_pad(width = 2, pad = "0", lubridate::month(min(data$date))),
                           stringr::str_pad(width = 2, pad = "0", lubridate::day(min(data$date))),
                           sep = "-")
  )


  plot_data <- create_plot_data(data = data,
                                over = over,
                                index_to = index_to,
                                years = years)

  plot_parameters <- plot_parameters(plot_data = plot_data,
                                     over = over,
                                     sw = "labour_force",
                                     col_var = col_var,
                                     markdown = markdown,
                                     compare_aus = compare_aus,
                                     facet = facet)
  create_plot(plot_data = plot_data,
              plot_parameters = plot_parameters,
              void = void,
              plotly = plotly)
}




