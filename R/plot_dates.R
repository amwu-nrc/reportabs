#' Create Date Range for Plots
#'
#' Ensures the last period of data is shown on the plot.
#'
#' @param plot_data The data
#'
#' @returns a vector of dates
#' @export
#'
#' @examples
#'
plot_date_range <- function(plot_data) {

  date_min <- as.Date(min(plot_data$date))
  date_max <- as.Date(max(plot_data$date))

  pre <- scales::breaks_pretty(n = 5)(c(date_min, date_max))
  date_adj <- as.numeric(pre[length(pre)] - date_max)
  adj <- pre - date_adj
  names(adj) <- NULL
  date_breaks <- adj[adj >= date_min & adj <= date_max]
  return(date_breaks)
}
