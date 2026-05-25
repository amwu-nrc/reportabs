#' Change in Living Costs
#'
#' @description
#' A `gt` table of the change in living costs, by category, between the most recent
#' available data and the date specified in `since`. Includes the change in Manufacturing
#' industry wages over the same period for comparison.
#'
#' @param wpi_data Wage Price Index (wpi_quarterly) data from `read_absdata`
#' @param cli_data Living Cost Index data (cli) from `read_absdata`
#' @param since The date ("%Y-%m-%d) for comparison
#'
#' @returns A gt table
#' @export
#'
#' @examples \dontrun{
#' wpi_data <- read_absdata("wpi_quarterly", export_dir = "data")
#' cli_data <- read_absdata("cli", export_dir = "data")
#' table_cli(wpi_data, cli_data, "2020-03-01")
#' }
table_cli <- function(wpi_data, cli_data, since) {

  wage_growth <- wpi_data |>
    dplyr::filter(date %in% c(max(date), {{since}}),
           data_type == "Quarterly Index",
           indicator == "Ordinary time hourly rates of pay excluding bonuses",
           state == "Australia",
           industry == "Manufacturing",
           sector == "Private and Public") |>
    dplyr::mutate(pc = value / value[date=={{since}}] - 1) |>
    dplyr::filter(date == max(date))

  first_date <- scales::label_date(format = "%B %Y")(lubridate::as_date({{since}}))
  last_date <-  scales::label_date(format = "%B %Y")(wage_growth$date)

  cli_data |>
    dplyr::filter(data_type == "Index Numbers",
           date %in% c(max(date), {{since}}),
           household_type == "Employee households") |>
    dplyr::group_by(cpi_category) |>
    dplyr::mutate(pc = value / value[date=={{since}}] - 1) |>
    dplyr::ungroup() |>
    dplyr::filter(date == max(date)) |>
    dplyr::select(cpi_category, pc) |>
    dplyr::add_row(cpi_category = "Wages",
            pc = wage_growth$pc) |>
    dplyr::arrange(-pc) |>
    gt::gt(rowname_col = "cpi_category") |>
    gt::tab_header(
      title = gt::md("**Change in living costs by product category**"),
      subtitle = glue::glue("% change between {last_date} and {first_date}")
    ) |>
    gt::tab_stubhead(label = "Price Category") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(
                rows = cpi_category %in% c("All groups", "Wages")
              )) |>
    gt::fmt_percent(columns = pc, scale = 100, decimal = 1) |>
    gt::cols_label(pc = "% change")

}
