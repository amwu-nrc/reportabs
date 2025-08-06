filter_list <- function(data, over) {

  if (!is.list(over)) {
    cli::cli_abort("The variable 'filter_with' must be a named list")
  }

  if (!"indicator" %in% names(over)) {
    cli::cli_abort("The variable 'filter_with' must include an indicator")
  }

  l <- purrr::map(over, length) |>
    purrr::list_c()

  build_exprs <- over |>
    tibble::enframe() |>
    dplyr::mutate(l = l,
                  expr = dplyr::case_when(
                    l == 1 ~ paste0(name, " == ", "'", value, "'"),
                    l > 1 ~ paste0(name, " %in% ", value)
                  )
    )

  dplyr::filter(data, !!!rlang::parse_exprs(build_exprs$expr))

}

make_safe <- function(data, over, sw) {
  switch(sw,
         "labour_force" = make_safe_labour_force(data, over),
         "australian_industry" = make_safe_australian_industry(data, over),
         "wpi" = make_safe_wpi(data, over)
         )
}

make_safe_labour_force <- function(data, over) {

  over_safe <- over[names(over) %in% names(data)]

  if (any(!over %in% over_safe)) {
    cli::cli_warn("A variable specified in `filter_with` was not found in the plot data. It has been dropped")
  }

  if (any(names(data) == "sex") && (!"sex" %in% names(over_safe)))  {

    #message("implied sex = 'Persons'")

    over_safe$sex = "Persons"

  }

  if (any(names(data) == "series_type") && (!"series_type" %in% names(over_safe))) {

    cli::cli_warn("implied series_type = 'Trend'")

    over_safe$series_type = "Trend"
  }

  if (any(names(data) == "state") && (!"state" %in% names(over_safe))) {

    #message("implied state = 'Australia'")

    over_safe$state = "Australia"
  }

  if (any(names(data) == "age") && (!"age" %in% names(over_safe))) {

    #message("implied age = 'Total (age)'")
    over_safe$age = "Total (age)"
  }

  if (any(names(data) == "industry") && (!"industry" %in% names(over_safe))) {
    over_safe$industry = unique(data$industry)
  }


  return(over_safe)

}

make_safe_wpi <- function(data, over) {
  over_safe <- over[names(over) %in% names(data)]

  if (any(names(data) == "state") && (!"state" %in% names(over_safe))) {

    over_safe$state = "Australia"
  }

  if (any(names(data) == "sector") && (!"sector" %in% names(over_safe))) {

    cli::cli_alert("implied sector = 'Private and Public'")

    over_safe$sector = "Private and Public"
  }

  if (any(names(data) == "industry") && (!"industry" %in% names(over_safe))) {
    over_safe$industry = "All industries"
  }

  over_safe$data_type = "Quarterly Index"
  return(over_safe)
}
