create_axes <- function(over, sw) {
  switch(sw,
         "australian_industry" = create_axes_australian_industry(over),
         "labour_force" = create_axes_labour_force(over),
         "wpi" = create_axes_wpi(over)
  )
}


create_axes_wpi <- function(over) {
  plot_parameters <- list()
  plot_parameters$index <- TRUE
  plot_parameters$y_label <- scales::comma_format(scale = 1)
  plot_parameters$hover <- as_comma

  return(plot_parameters)

}

create_axes_labour_force <- function(over) {

  to_match <- paste(c("rate", "ratio", "proportion", "index"), collapse = "|")

  plot_parameters <- list()

  plot_parameters$index <- dplyr::case_when(
    any(grepl("payroll", over$indicator, ignore.case = TRUE)) ~ FALSE,
    any(grepl("index", over$indicator, ignore.case = TRUE)) ~ FALSE,
    length(over$state) >= 2 & !any(grepl(to_match, over$indicator)) ~ TRUE,
    length(over$state == 1) & any(grepl(to_match, over$indicator)) ~ FALSE,
    any(grepl(to_match, over$indicator)) ~ FALSE,
    TRUE ~ FALSE

  )

  if (any(grepl(to_match, over$indicator))) {
    plot_parameters$y_label <- scales::label_percent(scale = 1)
  } else {
    plot_parameters$y_label <- scales::label_comma(scale = 1)
  }

  plot_parameters$hover <- as_comma

    return(plot_parameters)

}
