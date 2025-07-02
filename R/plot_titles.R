create_titles <- function(plot_data, index, over, sw) {
  switch(sw,
         "australian_industry" = create_titles_australian_industry(index),
         "labour_force" = create_titles_labour_force(plot_data, index, over),
         "wpi" = create_titles_wpi(plot_data, index, over))
}

create_titles_labour_force <- function(plot_data, index, over) {

  plot_parameters <- list()

  plot_parameters$month <- lubridate::month(min(plot_data$index_to), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$index_to))

  if(index) {
    plot_parameters$title <- paste0(collapse = ", ", stringr::str_to_title(over$indicator))
    plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
    plot_parameters$y_var <- "index"
  } else {
    plot_parameters$title <- paste0(collapse = ", ",stringr::str_to_title(over$indicator))
    plot_parameters$subtitle <- if(length(over$state) == 1) {over$state} else NULL
    plot_parameters$y_var <- "value"
  }
  return(plot_parameters)
}

create_titles_wpi <- function(plot_data, index, over) {
  plot_parameters <- list()

  plot_parameters$month <- lubridate::month(min(plot_data$index_to), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$index_to))

  plot_parameters$title <- paste0(collapse = ", ", stringr::str_to_title(over$indicator))
  plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
  plot_parameters$y_var <- "index"

  return(plot_parameters)
}

create_titles_markdown <- function() {

  if (plot_parameters$markdown & plot_parameters$col_var == "state") {
    title_cols <- nrc_pal()(plot_parameters$n_col)

    plot_title_md <- paste0("<span style = color:'", title_cols, "'>", names(title_cols), "</span>", collapse = " and ")

  } else {
    plot_title_md <- paste0(over$state, collapse = " & ")
  }
}
