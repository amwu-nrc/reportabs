plot_parameters <- function(plot_data, over, col_var, n_cols, markdown, compare_aus, facet) {

  plot_parameters <- list()

  plot_parameters$col_var <- col_var
  plot_parameters$n_cols <- n_cols
  plot_parameters$data_name <- unique(plot_data$data)

  if (any(Map(length, over) > 1)) {
    plot_parameters$legend <- "top"
  }


  to_match <- c("rate", "ratio", "proportion", "index")

  if (any(grepl("payroll", over$indicator, ignore.case = TRUE))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (length(over$state) >= 2 & !any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- TRUE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if ((length(over$state) == 1) & any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_percent
  } else if (any(grepl("index", over$indicator, ignore.case = TRUE))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else {
    plot_parameters$index <- FALSE
    plot_parameters$scale <- ifelse(min(plot_data$value > 1e6), 1e-6, 1e-3)
    plot_parameters$suffix <- dplyr::case_when(
      min(plot_data$value) > 1e6 ~ "m",
      min(plot_data$value) > 1e3 ~ "k",
      TRUE ~ "")
    plot_parameters$y_label <- scales::comma_format(scale = plot_parameters$scale, suffix = plot_parameters$suffix)
    plot_parameters$hover <- as_comma
  }


  plot_parameters$num_months <- as.numeric(lubridate::month(max(plot_data$date)))
  plot_parameters$month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$date))
  plot_parameters$caption <- create_caption(plot_parameters$data_name, plot_data, over)
  plot_parameters$date_range <- c(min(plot_data$date), max(plot_data$date))
  plot_parameters$markdown <- markdown

  if (plot_parameters$markdown & plot_parameters$col_var == "state") {
    title_cols <- nrc_pal()(plot_parameters$n_col)

    plot_title_md <- paste0("<span style = color:'", title_cols, "'>", names(title_cols), "</span>", collapse = " and ")

  } else {
    plot_title_md <- paste0(over$state, collapse = " & ")
  }
  if(plot_parameters$index) {
    plot_parameters$title <- paste0(collapse = ", ", stringr::str_to_title(over$indicator)) #paste0(stringr::str_to_title(indicator), ": ", plot_title_md)
    plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
    plot_parameters$y_var <- "index"
  } else {
    plot_parameters$title <- paste0(collapse = ", ",stringr::str_to_title(over$indicator)) #paste0(stringr::str_to_title(indicator), ": ", plot_title_md)
    plot_parameters$subtitle <- if(length(over$state) == 1) {over$state} else NULL
    plot_parameters$y_var <- "value"
  }

  if (!is.null(facet)) {
    plot_parameters$facet <- facet
  }


  return(plot_parameters)
}



create_plot <- function(plot_data, plot_parameters, void, plotly, ...) {

  date_min <- as.Date(plot_parameters$date_range[1])
  date_max <- as.Date(plot_parameters$date_range[2])

  pre <- scales::breaks_pretty(n = 5)(c(date_min, date_max))
  date_adj <- as.numeric(pre[length(pre)] - date_max)
  adj <- pre - date_adj
  names(adj) <- NULL
  date_breaks <- adj[adj >= date_min & adj <= date_max]


  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = date,
                                    y = .data[[plot_parameters$y_var]],
                                    colour = .data[[plot_parameters$col_var]])) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_date(date_labels = "%e %b\n%Y",
                          breaks = date_breaks) +
    ggplot2::scale_y_continuous(labels = plot_parameters$y_label)

  if (!void) {
    p <- p + ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_parameters$title,
      subtitle = plot_parameters$subtitle,
      caption = plot_parameters$caption
    ) + ggplot2::guides(colour = ggplot2::guide_legend())

    p <- p + theme_nrc(legend = "top",...) + scale_colour_nrc()

  } else {
    p <- p + ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
  }

  if (!is.null(plot_parameters$facet)) {

    p <- p + ggplot2::facet_wrap(plot_parameters$facet)

  }

  if (plotly) {

    hover_format <- plot_parameters$hover


    p <- p +
      ggplot2::aes(group = 1,
                   text = paste0(.data$state,
                                 "<br>Sex: ", .data$sex,
                                 "<br>Age: ", .data$age,
                                 "<br>Date: ", format(date, "%b-%Y"),
                                 "<br>", .data$indicator, ": ", hover_format(.data$value))) +
      ggplot2::geom_point(shape = 1, size = 1)

    p <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(autosize = TRUE,
                     title = list(text = paste0(plot_parameters$title,
                                                '<br>',
                                                '<sup>',
                                                plot_parameters$subtitle),
                                  x = 0),
                     legend = list(orientation = "h",
                                   title = "",
                                   x = 0,
                                   y = 1),
                     annotations = list(
                       x = 1,
                       y = -0.5,
                       xref = "paper",
                       yref = "paper",
                       xanchor = "right",
                       yanchor = "auto",
                       text = "Source: Economic Indicators"
                     )

      )


  }

  return(p)

}
