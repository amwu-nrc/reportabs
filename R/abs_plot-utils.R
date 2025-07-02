plot_parameters <- function(plot_data, over, sw, col_var,  markdown, compare_aus, facet, ...) {

  plot_parameters <- list()

  plot_parameters$col_var <- col_var
  plot_parameters$n_col <- length(unlist(over[names(over)==col_var]))

  if (any(Map(length, over) > 1)) {
    plot_parameters$legend <- "top"
  }

  # Y axis stuff
  # Need to scale data still (ie 140,000 = 140k, or something)
  plot_parameters <- append(plot_parameters,  create_axes(over = over, sw = sw))

  #scale

  #Titles and subtitles
  plot_parameters <- append(plot_parameters,
                            create_titles(plot_data = plot_data, index = plot_parameters$index, over = over, sw = sw))

  #Captions
  plot_parameters$caption <- create_caption(sw, plot_data, over)

  #Miscellany

  plot_parameters$num_months <- as.numeric(lubridate::month(max(plot_data$date)))

  plot_parameters$date_range <- c(min(plot_data$date), max(plot_data$date))
  plot_parameters$markdown <- markdown



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

check_valid_graph <- function(over, facet) {
  e <- Map(length, over)
  if (sum(e > 1) > 1 && is.null(facet)) {
    cli::cli_abort("Only one variable in `filter_with` can have length greater than 1.
                     You can try specifying `facet`")
  } else {
    return(e)
  }
}

get_col_var <- function(e) {
  # The simplest case is when 1 variable in filter_with has length > 1
  if (length(e[e > 1]) == 1) {
    col_var <- names(e[e > 1])
  } else {
    col_var <- dplyr::case_when(
      length(e[e > 1]) == 0 && !"industry" %in% names(e) ~ "indicator",
      length(e[e > 1]) == 0 && "industry" %in% names(e) ~ "industry"
    )
  }

  col_var
}

create_plot_data <- function(data, over, index_to, years) {
  plot_data <- data |>
    filter_list(over) |>
    dplyr::filter(lubridate::year(date) >= years) |>
    dplyr::group_by(dplyr::across(-dplyr::any_of(c("date", "value")))) |>
    dplyr::mutate(index = 100 * .data$value / .data$value[date == index_to],
                  value = ifelse(.data$unit == "000", .data$value*1000, .data$value),
                  index_to = {{index_to}}) |>
    dplyr::ungroup()

  plot_data
}
