#' Factory of the Future Theme
#'
#' Theme for plots in Factory of the Future publications, reports, Shiny Apps.
#'
#' @param base_size The base size of text elements of the plot. (default 12)
#' @param colour The background colour of the plot.
#' @param flipped TRUE to flip the y-axis guide lines to show on the x-axis instead.
#' @param legend The position of the legend (default no legend)
#' @param markdown Whether to use markdown formatting for plot titles (default FALSE)
#' @param legacy Whether to use legacy fonts
#'
#' @return ggplot2 theme
#' @export

#' @description
#' `r lifecycle::badge("deprecated")`
#' `theme_fof()` has been renamed `theme_nrc()`

theme_fof <- function(base_size = 14,
                      colour = lifecycle::deprecated(),
                      legend = "none",
                      markdown = lifecycle::deprecated(),
                      flipped = FALSE,
                      legacy = lifecycle::deprecated()) {

  lifecycle::deprecate_warn("0.0.3", "theme_fof()", "theme_nrc()")

  theme_nrc(base_size,
            legend,
            flipped)

}

#' ggplot2 theme for Australian Manufacturing Workers Union
#'
#' Theme for plots in publications, reports, Shiny apps, etc. Anywhere ggplot2 is used, you can use this theme!
#'
#' @param base_size The base size of text elements of the plot. (default 12)
#' @param legend The position of the legend. (default "none")
#' @param flipped Whether to flip the y-axis guide lines to show on the x-axis instead (default FALSE)
#'
#' @returns a ggplot2 theme
#' @export
#' @importFrom ggplot2 element_line element_rect element_text element_blank rel margin unit '%+replace%'
#' theme_sub_axis theme_sub_legend theme_sub_panel theme_sub_strip theme_sub_plot theme_sub_axis_bottom
#' theme_sub_axis_left theme theme_grey
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = c("One", "Two", "Three"), y = c(4, 2, 9))
#' p <- ggplot(df, aes(x = x, y = y, fill = x)) + geom_col()
#' p + theme_nrc()
theme_nrc <- function(base_size = 12,
                      legend = "none",
                      ink = midnight,
                      paper = white,
                      flipped = FALSE) {

  stopifnot(legend %in% c("none", "top", "bottom", "left", "right"))

  base_family <- "Arial"
  systemfonts::require_font("Arial")

  thm <- theme_bw(base_size = base_size,
           base_family = base_family,
           header_family = "",
           base_line_size = base_size / 22,
           base_rect_size = base_size /22,
           ink = ink,
           paper = paper) %+replace%
    theme(palette.colour.discrete = make_amwu_pal(),
          palette.fill.discrete = make_amwu_pal(),
          palette.colour.continuous = make_amwu_pal()(256),
          palette.fill.continuous = make_amwu_pal()(256),
          line = element_line(linetype = 1, colour = ink, linewidth = 0.25),
          text = element_text(colour = ink, lineheight = 0.9, size = base_size)) +
    theme_sub_plot(
      title = ggtext::element_textbox_simple(face = "bold", colour = ink, family = base_family, margin = margin(0, 0, 5, 0)),
      subtitle = ggtext::element_textbox_simple(family = base_family, margin = margin(5,0,0,0)),
      margin = unit(c(1,1,1,1), "lines"),
      caption = ggtext::element_textbox_simple(face = "italic", margin = margin(2.5, 0, 0, 0))
    ) +
    theme_sub_strip(
      background = element_blank(),
      text = ggtext::element_textbox(
        size = rel(1),
        colour = "white",
        fill = lagoon,
        halign = 0.5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    ) +
    theme_sub_panel(
      grid.major.x = element_blank(),
      grid.minor = element_blank(),
      border = element_blank(),
      background = element_rect(fill = NA),
      grid = element_line(colour = ink, linewidth = 0.15),
    ) +
    theme_sub_axis(
      title = element_text(family = base_family, size = rel(1.5)),
      text = element_text(family = base_family,face = "bold", size = rel(0.85)),
      ticks.length = unit(4, "pt"),
      line = element_line(linewidth = 0.35, colour = ink)
    ) +
    theme_sub_axis_bottom(
      title = ggtext::element_markdown(margin = margin(t = 6), vjust = 1),
    ) +
    theme_sub_axis_left(
      title = ggtext::element_markdown(angle = 90,vjust = 1),
      ticks = element_blank(),
      line = element_blank()
    ) +
    theme_sub_legend(
      background = element_rect(),
      key.size = unit(16, "pt"),
      text = element_text(size = rel(0.85)),
      box.spacing = unit(0, "pt"),
      position = "bottom",
      direction = "horizontal",
      justification = "left",
      title = element_blank(),
      box = "vertical"
    )



  if (flipped) {
    thm <- thm %+replace%
      ggplot2::theme(panel.grid.major.x = element_line(),
                     panel.grid.major.y = element_blank())
  }



  if (legend == "none") {
    thm <- thm %+replace%
      ggplot2::theme(legend.position = "none")
  } else {
    thm <- thm %+replace%
      ggplot2::theme(legend.position = legend)
  }

  thm

}
