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
                       colour = "Soft Black",
                       legend = "none",
                       markdown = FALSE,
                       flipped = FALSE,
                       legacy = FALSE) {

  lifecycle::deprecate_warn("0.0.3", "theme_fof()", "theme_nrc()")

  theme_nrc(base_size,
            colour,
            legend,
            markdown,
            flipped,
            legacy)

}

#' ggplot2 theme for Australian Manufacturing Workers Union
#'
#' Theme for plots in publications, reports, Shiny apps, etc. Anywhere ggplot2 is used, you can use this theme!
#'
#' @param base_size The base size of text elements of the plot. (default 12)
#' @param colour The background colour of the plot. Background colours are applied at 80% transparency. (default 'Soft Black')
#' @param legend The position of the legend. (default "none")
#' @param markdown `r lifecycle::badge("experimental")` Whether to use markdown formatting for plot titles (default FALSE)
#' @param flipped Whether to flip the y-axis guide lines to show on the x-axis instead (default FALSE)
#' @param legacy `r lifecycle::badge("deprecated")` Whether to use legacy fonts. This option will be removed sometime in the future.
#'
#' @returns a ggplot2 theme
#' @export
#' @importFrom ggplot2 element_line element_rect element_text element_blank rel margin unit '%+replace%'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = c("One", "Two", "Three"), y = c(4, 2, 9))
#' p <- ggplot(df, aes(x = x, y = y, fill = x)) + geom_col()
#' p + theme_nrc()
theme_nrc <- function(base_size = 12,
                      colour = "Sand",
                      legend = "none",
                      markdown = FALSE,
                      flipped = FALSE,
                      legacy = FALSE) {

  stopifnot(legend %in% c("none", "top", "bottom", "left", "right"))

  col <- nrc_colours[colour]

  bg_colour <-grDevices::col2rgb(col) + (255 - grDevices::col2rgb(col))*0.8

  bg_colour <- grDevices::rgb(bg_colour[1],
                              bg_colour[2],
                              bg_colour[3],
                              maxColorValue = 255)

  base_family <- if (legacy) "Roboto" else "DM Sans"

  sysfonts::font_add_google("Roboto", "Roboto")
  sysfonts::font_add_google("DM Sans", "DM Sans")
  showtext::showtext_auto()

  ggplot2::update_geom_defaults("smooth", ggplot2::aes(color = nrc_colours["AMWU Orange"]))

  thm <- theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(line = element_line(linetype = 1, colour = "black", linewidth = 0.25),
                   rect = element_rect(fill = bg_colour,
                                       linetype = 0,
                                       colour = NA),
                   text = element_text(colour = "black",
                                       lineheight = 0.9,
                                       size = base_size,
                                       family = base_family),
                   axis.title = element_text(family = base_family, size = rel(1)),
                   axis.title.x = element_text(margin = margin(t = 6),
                                               vjust = 1),
                   axis.title.y = element_text(angle = 90,
                                               vjust = 1),
                   axis.line = element_line(linewidth = 0.35, colour = "black"),
                   axis.line.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.line.x = NULL,
                   axis.text = element_text(family = base_family, size = rel(1)),
                   axis.text.x = element_text(colour = NULL),
                   axis.text.y = element_text(colour = NULL),
                   axis.ticks = element_line(colour = "black"),
                   axis.ticks.length = unit(6, "pt"),
                   axis.ticks.length.x = NULL,
                   axis.ticks.length.x.top = NULL,
                   axis.ticks.length.x.bottom = NULL,
                   axis.ticks.length.y = NULL,
                   axis.ticks.length.y.left = NULL,
                   axis.ticks.length.y.right = NULL,
                   legend.background = element_rect(),
                   legend.key.size = unit(32, "pt"),
                   legend.text = element_text(size = rel(0.75)),
                   legend.box.spacing = unit(0, "pt"),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.justification = "left",
                   legend.title = element_blank(),
                   legend.box = "vertical",
                   panel.grid = element_line(colour = "black", linewidth = 0.15),
                   panel.grid.major.x = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.title = ggtext::element_textbox_simple(face = "bold", family = base_family, margin = margin(0, 0, 5, 0)),
                   plot.subtitle = ggtext::element_textbox_simple(family = base_family, margin = margin(5,0,0,0)),
                   plot.margin = unit(c(1,1,1,1), "lines"),
                   plot.caption = ggtext::element_textbox_simple(margin = margin(2.5, 0, 0, 0)),
                   strip.background = element_rect()
    )

  if (flipped) {
    thm <- thm %+replace%
      ggplot2::theme(panel.grid.major.x = element_line(),
                     panel.grid.major.y = element_blank())
  }

  if (markdown) {
    thm <- thm %+replace%
      ggplot2::theme(
        plot.title = ggtext::element_markdown(
          hjust = 0,
          vjust = 1,
          face = "bold",
          margin = margin(b = base_size / 2)
        ),
        plot.subtitle = ggtext::element_markdown(
          hjust = 0,
          vjust = 1,
          lineheight = 1.2
        ),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())
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
