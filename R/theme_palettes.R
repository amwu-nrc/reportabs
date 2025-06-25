#' Generate Factory of the Future colours from a pre-defined palette
#'
#' @param palette Name of colour palette. See `palette_names()` for a list of available palettes.
#' @param reverse Whether the order of the colours should be reversed (default FALSE)
#' @param ... Other arguments passed to `colorRampPalette()`
#'
#' @returns A colour palette
#' @export
#' @description
#' `r lifecycle::badge("deprecated")`
#' `fof_pal()` has been renamed `nrc_pal()`

fof_pal <- function(palette = "main", reverse = FALSE, ...) {

  lifecycle::deprecate_warn("0.0.3", "fof_pal()", "nrc_pal()")

  nrc_pal(palette, reverse, ...)
}

#' Generate Australian Manufacturing Workers Union colours from a pre-defined palette
#'
#' @param palette Name of colour palette. See `palette_names()` for a list of available palettes.
#' @param reverse Whether the order of the colours should be reversed (default FALSE)
#' @param ... Other arguments passed to `colorRampPalette()`
#'
#' @returns A colour palette
#' @export
#'
#' @examples
#' # To generate a sequence of n colours from the 'main' palette:
#' n <- 2
#' nrc_pal("main")(n)
nrc_pal <- function(palette = "main", reverse = FALSE, ...) {

  if (!palette %in% palette_names()) {
    cli::cli_abort(c("There is no palette called {.var palette}.",
                     "See `palette_names()` for a list of available palettes."))
  }

  pal <- nrc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)

}

#' List available palettes
#'
#' @return character vector of palette names
#' @export
#'
#' @examples palette_names()
palette_names <- function() {
  names(nrc_palettes)
}

#' List available colours
#'
#' @param colour A character of the name of the colour. If NULL, returns all available colours.
#'
#' @return A named character vector of the defined colours in the package.
#' @export
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `fof_cols()` has been renamed `nrc_cols()`
#'
fof_cols <- function(colour = NULL) {

  lifecycle::deprecate_warn("0.0.3", "fof_cols()", "nrc_cols()")
  nrc_cols(colour)
}

#' List available colours
#'
#' @param colour A character of the name of the colour. If NULL, returns all available colours.
#'
#' @returns A named character vector of the defined colours in the package.
#' @export
#'
#' @examples nrc_cols()
nrc_cols <- function(colour = NULL) {
  cols <- c(colour)

  if (is.null(cols))
    return (nrc_colours)

  nrc_colours[cols]
}



#' Visualise individual palettes
#'
#' @param palette Name of colour palette
#' @param n Number of colours required
#'
#' @returns NULL
#' @export
#'
#' @examples visualise_palette("just orange", n = 5)
visualise_palette <- function(palette, n) {

  cols <- nrc_pal(palette)(n)

  scales::show_col(cols)
}


