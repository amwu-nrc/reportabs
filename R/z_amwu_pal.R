amwu_palettes <- list(
  `main` = c(amwu_yellow,
             amwu_orange,
             amwu_grey,
             amwu_darkgrey,
             amwu_blue,
             amwu_darkblue),
  `sequential` = c(amwu_orange, amwu_yellow),
  `diverging` = c(amwu_orange, amwu_yellow, amwu_blue)
)

make_amwu_pal <- function(palette = "sequential",
                          reverse = FALSE,
                          ...) {

  pal <- amwu_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ..., interpolate = "spline")
}

make_amwu_pal_discrete <- function(n) {
  pal <- amwu_palettes[["main"]][1:n]
}
