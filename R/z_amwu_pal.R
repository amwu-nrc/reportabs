amwu_palettes <- list(
  `main` = c(vivid_orange,
             steel_blue,
             sky_blue),
  `grey` = c(deep_grey,
             mercury,
             silver,
             white),
  `sequential` = c(vivid_orange, sky_blue)
)

make_amwu_pal <- function(palette = "main",
                          reverse = FALSE,
                          ...) {

  pal <- amwu_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ..., interpolate = "spline")
}

make_amwu_pal_discrete <- function(n) {
  pal <- amwu_palettes[["main"]][1:n]
}
