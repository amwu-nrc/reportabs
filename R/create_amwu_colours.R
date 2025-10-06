# Following how Grattan define/set their colours/palettes
# https://github.com/grattan/grattantheme/blob/master/R/grattan_pal.R

base_yellow <- "#febe10"
base_blue <- "#094fa3"
base_darkblue <- "#001d26"
base_grey <- "#e4e4e4"
base_darkgrey <- "#231f20"
base_orange <- "#f15c30"

yellow_palette <- grDevices::colorRampPalette(c(base_yellow, "white"))(10)

#' amwu_yellow
#'
#' @export
#'
amwu_yellow <- yellow_palette[1]

#' amwu_yellow1
#'
#' @export
#'
amwu_yellow1 <- yellow_palette[2]

#' amwu_yellow2
#'
#' @export
#'
amwu_yellow2 <- yellow_palette[3]

#' amwu_yellow3
#'
#' @export
#'

amwu_yellow3 <- yellow_palette[4]

#' amwu_lightyellow4
#'
#' @export
#'
amwu_yellow4 <- yellow_palette[5]

#' amwu_yellow5
#'
#' @export
#'
amwu_yellow5 <- yellow_palette[6]

#' amwu_yellow6
#'
#' @export
#'
amwu_yellow6 <- yellow_palette[7]

#' amwu_yellow7
#'
#' @export
#'
amwu_yellow7 <- yellow_palette[8]

#' amwu_yellow8
#'
#' @export
#'
amwu_yellow8 <- yellow_palette[9]

orange_palette <- grDevices::colorRampPalette(c(base_orange, "white"))(10)

#' amwu_orange
#'
#' @export
#'
amwu_orange <- orange_palette[1]

#' amwu_orange1
#'
#' @export
#'
amwu_orange1 <- yellow_palette[2]

#' amwu_yellow2
#'
#' @export
#'
amwu_yellow2 <- orange_palette[3]

#' amwu_orange3
#'
#' @export
#'

amwu_orange3 <- orange_palette[4]

#' amwu_orange4
#'
#' @export
#'
amwu_orange4 <- orange_palette[5]

#' amwu_orange5
#'
#' @export
#'
amwu_orange5 <- orange_palette[6]

#' amwu_orange6
#'
#' @export
#'
amwu_orange6 <- orange_palette[7]

#' amwu_orange7
#'
#' @export
#'
amwu_orange7 <- orange_palette[8]

#' amwu_orange8
#'
#' @export
#'
amwu_orange8 <- orange_palette[9]

blue_palette <- grDevices::colorRampPalette(c(base_blue, "white"))(10)

#' amwu_blue
#'
#' @export
#'
amwu_blue <- blue_palette[1]

#' amwu_blue1
#'
#' @export
#'
amwu_blue1 <- blue_palette[2]

#' amwu_blue2
#'
#' @export
#'
amwu_blue2 <- blue_palette[3]

#' amwu_blue3
#'
#' @export
#'
amwu_blue3 <- blue_palette[4]

#' amwu_blue4
#'
#' @export
#'
amwu_blue4 <- blue_palette[5]

#' amwu_blue5
#'
#' @export
#'
amwu_blue5 <- blue_palette[6]

#' amwu_blue6
#'
#' @export
#'
amwu_blue6 <- blue_palette[7]

#' amwu_blue7
#'
#' @export
#'
amwu_blue7 <- blue_palette[8]

#' amwu_blue8
#'
#' @export
#'
amwu_blue8 <- blue_palette[9]

darkblue_palette <- grDevices::colorRampPalette(c(base_darkblue, "white"))(10)

#' amwu_darkblue
#'
#' @export
#'
amwu_darkblue <- darkblue_palette[1]

#' amwu_darkblue1
#'
#' @export
#'
amwu_darkblue1 <- darkblue_palette[2]

#' amwu_darkblue2
#'
#' @export
#'
amwu_darkblue2 <- darkblue_palette[3]

#' amwu_darkblue3
#'
#' @export
#'
amwu_darkblue3 <- darkblue_palette[4]

#' amwu_darkblue4
#'
#' @export
#'
amwu_darkblue4 <- darkblue_palette[5]

#' amwu_darkblue5
#'
#' @export
#'
amwu_darkblue5 <- darkblue_palette[6]

#' amwu_darkblue6
#'
#' @export
#'
amwu_darkblue6 <- darkblue_palette[7]

#' amwu_darkblue7
#'
#' @export
#'
amwu_darkblue7 <- darkblue_palette[8]

#' amwu_darkblue8
#'
#' @export
#'
amwu_darkblue8 <- darkblue_palette[9]

#' amwu_grey
#'
#' @export
#'
amwu_grey <- base_grey

#' amwu_transparentgrey
#'
#' @export
#'
amwu_transparentgrey <- grDevices::col2rgb(base_grey) + (255 - grDevices::col2rgb(base_grey))*0.8
amwu_transparentgrey <- grDevices::rgb(amwu_transparentgrey[1], amwu_transparentgrey[2], amwu_transparentgrey[3], maxColorValue = 255)

#' amwu_darkgrey
amwu_darkgrey <- base_darkgrey
