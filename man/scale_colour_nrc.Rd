% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colour_scale.R
\name{scale_colour_nrc}
\alias{scale_colour_nrc}
\title{Colour Scale for Australian Manufacturing Workers Union}
\usage{
scale_colour_nrc(palette = "main", discrete = TRUE, reverse = FALSE)
}
\arguments{
\item{palette}{Name of the colour palette (default "main")}

\item{discrete}{Whether the colours should be discrete or continuous (default TRUE)}

\item{reverse}{Whether the order of the colours should be reversed (default FALSE)}
}
\description{
Colour Scale for Australian Manufacturing Workers Union
}
\examples{
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = disp, col = factor(cyl))) + geom_point()
p + scale_colour_nrc()
}
