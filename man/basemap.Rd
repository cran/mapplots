\name{basemap}
\alias{basemap}
\title{
Draw a (blank) map 
}
\description{
A blank map is created that has approximatedly the correct aspect ratio for its latitude.
}
\usage{
basemap(xlim, ylim, xlab = "Longitude", ylab = "Latitude", bg = "lightblue", ...)
}
\arguments{
  \item{xlim}{
the x limits (x1, x2) of the plot.
}
  \item{ylim}{
the y limits of the plot.
}
  \item{xlab}{
a label for the x axis, defaults to "Longitude".
}
  \item{ylab}{
a label for the y axis, defaults to "Latitude".
}
  \item{bg}{
background colour for the map, defaults to "lightblue".
}
  \item{\dots}{
other arguments to be passed to \code{\link{plot.default}}.
}
}
\details{
The aspect ratio of the map is based on the approximation that the earth is a perfect sphere of 21600 nautical miles in circumference. The straight-line distance between two medidians that lie 1 degree apart is then 60 nautical miles \verb{*} cos(latitude). The aspect ratio is therefore set at the inverse of the cosine of the latitude at the middle of the y-scale.
If the plot window is re-sized the aspect ratio will remain correct but the background colour will not fill the full plot area.
}
\author{
Hans Gerritsen
}
\examples{
data(landings)
data(coast)
xlim <- c(-11,-5.5)
ylim <- c(51.5,55.5)
basemap(xlim, ylim)
draw.shape(coast, col="cornsilk")
}
\keyword{hplot}
