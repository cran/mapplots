\name{draw.shape}
\alias{draw.shape}
\title{
Draw shapefiles in an existing plot
}
\description{
Draw shapefiles in an existing plot
}
\usage{
draw.shape(shape, type = "poly", col = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shape}{
a shape list object created by (\code{\link[shapefiles]{shapefiles}}
}
  \item{type}{
type of plot desired. The following values are possible: \code{"p"} for points, \code{"l"} or \code{"lines"} for lines and \code{"poly"} (default) for polygons.
}
  \item{col}{
the colour of the points, lines or polygons
}
  \item{\dots}{
other arguments to be passed to \code{\link{points}}, \code{\link{lines}} or \code{\link{polygon}}
}
}
\note{
The shapefile needs to have the WGS 84 Geographic Coordinate System in order to display properly on a map of longitude and latitude.
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link[shapefiles]{read.shapefile}}
}
\examples{
library(shapefiles)
shp.file <- file.path(system.file(package = "mapplots", "extdata"), "Ireland")
irl <- read.shapefile(shp.file)
xlim <- c(-11,-5.5)
ylim <- c(51.5,55.5)
basemap(xlim, ylim)
draw.shape(irl, col="cornsilk")
}
\keyword{aplot}
\keyword{graphs}