\name{write.grid}
\alias{write.grid}
\title{
Export a grd object as csv
}
\description{
The function \code{\link{make.grid}} creates a grd object from x, y, and z data and this function converts it back to x, y, z and saves it as csv.
}
\usage{
write.grid(grd, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{
a grid object created by the function \code{\link{make.grid}}.
}
  \item{file}{
output filename.
}
}
\value{
A csv file with three columns corresponing to \code{x}, \code{y} and \code{z} (lon, lat, value).
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{make.grid}}.
}
\examples{
library(shapefiles)
library(rgdal)
data(landings)
data(coast)
byx = 1
byy = 0.5
xlim <- c(-15.5,0)
ylim <- c(50.25,56)
grd <- make.grid(landings$Lon, landings$Lat, landings$LiveWeight, byx, byy, xlim, ylim)
breaks <- breaks.grid(grd,zero=FALSE)
basemap(xlim, ylim, main = 'Gadoid landings')
draw.grid(grd,breaks)
draw.shape(coast, col='darkgreen')
legend.grid('topright', breaks=breaks/1000, type=2, round=1)
\dontrun{
write.grid(grd,'c:/test1.csv')
}
}
\keyword{file}
