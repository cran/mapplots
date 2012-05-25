\name{geoTiff}
\alias{geoTiff}
\alias{geoTiffRgb}
\title{
Export a grd object as geotiff
}
\description{
Export a grd object as geotiff, \code{z} data are exported as values (\code{geoTiff}) or as RGB colours (\code{geoTiffRgb}).
}
\usage{
geoTiff(grd, file)
geoTiffRgb(grd, breaks, col, file)
}
\arguments{
  \item{grd}{
a grid object created by the function \code{\link{make.grid}}.
}
  \item{breaks}{
a vector of breakpoints for the colours, must give one more breakpoint than colour.
}
  \item{col}{
a vector with colours. defaults to 12 colors ranging from white (lowest) through yellow and orange to red (highest).
}
  \item{file}{
output filename
}
}
\details{
These functions do not always seem result in geoTiffs that display properly. It seems to depend on the mapping software if they do. See \code{\link[rgdal]{writeGDAL}} for more details.
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link[rgdal]{writeGDAL}} 
}
\examples{
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
geoTiff(grd,'c:/test1.tiff')
geoTiffRgb(grd,breaks,file='c:/test2.tiff')
}
}
\keyword{file}
