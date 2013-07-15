\name{write.grid}
\alias{write.grid}
\title{
Export a grd object as csv or shapefile
}
\description{
Export a grd object (created by \code{\link{make.grid}}) as csv or shapefile.
}
\usage{
write.grid(grd, file, type="csv")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{
a grid object created by the function \code{\link{make.grid}}.
}
  \item{file}{
character string naming the output filename. For csv files do include the extension (e.g. "my_file.csv"); for shapefiles omit the extension (e.g. "my_file").
}
  \item{type}{
character string specifying the output file type. Must be one of "csv" (default) or "shape"
}
}
\value{
A csv file with three columns corresponing to \code{x}, \code{y} and \code{z} (lon, lat, value), or a shapefile with a polygon for each cell in the grd object. The assumed projection is EPSG:4326
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{make.grid}}, \code{\link[shapefiles]{write.shapefile}} .
}
\examples{
library(shapefiles)
data(landings)
data(coast)
byx = 1
byy = 0.5
xlim <- c(-15.5,0)
ylim <- c(50.25,56)
grd <- make.grid(landings$Lon, landings$Lat, landings$LiveWeight, byx, byy, xlim, ylim)
breaks <- breaks.grid(grd,zero=FALSE)
basemap(xlim, ylim, main = "Gadoid landings")
draw.grid(grd,breaks)
draw.shape(coast, col="darkgreen")
legend.grid("topright", breaks=breaks/1000, type=2)
\dontrun{
write.grid(grd,"c:/test1.csv")
write.grid(grd,"c:/test1","shape")
}
}
\keyword{file}
