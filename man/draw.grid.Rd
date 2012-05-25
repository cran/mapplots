\name{draw.grid}
\alias{draw.grid}
\title{
Display a grd object as a heatmap
}
\description{
Displays a grid of colored or gray-scale rectangles with colors corresponding to the values in z. This can be used to display three-dimensional or spatial data as images.
}
\usage{
draw.grid(grd, breaks = NULL, col = NULL)
}
\arguments{
  \item{grd}{
a 2-dimensional array with data to be plotted. The row and column names should should correspond to the x and y locations of the gridlines at which the values are displayed. see \code{\link{make.grid}}.
}
  \item{breaks}{
a vector of breakpoints for the colours, must give one more breakpoint than colour.
}
  \item{col}{
a vector with colours. defaults to 12 colors ranging from white (lowest) through yellow and orange to red (highest).
}
}
\author{
Hans Gerritsen
}
\note{
The resulting maps are often referred to as heat maps although this is not strictly correct as red would imply the lowest and white the highest value (an object glowing white is hotter than an object glowing red). However using the scale in this way is counter-intuitive.
}
\seealso{
\code{\link{image}}
}
\examples{
data(coast)
data(landings)
byx = 1
byy = 0.5
xlim <- c(-15.5,0)
ylim <- c(50.25,56)
grd <- make.grid(landings$Lon,landings$Lat,landings$LiveWeight, byx, byy, xlim, ylim)
breaks <- breaks.grid(grd,zero=FALSE)
basemap(xlim, ylim, main = 'Gadoid landings')
draw.grid(grd,breaks)
draw.shape(coast, col='darkgreen')
legend.grid('topright', breaks=breaks/1000, type=2, inset=0.02, title='tonnes')
}
\keyword{aplot}
\keyword{graphs}