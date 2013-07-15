\name{make.grid}
\alias{make.grid}
\title{
Create grd object
}
\description{
Create a grd object from \code{x}, \code{y} and \code{z} data.
For use with \code{\link{draw.grid}} and \code{\link{write.grid}}.
}
\usage{
make.grid(x, y, z, byx , byy , xlim, ylim, fun = function(x) sum(x, na.rm = T))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of x-coordinates (longitude)
}
  \item{y}{
a vector of y-coordinates (latitude; same length as \code{x})
}
  \item{z}{
a vector of values; same length as \code{x}
}
  \item{byx, byy}{
the size of the grid cells on the x and y scale
}
  \item{xlim, ylim}{
the x and y limits of the grid. Note that the origin of the grid depends on \code{xlim[1]} and \code{ylim[1]}, these values will be taken as the mid-point of the bottom-left grid-cell. See example below how to use this to create a grid that matches that of the ICES rectangles.
}
  \item{fun}{
a function to be applied to \code{z}. Defaults to \code{sum}.
}
}
\details{
Due to the way fractions are stored in binary format, rounding errors can occur, e.g.:

 \code{as.character(seq(-5,0,by=0.8))} 
 
 results in:
 
 \code{"-5"	"-4.2"	"-3.4"	"-2.6"	"-1.8"	"-1"	"-0.199999999999999"}

this can affect the \code{make.grid} function although this is generally not a problem.
}
\value{
a grd object, which is simply a 2-dimensional array with row and column names that correspond to the x and y positions of the grid.
}
\author{
Hans Gerritsen
}
\seealso{
 \code{\link{draw.grid}} 
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
basemap(xlim, ylim, main = "Gadoid landings")
draw.grid(grd,breaks)
draw.shape(coast, col="darkgreen")
legend.grid("topright", breaks=breaks/1000, type=2, inset=0.02, title="tonnes")
}