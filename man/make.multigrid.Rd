\name{make.multigrid}
\alias{make.multigrid}
\title{
Create a grd object
}
\description{
Create list of grd objects from \code{x}, \code{y}, \code{z} and \code{group} data, where each level of \code{group} provides a separate grd object. For use with \code{\link{draw.grid}}.
}
\usage{
make.multigrid(x, y, z, group, ...)
}
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
  \item{group}{
a factor; same length as \code{x}. E.g. species, season etc.
}
  \item{\dots}{
other arguments to be passed to \code{\link{make.grid}}.
}
}
\value{
a list of grd objects, see  \code{\link{make.grid}}.
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{make.grid}}, \code{\link{draw.grid}}
}
\examples{
data(coast)
data(landings)
byx = 1
byy = 0.5
xlim <- c(-12.5,-5)
ylim <- c(50.25,56)
grd <- make.multigrid(landings$Lon,landings$Lat,landings$LiveWeight, landings$Species,
 byx, byy, xlim, ylim)
breaks <- breaks.grid(grd,zero=FALSE)
par(mfrow=c(2,3),mar=c(1,1,2,1) )
for(s in names(grd) ) {
  basemap(xlim, ylim, main = s, axes=FALSE)
  draw.grid(grd[[s]],breaks)
  draw.shape(coast, col="darkgreen")
}
plot.new()
legend.grid("center",breaks=breaks/1000,type=2)
}
