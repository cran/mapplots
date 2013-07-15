\name{draw.pie}
\alias{draw.pie}
\title{
Draw pie plots in an existing plot
}
\description{
Draw pie plots in an existing plot
}
\usage{
draw.pie(x, y, z, radius, scale = T, labels = NA, silent = TRUE, ...)
}
\arguments{
  \item{x, y}{
vector with x and y-locations of the centre of the pies
}
  \item{z}{
array where the rows correspond to \code{x} and \code{y} and the columns correspond to categories to be plotted. The function \code{\link{make.xyz}} can be useful to create \code{z}.
}
  \item{radius}{
the radius of the (largest) pie (y-scale units). This can be a single value or a vector with the same length as \code{x}.
}
  \item{scale}{
logical, should the surface area of each pie plot automatically be scaled to the sum of its z-values? Only works if \code{radius} is a vector with a length of 1.
}
  \item{labels}{
labels for each slice. Defaults to NA, labels are probably best placed in a legend by legend.pie.
}
  \item{silent}{
logical, should a progress message be displayed in the console? Defaults to \code{FALSE}.
}
  \item{\dots}{
other arguments to be passed to \code{\link{add.pie}}
}
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{add.pie}}
}
\examples{
data(landings)
data(coast)
xlim <- c(-12,-5)
ylim <- c(50,56)
xyz <- make.xyz(landings$Lon,landings$Lat,landings$LiveWeight,landings$Species)
col <- rainbow(5)
basemap(xlim, ylim, main = "Species composition of gadoid landings")
draw.shape(coast, col="cornsilk")
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.3, col=col)
legend.pie(-13.25,54.8,labels=c("cod","had","hke","pok","whg"), radius=0.3, bty="n", col=col,
 cex=0.8, label.dist=1.3)
legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE))/10^6,0)
legend.bubble(-13.25,55.5,z=legend.z,round=1,maxradius=0.3,bty="n",txt.cex=0.6)
text(-13.25,56,"landings (kt)",cex=0.8) 
}
\keyword{aplot}
\keyword{graphs}