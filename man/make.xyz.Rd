\name{make.xyz}
\alias{make.xyz}
\title{
Create xyz object
}
\description{
Creata an xyz object for use with the functions \code{\link{draw.barplot2D}} and \code{\link{draw.pie}}
}
\usage{
make.xyz(x, y, z, group, FUN = sum, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
vector with x and y-locations of the 2D barplots or pies to be plotted.
}
  \item{z}{
vector (same length as \code{x}) with the values to be displayed as the areas of the pie slices or 2D barplot.
}
  \item{group}{
factor (same length as \code{x}) with groupings for the \code{x}, \code{y}, \code{z} data, e.g. year, species etc.
}
  \item{FUN}{
function to apply to the \code{z}(in case there are duplicate combinations of \code{x}, \code{y} and \code{group}).
}
  \item{\dots}{
optional arguments to \code{FUN}.
}
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{draw.barplot2D}} and \code{\link{draw.pie}}
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
legend.pie(-13.25,54.8,labels=c("cod","had","hke","pok","whg"), radius=0.3, bty="n",
 col=col, cex=0.8, label.dist=1.3)
legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE))/10^6,0)
legend.bubble(-13.25,55.5,z=legend.z,round=1,maxradius=0.3,bty="n",txt.cex=0.6)
text(-13.25,56,"landings (kt)",cex=0.8) 
}
