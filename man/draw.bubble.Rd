\name{draw.bubble}
\alias{draw.bubble}
\title{
Draw bubble plots in an existing plot
}
\description{
Bubble plots are plots of circles whose surfaces areas are proportional to values in \code{z}.
}
\usage{
draw.bubble(x, y, z, maxradius = 1, ...)
}
\arguments{
  \item{x,y}{
vector with x and y-locations of the centre of the bubbles.
}
  \item{z}{
vector with positive values to correspond with the surface area of the bubbles.
}
  \item{maxradius}{
the radius of the largest bubble in units of the y-scale.
}
  \item{\dots}{
other arguments to be passed to \code{\link{points}}
}
}
\author{
Hans Gerritsen
}
\examples{
data(landings)
data(coast)
xlim <- c(-12,-5)
ylim <- c(50,56)
agg <- aggregate(list(z=landings$LiveWeight),list(x=landings$Lon,y=landings$Lat),sum)
basemap(xlim, ylim, main = 'Gadoid landings')
draw.shape(coast, col='cornsilk')
draw.bubble(agg$x, agg$y, agg$z, maxradius=0.5, pch=21, bg='#00FF0050')
legend.z <- round(max(agg$z)/1000,0)
legend.bubble('topright', z=legend.z, maxradius=0.5, inset=0.02, bg='lightblue', txt.cex=0.8,
 pch=21, pt.bg='#00FF0050')
}
\keyword{aplot}
\keyword{graphs}