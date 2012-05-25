\name{legend.pie}
\alias{legend.pie}
\title{
Legend for pie plots
}
\description{
Draw a legend for a \code{\link{draw.pie}} plot.
}
\usage{
legend.pie(x, y = NULL, z, labels, radius = 1, bty = "o", mab = 1.2, bg = NULL, 
	inset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
the x and y co-ordinates to be used to position the legend. \code{x} can be a keyword (e.g. \code{'topleft'}. See \code{\link{legend}}.
}
  \item{z}{
optional vector of non-negative numerical quantities. The values in \code{z} are displayed as the areas of pie slices in the legend.
}
  \item{labels}{
character vector (same length as \code{z}) with labels for the pies.
}
  \item{radius}{
the radius of the pie to be displayed.
}
  \item{bty}{
single character indicating the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" (no box).
}
  \item{mab}{
the margin around the pie. i.e. how much space should there be between the pie and the legend box. relative to the size of the pie.
}
  \item{bg}{
background colour of the legend box.
}
  \item{inset}{
inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword.
}
  \item{\dots}{
any other arguments to be passed on to \code{\link{add.pie}}.
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
xyz <- make.xyz(landings$Lon,landings$Lat,landings$LiveWeight,landings$Species)
col <- rainbow(5)
basemap(xlim, ylim, main = 'Species composition of gadoid landings')
draw.shape(coast, col='cornsilk')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.3, col=col)
legend.pie(-13.25,54.8,labels=c('cod','had','hke','pok','whg'), radius=0.3, bty='n', col=col,
 cex=0.8, label.dist=1.3)
legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE))/10^6,0)
legend.bubble(-13.25,55.5,z=legend.z,round=1,maxradius=0.3,bty='n',txt.cex=0.6)
text(-13.25,56,'landings (kt)',cex=0.8)
}
