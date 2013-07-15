\name{legend.bubble}
\alias{legend.bubble}
\title{
Legend for bubble plot
}
\description{
Draw a legend for a bubble plot (\code{\link{draw.bubble}})
}
\usage{
legend.bubble(x, y = NULL, z, maxradius = 1, n = 3, round = 0, bty = "o", mab = 1.2, 
	bg = NULL, inset = 0, pch = 21, pt.bg = NULL, txt.cex = 1, 
	txt.col = NULL, font = NULL, ...)
}
\arguments{
  \item{x, y}{
the x and y co-ordinates to be used to position the legend. \code{x} can be a keyword (e.g. \code{"topleft"}). See \code{\link{legend}}.
}
  \item{z}{
either a single value representing the largest bubble, in which case \code{n} determines how many bubbles are displayed; or a vector of z values which will be to be shown as bubbles in the legend.
}
  \item{maxradius}{
the radius of the largest bubble. this should match \code{maxradius} in \code{\link{draw.bubble}}.
}
  \item{n}{
integer giving the number of bubbles that should be shown in the legend. Defaults to 3. Only relevant when z is a single value.
}
  \item{round}{
integer indicating the number of decimal places to be used in the legend.
}
  \item{bty}{
single character indicating the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" (no box).
}
  \item{mab}{
the margin around the bubble. i.e. how much space should there be between the largest bubble and the legend box. relative to the size of the bubble.
}
  \item{bg}{
background colour of the legend box.
}
  \item{inset}{
inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword.
}
  \item{pch}{
plotting character, i.e., symbol to use. Defaults to 21.
}
  \item{pt.bg}{
background (fill) color for the open plot symbols given by pch=21:25.
}
  \item{txt.cex}{
character expansion for the legend text.
}
  \item{txt.col}{
colour of the legend text.
}
  \item{font}{
An integer which specifies which font to use for text. See \code{\link{graphical parameters}}
}
  \item{\dots}{
any other arguments to be passed to \code{\link{points}}.
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
basemap(xlim, ylim, main = "Gadoid landings")
draw.shape(coast, col="cornsilk")
draw.bubble(agg$x, agg$y, agg$z, maxradius=0.5, pch=21, bg="#00FF0050")
legend.bubble("topright", z=round(max(agg$z)/1000,0), maxradius=0.5, inset=0.02, bg="lightblue",
 txt.cex=0.8, pch=21, pt.bg="#00FF0050")
}
\keyword{aplot}
\keyword{graphs}