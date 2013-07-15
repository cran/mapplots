\name{add.pie}
\alias{add.pie}
\title{
Add pie plot to existing plot
}
\description{
This function is used by \code{\link{draw.pie}} to add a pie plot at a specific location to an existing plot. It can be used directly but in general it is advisable to use \code{\link{draw.pie}} instead.
}
\usage{
add.pie(z, x = 0, y = 0, labels = names(z), radius = 1, edges = 200, clockwise = 
  TRUE, init.angle = 90, density = NULL, angle = 45, col = NULL, border = NULL, 
  lty =  NULL, label.dist = 1.1, ...)
}
\arguments{
  \item{z}{
a vector of non-negative numerical quantities. The values in \code{z} are displayed as the areas of pie slices.
}
  \item{x, y}{
the location of the centre of the pie on the x and y-scale of the existing plot.
}
  \item{labels}{
one or more expressions or character strings giving names for the slices. Other objects are coerced by \code{\link{as.graphicsAnnot}}. For empty or NA (after coercion to character) labels, no label nor pointing line is drawn.
}
  \item{radius}{
the radius of the pie in units of the y-scale
}
  \item{edges}{
the circular outline of the pie is approximated by a polygon with this many edges.
}
  \item{clockwise}{
logical indicating if slices are drawn clockwise or counter clockwise (i.e., mathematically positive direction), the former is default.
}
  \item{init.angle}{
number specifying the starting angle (in degrees) for the slices. see \code{\link[graphics]{pie}} for details.
}
  \item{density}{
the density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.
}
  \item{angle}{
the slope of shading lines, given as an angle in degrees (counter-clockwise).
}
  \item{col}{
a vector of colors to be used in filling or shading the slices. If missing a set of 6 pastel colours is used, unless density is specified when par("fg") is used.
}
  \item{border}{
(possibly vector) argument passed to polygon which draws each slice.
}
  \item{lty}{
(possibly vector) argument passed to polygon which draws each slice.
}
  \item{label.dist}{
distance that the label is placed away from the pie (relative to the radius)
}
  \item{\dots}{
\code{\link{graphical parameters}} can be given as arguments to pie. They will affect the main title and labels only.
}
}
\details{
Because this function is intended to add pie plots to a map, the radius is scaled to units on the y-scale. This is more convenient than using the x-scale as 1 degree latitude is exactly 60 nautical miles. (The conversion from degrees to distance on the x-scale is less straightforward as it depends on the latitude).
}
\note{
The function obtaines the aspect ratio of the current plot from \code{\link{get.asp}} in order to draw circular pies. If the plot window is re-sized and \code{asp} is not defined in \code{\link{par}} then the pies will end up being oval.
}
\author{
Adapted from the function \code{\link[graphics]{pie}} by Hans Gerritsen
}
\seealso{
\code{\link{draw.pie}} 
}
\examples{
plot(NA,NA, xlim=c(-1,1), ylim=c(-1,1) )
add.pie(z=rpois(6,10), x=-0.5, y=0.5, radius=0.5)
add.pie(z=rpois(4,10), x=0.5, y=-0.5, radius=0.3)
}
\keyword{aplot}
\keyword{graphs}
