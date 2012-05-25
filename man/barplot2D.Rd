\name{barplot2D}
\alias{barplot2D}
\title{
Draw 2-dimensional barplots 
}
\description{
This function is used by \code{\link{draw.barplot2D}} to add a 2-dimensional barplot at a specific location to an existing plot. It can be used directly but in general it is advisable to use \code{\link{draw.barplot2D}} instead.
}
\usage{
barplot2D(z, x = 0, y = 0, width = 1, height = 1, colour, add = TRUE, col.frame = NULL, 
	lwd.frame = 1, threshold = 1.1, ...)
}
\arguments{
  \item{z}{
vector, containing positive numbers (NAs are discarded), to be used as the area of the rectangles 
}
  \item{colour}{
a vector (same length as \code{z}) of colors to be used in filling the rectangles
}
  \item{x, y}{
the location of the centre of the pie on the x and y-scale of the existing plot.
}
  \item{width, height}{
the width and height of the 2D barplot (user coordinate units).
}
  \item{add}{
logical, should the 2D barplot be added to an existing plot> Defaults to TRUE
}
  \item{col.frame}{
the colour of the frame of the 2D barplot
}
  \item{lwd.frame}{
the line width of the frame of the 2D barplot
}
  \item{threshold}{
the maximum acceptable aspect ratio of the rectangles 
}
  \item{\dots}{
arguments to be passed to \code{\link{polygon}}
}
}
\details{
The algorithm that determines the location of each rectangle within the 2D-barplot is as follows: 1) Start with a rectangle representing the highest value of \code{z}. 2) Try to put the first rectangle on the left. 3) If it too elongated, try to put two rectangles, on top of each other, on the left. 4) When you have placed those rectangles, proceed with the remaining rectangles.

More precisely, we choose the number of rectables to stack so as to minimize the following penalty: penalty for the first rectangle in the stack + penalty for the last where the penalty of a rectangle is: \code{ratio - 1.1}. where \code{ratio} is the ratio of the longer side by the smaller.
}
\references{
This function was adapted from code pulbished on \url{http://zoonek2.free.fr/UNIX/48_R/03.html} (accessed 1 Jun 2012).
}
\author{
Adapted by Hans Gerritsen
}
\seealso{
\code{\link{draw.barplot2D}}
}
\examples{
plot(NA,NA, xlim=c(-1,1), ylim=c(-1,1) )
barplot2D(z=rpois(6,10), x=-0.5, y=0.5, width=0.75, height=0.75, colour=rainbow(6))
barplot2D(z=rpois(4,10), x=0.5, y=-0.5, width=0.5, height=0.5, colour=rainbow(4))
}
\keyword{aplot}
\keyword{graphs}
