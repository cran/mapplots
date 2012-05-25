\name{draw.barplot2D}
\alias{draw.barplot2D}
\title{
Draw 2-dimensional barplots in an existing plot
}
\description{
2-Dimensional barplots are essentially rectangular pieplots. These plots can be used to display proportional data in certain locations on a map.
}
\usage{
draw.barplot2D(x, y, z, width, height, scale = F, col = NULL, col.frame = "black", 
	lwd.frame = 1, silent = TRUE, ...)
}
\arguments{
  \item{x, y}{
vector with x and y-locations of the centre of the 2D barplots
}
  \item{z}{
array where the rows correspond to \code{x} and \code{y} and the columns correspond to categories to be plotted. The function \code{\link{make.xyz}} can be useful to create \code{z}.
}
  \item{width}{
the width of the (largest) 2D barplot (user coordinate units). This can be a single value or a vector with the same length as \code{x}.
}
  \item{height}{
the width of the (largest) 2D barplot see \code{width}
}
  \item{scale}{
logical, should the surface area of each 2d-barplot automatically be scaled to the sum of its z-values? Only works if \code{width} and \code{height} are vectors with a length of 1.
}
  \item{col}{
a vector (same length as the number of columns in \code{z}) of colors to be used in filling the rectangles
}
  \item{col.frame}{
the colour of the frame of the 2D barplot
}
  \item{lwd.frame}{
the line width of the frame of the 2D barplot
}
  \item{silent}{
logical, should a progress message be displayed in the console? Defaults to \code{FALSE}.
}
  \item{\dots}{
arguments to be passed to \code{\link{barplot2D}}, particlarly \code{border}, which determines the colour of the borders of the rectangles (as opposed to the frame around each 2D barplot which is controlled by \code{col.frame}. Other relevant arguments include \code{density} and \code{lwd}.
}
}
\details{
The algorithm that determines the location of each rectangle within the 2D-barplot is as follows: 1) Start with a rectangle representing the highest value of \code{z}. 2) Try to put the first rectangle on the left. 3) If it too elongated, try to put two rectangles, on top of each other, on the left. 4) When you have placed those rectangles, proceed with the remaining rectangles.

More precisely, we choose the number of rectables to stack so as to minimize the following penalty: penalty for the first rectangle in the stack + penalty for the last where the penalty of a rectangle is: \code{ratio - 1.1}. where \code{ratio} is the ratio of the longer side by the smaller.
}
\references{
This function was adapted from \url{http://zoonek2.free.fr/UNIX/48_R/03.html}
}
\author{
Adapted by Hans Gerritsen
}
\examples{
data(landings)
data(coast)
xlim <- c(-15,0)
ylim <- c(50,56)
xyz <- make.xyz(landings$Lon,landings$Lat,landings$LiveWeight,landings$Species)
col <- rainbow(5)
basemap(xlim, ylim, main = 'Species composition of gadoid landings')
draw.shape(coast, col='cornsilk')
draw.barplot2D(xyz$x, xyz$y, xyz$z, width = 0.8, height = 0.4, col=col)
legend('topright', colnames(xyz$z), fill=col, bg='lightblue', inset=0.02)  
 
basemap(xlim, ylim, main = 'Species composition of gadoid landings')
draw.shape(coast, col='cornsilk')
draw.barplot2D(xyz$x, xyz$y, xyz$z, width = 1, height = 0.5, scale=TRUE, col=col)
legend('topright', colnames(xyz$z), fill=col, bg='lightblue', inset=0.02)
}
\keyword{aplot}
\keyword{graphs}