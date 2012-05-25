\name{legend.box}
\alias{legend.box}
\title{
Define location of a legend box.
}
\description{
Define a location of a legend box. Used by \code{\link{legend.bubble}} and \code{\link{legend.pie}}. This is not intended as a user function.
}
\usage{
legend.box(x, y = NULL, maxradius, mab = 1.2, inset = 0, double = F)
}
\arguments{
  \item{x, y}{
the x and y co-ordinates to be used to position the legend, see \code{\link{legend}}.
}
  \item{maxradius}{
the (maximum) radius of the pie or bubble to be represented in the legend.
}
  \item{mab}{
the margin around the bubble or pie.
}
  \item{inset}{
inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword.
}
  \item{double}{
logical, should the box be double the 'normal' size to allow for pies and bubbles to be displayed in a single legend box. generally not very pretty.
}
}
\value{
Returns the corner points of the legend box in user coordinates 
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{legend.bubble}}, \code{\link{legend.pie}}
}
\examples{
plot(1)
box <- legend.box('topleft', maxradius=0.2, inset=0.02)
rect(box[1],box[2],box[3],box[4], border='red', lwd=3, lty=2)
legend.bubble('topleft', z=10, maxradius=0.2, inset=0.02)
}
