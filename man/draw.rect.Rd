\name{draw.rect}
\alias{draw.rect}
\title{
Draw ICES rectangles in an existing plot
}
\description{
Draw ICES rectangles in an existing map and add axis labels to the top and righ axes.
ICES statistical rectangles (\url{https://www.ices.dk}) are rectangles of 1 degree longitude x 0.5 degrees latitude. They are used for reporting of fishinge effort and landings.
}
\usage{
draw.rect(col = "grey", lty = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{
colour of the lines, defaults to "grey".
}
  \item{lty}{
line type, defaults to 2.
}
  \item{\dots}{
any other arguments to be passed to \code{\link{abline}}.
}
}
\references{
\url{https://www.ices.dk} 
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{basemap}}
}
\examples{
xlim <- c(-15,0)
ylim <- c(50,56)
basemap(xlim, ylim)
draw.rect()
}
