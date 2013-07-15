\name{ices.rect}
\alias{ices.rect}
\alias{ices.rect2}
\title{
Convert ICES rectangles from or to geographical coordinates
}
\description{
\code{ices.rect} converts the names of ICES statistical rectangles into geographical coordinates (midpoints).
\code{ices.rect2} converts geographical coordinates into ICES statistical rectangles.
}
\usage{
ices.rect(rectangle)
ices.rect2(lon, lat)
}
\arguments{
  \item{rectangle}{
a characer vector with the names of ices rectangles. Note that the code can cope with rectangle names that have been converted into numbers by helpful microsoft office software, e.g. "36E2" tends to be converted to 3600.
}
  \item{lon, lat}{
a vector with longitude and latitude (not neccesarily the mid-points of rectangles).
}
}
\value{
\code{ices.rect} will return a data frame with the midpoints of the ICES rectangles.
\code{ices.rect2} will return a vector with the names of the ICES rectangles.
}
\references{
ICES statistical rectangles \url{www.ices.dk} are rectangles of 1 degree longitude x 0.5 degrees latitude. They are used for reporting of fishinge effort and landings.
}
\author{
Hans Gerritsen
}
\examples{
# rectangle names to coordinates:
ices.rect(c("36E2","3600","40D8"))

# coordinates to rectangle names:
lon <- rnorm(10,-10,2)
lat <- rnorm(10,53,1)
rect <- ices.rect2(lon,lat)
basemap(xlim=range(lon), ylim=range(lat) )
draw.rect()
points(lon,lat)
text(lon,lat,rect,cex=0.7,pos=3)
}
\keyword{manip}
