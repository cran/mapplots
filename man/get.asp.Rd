\name{get.asp}
\alias{get.asp}
\title{
Get the current aspect ratio of a plot
}
\description{
This function is used by \code{\link{draw.pie}} to draw circles (rather than ellipses) in existing plots.
}
\usage{
get.asp()
}
\value{
Returns the aspect ratio of the current plot.
}
\author{
Hans Gerritsen
}
\note{
If the plot is re-sized \code{get.asp} is \strong{not} automatically updated
}
\seealso{
\code{\link{draw.pie}} 
}
\examples{
plot(1:10,(1:10)/10)
get.asp()
}
