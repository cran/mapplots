\name{breaks.grid}
\alias{breaks.grid}
\title{
Define breakpoints for colour scales
}
\description{
This function can be used to define breakpoints for use with the functions \code{\link{draw.grid}} and \code{\link{legend.grid}}
}
\usage{
breaks.grid(grd, quantile = 0.955, ncol = 12, zero = TRUE)
}
\arguments{
  \item{grd}{
an array produced by \code{\link{make.grid}} or a list produced by \code{\link{make.multigrid}} or a vector of positive values.
}
  \item{quantile}{
the maximum value of the breaks will be determined by the quantile given here. This can be used to deal with outlying values in \code{grd}. If \code{quantile = 1} then the maximum value of the breaks will be the same as the maximum value in \code{grd}.
}
  \item{ncol}{
number of colours to be used, always one more than the number of breakpoints. Defaults to 12.
}
  \item{zero}{
logical, should zero be included as a separate category? Defaults to \code{TRUE}.
}
}
\value{
a vector of breakpoints to be used by \code{\link{draw.grid}} and \code{\link{legend.grid}}
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{draw.grid}}, \code{\link{legend.grid}}
}
\examples{
breaks.grid(100,ncol=6)
breaks.grid(100,ncol=5,zero=FALSE)

# create breaks on the log scale
exp(breaks.grid(log(10000),ncol=4,zero=FALSE))
}
