\name{progressMsg}
\alias{progressMsg}
\title{
Progress message
}
\description{
progress message in the R console.
}
\usage{
progressMsg(value, min = 0, max = 1, round = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{
value for the progress message.
}
  \item{min, max}{
(finite) numeric values for the extremes of the progress message. Must have \code{min < max}
}
  \item{round}{
integer indicating the number of decimal places for the percentage completed. Defaults to 0.
}
}
\details{
It is important to start with \code{value} that is exactly the same as \code{min}. If not, it is possible that the start time is not re-set, resuling in incorrect estimates of the remaining time.
}
\value{
two varariables are created or updated: \code{.t0} () the start time for the loop and \code{.pc} (the percentage completed).
}
\author{
Hans Gerritsen
}
\seealso{
See Also as \code{\link[utils]{txtProgressBar}}
}
\examples{
for(i in 1:500) {
  Sys.sleep(0.01)
  progressMsg(i,1,500)
  }
}
