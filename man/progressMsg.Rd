\name{progressMsg}
\alias{progressMsg}
\alias{setProgressMsg}
\title{
Progress message
}
\description{
progress message in the R console.
}
\usage{
setProgressMsg(min = 0, max = 1)
progressMsg(pm, value, round = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min, max}{
(finite) numeric values for the extremes of the progress message. Must have \code{min < max}
}
  \item{pm}{
list created by setProgressMsg
}
  \item{value}{
value for the progress message.
}
  \item{round}{
integer indicating the number of decimal places for the percentage completed. Defaults to 0.
}
}
\details{
\code{setProgressMsg} sets up a list with variables used and updated by \code{progressMsg}
}
\value{
a list
}
\author{
Hans Gerritsen
}
\seealso{
See also \code{\link[utils]{txtProgressBar}}
}
\examples{
pm <- setProgressMsg(0,500)
for(i in 1:500) {
  Sys.sleep(0.01)
  pm<- progressMsg(pm,i)
}
rm(pm)
}
