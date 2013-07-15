\name{draw.xy}
\alias{draw.xy}
\title{
Draw xy sub-plots in an existing plot
}
\description{
This function is intended to draw xy-plots or barplots in existing maps or other plots.
}
\usage{
draw.xy(x, y, xx, yy, xlim = NULL, ylim = NULL, width = 1, height = 0.5, bg = NULL, 
	border = 1, type = "p", col = 1, silent = TRUE, ...)
}
\arguments{
  \item{x, y}{
vectors with x and y-locations of the centre of the xy-plots.
}
  \item{xx, yy}{
vectors (of the same length as \code{x}) with the x and y-values of the xy-plots
}
\item{xlim, ylim}{
the x and y-limits of the xy-plots. the same limits will apply to all xyplots. Defaults to \code{range(xx)} and \code{range(yy)}.
}
\item{width, height}{
the width and height of the xy-plot area (coordinate units of the main map or plot).
}
  \item{bg}{
background colour of the xy-plots
}
  \item{border}{
the colour of the border of the xy-plots. \code{NA} will result in no border
}
  \item{type}{
the type of plot. See \code{\link{plot.default}} for possible values. Also see Details section below
}
  \item{col}{
a single colour or a vector of colours (same length as \code{x}) for the plotting symbols.
}
  \item{silent}{
logical, should a progress message be displayed in the console? Defaults to \code{FALSE}.
}
  \item{\dots}{
other arguments to be passed to \code{\link{points}}
}
}
\details{
\code{type = "h"} will result in plots that resemble barplots. These are not true barplots as the x-axis is not categorical. However if suitable \code{xx} data are provided and \code{lwd} is tweaked correctly for the current graphical device it will result in adequate plots.
}
\author{
Hans Gerritsen
}
\examples{
data(effort)
data(coast)
xlim <- c(-12,-5)
ylim <- c(51,54)
col <- terrain.colors(12)
effort$col <- col[match(effort$Month,1:12)]
basemap(xlim, ylim, main = "Monthly trends in haddock landings and fishing effort")
draw.rect(lty=1, col=1)
draw.shape(coast, col="cornsilk")
draw.xy(effort$Lon, effort$Lat, effort$Month, effort$LiveWeight, width=1, height=0.5,
 col=effort$col, type="h",lwd=3, border=NA)
draw.xy(effort$Lon, effort$Lat, effort$Month, effort$Effort, width=1, height=0.5, col="red",
 type="l", border=NA)
draw.xy(effort$Lon, effort$Lat, effort$Month, effort$Effort, width=1, height=0.5, col="red",
 type="p",cex=0.4,pch=16, border=NA)
legend("topleft", c(month.abb,"Effort"), pch=c(rep(22,12),16), pt.bg=c(col,NA),
 pt.cex=c(rep(2,12),0.8),col=c(rep(1,12),2), lty=c(rep(NA,12),1), bg="lightblue",
 inset=0.02, title="Landings", cex=0.8)
}
