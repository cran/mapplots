\name{legend.grid}
\alias{legend.grid}
\title{
Legend for 
}
\description{
Draw a legend for a \code{\link{draw.grid}} plot
}
\usage{
legend.grid(x, y = NULL, breaks, col ,digits = 2, suffix = "", type = 1, pch = 15,
	pt.cex = 2.5, bg = "lightblue", ...)
}
\arguments{
  \item{x, y}{
the x and y co-ordinates to be used to position the legend. \code{x} can be a keyword (e.g. \code{"topleft"}). See \code{\link{legend}}.
}
  \item{breaks}{
a vector of breakpoints for the colours, must give one more breakpoint than colour.
}
  \item{col}{
a vector with colours. defaults to colors ranging from white (lowest) through yellow and orange to red (highest).
}
  \item{digits}{
integer indicating the number of significant places to be used in the legend. Defaults to 2.
}
  \item{suffix}{
character string to be placed after the legend entries, e.g. \code{"kg"}.
}
  \item{type}{
integer specifying the legend type, \code{1} will result in a single value for each colour (the average value for each colour category). Type \code{2} will result in a range being displayed for each colour category. This is more accurate but can make the legend take up a lot of space.
}
  \item{pch}{
plotting character, i.e., symbol to use. This can either be a single character or an integer code for one of a set of graphics symbols. See \code{\link{points}}. Defaults to 15.
}
  \item{pt.cex}{
character expansion of the symbols. Defaults to 2.5
}
  \item{bg}{
background colour for the legend box
}
  \item{\dots}{
other arguments to be passed to \code{\link{legend}}.
}
}
\author{
Hans Gerritsen
}
\seealso{
\code{\link{draw.grid}} 
}
\examples{
data(coast)
data(landings)
byx = 1
byy = 0.5
xlim <- c(-15.5,0)
ylim <- c(50.25,56)
grd <- make.grid(landings$Lon,landings$Lat,landings$LiveWeight, byx, byy, xlim, ylim)
breaks <- breaks.grid(grd,zero=FALSE)
basemap(xlim, ylim, main = "Gadoid landings")
draw.grid(grd,breaks)
draw.shape(coast, col="darkgreen")
legend.grid("topright", breaks=breaks/1000, type=2, inset=0.02, title="tonnes")
}