\name{mapplots-package}
\alias{mapplots-package}
\alias{mapplots}
\docType{package}
\title{
Data visualisation on maps
}
\description{
Create simple maps; add sub-plots like pie plots to a map or any other plot; format, plot and export gridded data. The package was developed for displaying fisheries data but most functions can be used for more generic data visualisation.
For a complete list of functions with individual help pages, use \code{library(help="mapplots")}.
}
\details{
\tabular{ll}{
Package: \tab mapplots\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-05-25\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
The starting point is generally the function \code{\link{basemap}} which creates a blank map (although most mapplots functions could be applied to any plot, not just maps). Coastlines or other features can be added to the map with the function \code{\link{draw.shape}}. ICES rectangles can also be displayed on the map and axes by \code{\link{draw.rect}}. The main purpose of this package is to visualise data on maps. For univariate data, the main functions are \code{\link{draw.bubble}} (bubble plots) and \code{\link{draw.grid}} (heat maps). Multivariate data can be displayed with \code{\link{draw.barplot2D}} ('square pie plots'), \code{\link{draw.pie}} (pie plots) and \code{\link{draw.xy}} (xy or barplots). Some of these have a specific function for displaying a legend: \code{\link{legend.bubble}}, \code{\link{legend.grid}} and \code{\link{legend.pie}}. The following functions can help to get data in the right format for plotting: \code{\link{make.grid}} and \code{\link{make.multigrid}} (to create grd objects for \code{\link{draw.grid}}) and \code{\link{make.xyz}} (to create xyz objects for \code{\link{draw.barplot2D}} and \code{\link{draw.pie}}). Finally, there are some functions to export grd objects: \code{\link{geoTiff}}, \code{\link{geoTiffRgb}} and \code{\link{write.grid}}. The remaining functions are called by the main funcions listed above and were never intended to be used directly. However, they are documented and can be called directly.
}
\author{
Hans Gerritsen

Maintainer: <hans.gerritsen@marine.ie>
}
\keyword{ package }
