\name{ilines}
\alias{ilines}
\title{
  Add connected lines or polygon to the current iPlot.
}
\description{
  A generic function taking coordinates of points in data space and
  creating corresponding connected lines or polygon in the current iPlot.
}
\usage{
ilines(x, y, col=NULL, fill=NULL, visible=NULL, plot = iplot.cur())
}
\arguments{
\item{x, y}{Coordinate vectors of points to
  join. \code{\link{xy.coords}} is used to obtain the coordinates for plotting.}\item{col}{Drawing color of the lines. Currently only "#rrggbb"
  notation and named colors are supported. \code{NULL} means that color
  of the object is not explicitely specified.}
\item{fill}{Color of the polygon area or \code{NA} if no filling
  should be performed.}
\item{visible}{If set to \code{FALSE} the lines/polygon won't be
  visible initially.}
\item{plot}{parent plot for the lines}
}
\value{
  Resulting iObject.
}
\details{
  The point vectors \code{x} and \code{y} can contain \code{NA} values,
  in which case each sequence of points separated by \code{NA}s will be
  treated as a separate polygon. \code{NA}s must be present in both
  coordinates at the same index, otherwise the behavior is undefined.
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iobj.opt}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
l<-lowess(Sepal.Width,Petal.Width)
ilines(l)
}
\keyword{hplot}
