\name{ilines}
\alias{ilines}
\title{
Adds a connected lines or polygon to the current iPlot.
}
\description{
A generic function taking coordinates of points in data space and
creating corresponding connected lines or polygon in the current iPlot.
}
\usage{
ilines(x,y,col="black",fill=NULL,layer=-1)
}
\arguments{
\item{x, y}{Coordinate vectors of points to join.}
\item{col}{Drawing color of the lines. Currently only "#rrggbb"
  notation and named colors are supported.}
\item{fill}{Color of the polygon area or \code{NULL} if no filling
  should be performed.}
\item{layer}{Layer to use for drawing. -1 denotes the topmost layer.}
}
\value{
Resulting iobj.
}
\seealso{
\code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
l<-lowess(Sepal.Width,Petal.Width)
ilines(l$x,l$y)
}
\keyword{interactive scatter plot}
