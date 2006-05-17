\name{ibar}
\alias{ibar}
\title{
  Interactive Bar Chart
}
\description{
  This function creates a new interactive bar chart from the given data.
}
\usage{
ibar(var, drawTicks=FALSE, isSpine=FALSE, fillColorDrag=rgb(128,128,128,70,max=255), ...)
}
\arguments{
  \item{var}{Factor, vector of strings or variable to use.}
  \item{drawTicks}{Whether ticks should be drawn.}
  \item{isSpine}{Whether a spineplot should be created instead of a normal barchart.}
  \item{fillColorDrag}{Fill color of dragged bars.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}.}
}
\value{
  Resulting plot object.
}
\seealso{
  \code{\link{iplot}}, \code{\link{ihist}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
data(iris)
attach(iris)
ibar(Species)
}
\keyword{hplot}
