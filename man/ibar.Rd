\name{ibar}
\alias{ibar}
\title{
  Interactive Bar Chart
}
\description{
  This function creates a new interactive bar chart from the given data.
}
\usage{
ibar(var, ...)
}
\arguments{
  \item{var}{Factor, vector of strings or variable to use.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}.}
}
\value{
  Resulting plot object.
}
\details{
  Creates an interactive barchart or spline plot.

  Additional parameters (also accessible via \code{\link{iplot.opt}}):
  \describe{
    \item{\code{drawTicks}}{Whether ticks should be drawn.}
    \item{\code{isSpine}}{Whether a spineplot should be created instead of a normal barchart.}
    \item{\code{borderColorSel}}{Here this color defaults to black. See
      \code{\link{iplot.opt}} for details.}
  }
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
