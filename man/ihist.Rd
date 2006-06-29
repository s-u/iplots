\name{ihist}
\alias{ihist}
\title{
  Interactive Histogram
}
\description{
  This function creates a new interactive histogram from the given data.
}
\usage{
  ihist(var, ...)
}
\arguments{
  \item{var}{Vector of numbers or variable to use.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}.}
}
\details{
  Creates an interactive histogram.

  Additional parameters (also available via \code{\link{iplot.opt}}):
  \describe{
    \item{anchor}{Anchor point for the histogram.}
    \item{binw}{Bin width.}
    \item{autoScaleXAxis}{Whether the x axis should be automatically rescaled.}
    \item{autoScaleYAxis}{Whether the y axis should be automatically
      rescaled.}
  }
}
\value{
  Resulting plot object.
}
\seealso{
  \code{\link{iplot}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
data(iris)
ihist(iris$Sepal.Width)
}
\keyword{hplot}
