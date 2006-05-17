\name{imosaic}
\alias{imosaic}
\title{
  Interactive Mosaic Plot
}
\description{
  This function creates a new interactive mosaic plot from the given data.
}
\usage{
imosaic(x, ...)
}
\arguments{
  \item{x}{Data frame containing the variables.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. Many of the parameters used in
    \code{\link{plot}} are supported.
  }
}
\value{
  Resulting plot object.
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
library(MASS)
data(Cars93)
attach(Cars93)
imosaic(data.frame(AirBags,Cylinders,Origin))
}
\keyword{hplot}
