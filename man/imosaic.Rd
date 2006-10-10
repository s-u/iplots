\name{imosaic}
\alias{imosaic}
\title{
  Interactive Mosaic Plot
}
\description{
  This function creates a new interactive mosaic plot from the given data.
}
\synopsis{
imosaic(...)
}
\usage{
imosaic(vars, ..., type="observed")
}
\arguments{
  \item{vars}{list of variables/factors or a single variable/factor}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. All unnamed parameters are treated as
variables to display. 
  }
  \item{type}{selects the variation of the mosaic plot. Valid values are
    "observed", "expected", "fluctuation", "same.bin.size" and
    "multiple.barchart". Partial matching is used, so first letter is
    sufficient.}
}
\value{
  Resulting plot object.
}
\details{
  Additional parameters:
  \describe{
    \item{rotateYLabelsBy}{The default value of this variable is changed
      here. For a description see \code{\link{iplot.opt}}.}
  }
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
library(MASS)
data(Cars93)
attach(Cars93)
imosaic(AirBags,Cylinders,Origin)
imosaic(AirBags,Cylinders,Origin,type="mul")
}
\keyword{hplot}
