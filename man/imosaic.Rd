\name{imosaic}
\alias{imosaic}
\title{
  Interactive Mosaic Plot
}
\description{
  This function creates a new interactive mosaic plot from the given data.
}
\usage{
imosaic(..., type="observed")
}
\arguments{
  \item{...}{All unnamed parameters are treated as variables to
    display. Additional parameters are passed to \code{\link{iplot.opt}}. 
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
  The plot can be used either in the form \code{imosaic(df)} to plot
  a list or data frame of variables contained in \code{df} or
  \code{imosaic(x, y, z)} to plot variables \code{x}, \code{y} and
  \code{z}.

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
iplot.location(300,100,TRUE)
}
\keyword{hplot}
