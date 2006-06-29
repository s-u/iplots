\name{ibox}
\alias{ibox}
\title{
  Interactive Box Plot
}
\description{
  This function creates a new interactive box plot from the given data.
}
\usage{
ibox(x, y=NULL, ...)
}
\arguments{
  \item{x}{Vector of numbers or data frame containing the variables}
  \item{y}{A factor to specify groups for y-by-x boxplot}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. Many of the parameters used in
    \code{\link{plot}} are supported.
  }
}
\value{
  Resulting plot object.
}
\details{
  Creates either a regular boxplot (if \code{y} is not specified) or
  `x-by-y boxplot (if \code{y} is specified). In the latter case both
  \code{x} and \code{y} must be of the same length.
 
  Additional parameters:
  \describe{
    \item{alterningLabels}{Whether lables should be placed alternately
      at the top and at the bottom.}
  }
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
data(iris)
attach(iris)
ibox(Petal.Length)
ibox(Sepal.Length,Species)
}
\keyword{hplot}
