\name{ibox}
\alias{ibox}
\title{
  Interactive Box Plot
}
\description{
  This function creates a new interactive box plot from the given data.
}
\usage{
iplot(x, y=NULL, alterningLabels=TRUE, ...)
}
\arguments{
  \item{x}{Vector of numbers or data frame containing the variables.}
  \item{y}{Vector of categories. If present and x represents only one variable, x is split according to the categories.}
  \item{alterningLabels}{Whether lables should be placed alternately at the top and at the bottom.}
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
data(iris)
attach(iris)
ibox(Sepal.Length,Species)
}
\keyword{hplot}
