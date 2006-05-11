\name{ipcp}
\alias{ipcp}
\title{
  Interactive Parallel Coordinates Plot
}
\description{
  This function creates a new interactive parallel coordinates plot from the given data.
}
\usage{
iplot(x, alterningLabels=TRUE, ...)
}
\arguments{
  \item{x}{Data frame containing at least two variables.}
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
ipcp(iris)
}
\keyword{hplot}
