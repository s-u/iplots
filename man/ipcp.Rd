\name{ipcp}
\alias{ipcp}
\title{
  Interactive Parallel Coordinates Plot
}
\description{
  This function creates a new interactive parallel coordinates plot from the given data.
}
\synopsis{
ipcp(...)
}
\usage{
ipcp(vars, ...)
}
\arguments{
  \item{vars}{list of at least two variables}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. All unnamed parameters are treated as
    variables to plot.
  }
}
\value{
  Resulting plot object.
}
\details{
  Creates an interactive parallel coordinates plot.

  Additional parameters:
  \describe{
    \item{\code{alterningLabels}}{Whether lables should be placed alternately at the top and at the bottom.}
    \item{\code{COL_AXES}}{Color of the (optional) axes.}
  }
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
