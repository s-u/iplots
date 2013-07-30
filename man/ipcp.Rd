\name{ipcp}
\alias{ipcp}
\title{
  Interactive Parallel Coordinates Plot
}
\description{
  This function creates a new interactive parallel coordinates plot from the given data.
}
\usage{
ipcp(...)
}
\arguments{
  \item{...}{All unnamed parameters are treated as variables to
    display. All additional parameters are passed to
    \code{\link{iplot.opt}}.
  }
}
\value{
  Resulting plot object.
}
\details{
  Creates an interactive parallel coordinates plot.

  The plot can be used either in the form \code{ipcp(df)} to plot
  a list or data frame of variables contained in \code{df} or
  \code{ipcp(x, y, z)} to plot variables \code{x}, \code{y} and
  \code{z}.
  
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
