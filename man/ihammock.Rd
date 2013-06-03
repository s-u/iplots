\name{ihammock}
\alias{ihammock}
\title{
  Interactive Hammock Plot
}
\description{
  This function creates a new interactive hammock plot. Please note that
  a hammock plot was not necessarily designed to support highlighting, so
  it may be of limited use. It was created as a proof of the iBase
  concept which makes it very easy to implement new interactive plots.
}
\usage{
  ihammock(vars, ...)
}
\arguments{
  \item{vars}{list of factors to display or the first variable if
    multiple variables are passed as arguments}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. All unnamed parameters are assumed to be
    data to plot.
  }
}
\details{
  The plot can be used in either in the form \code{ihammock(df)} to plot
  a list or data frame of variables or \code{ihammock(x, y, z)} to plot
  factors \code{x}, \code{y} and \code{z}.
}
\value{
  Resulting plot object.
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot}},
  \code{\link{iplot.opt}}
}
\examples{
library(MASS)
data(Cars93)
attach(Cars93)
ihammock(AirBags, Cylinders, Origin)
}
\keyword{hplot}
