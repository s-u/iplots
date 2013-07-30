\name{iabline}
\alias{iabline}
\title{
  Add a straight line to the current iPlot.
}
\description{
  This function adds one straight line through the current iPlot.
}
\usage{
iabline(a = NULL, b = NULL, reg = NULL, coef = NULL, ..., plot = iplot.cur())
}
\arguments{
  \item{a, b}{the intercept and slope}
  \item{coef}{a vector of length two giving the intercept and slope}
  \item{reg}{an object with a \code{coef} component}
  \item{...}{object options for \code{\link{iobj.opt}}}
  \item{plot}{plot to which this line is to be added}
}
\value{
  Resulting iObject.
}
\details{
  Typical usages are
  \preformatted{  iabline(a, b, ...)
  iabline(coef=, ...)
  iabline(reg=, ...)
}
}
\seealso{
  \code{\link{ilines}}, \code{\link{iobj.opt}}
}
\keyword{hplot}
