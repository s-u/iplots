\name{iabline}
\alias{iabline}
\title{
  Add a straight line to the current iPlot.
}
\description{
  This function adds one straight line through the current iPlot.
}
\usage{
iabline(a, b, ...)
iabline(coef=, ...)
iabline(reg=, ...)
}
\arguments{
  \item{a, b}{the intercept and slope}
  \item{coef}{a vector of length two giving the intercept and slope}
  \item{reg}{an object with a \code{coef} component}
  \item{...}{object options for \code{\link{iobj.opt}}}
}
\value{
  Resulting iObject.
}
\seealso{
  \code{\link{ilines}}, \code{\link{iobj.opt}}
}
\keyword{hplot}

