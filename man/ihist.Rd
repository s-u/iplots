\name{ihist}
\alias{ihist}
\title{
  Interactive Histogram
}
\description{
  This function creates a new interactive histogram from the given data.
}
\usage{
ihist(var, ...)
}
\arguments{
  \item{var}{Vector of numbers or variable to use.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}.}
}
\value{
  Resulting plot object.
}
\seealso{
\code{\link{iplot}}, \code{\link{ibar}}, \code{\link{iplot.list}}
}, \code{\link{iplot.opt}}
\examples{
data(iris)
attach(iris)
ihist(Sepal.Width)
}
\keyword{hplot}
