\name{ihist}
\alias{ihist}
\title{
Interactive Histogram
}
\description{
This function vreates a new interactive histogram from the given data.
}
\usage{
ihist(var,\ldots)
}
\arguments{
\item{var}{Vector of number or variable to use.}
}
\value{
Resulting plot object.
}
\seealso{
\code{\link{iplot}}, \code{\link{ibar}}, \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
ihist(Sepal.Width)
}
\keyword{interactive histogram}
