\name{ibar}
\alias{ibar}
\title{
Interactive Bar Chart
}
\description{
This function creates a new bar chart from the given data.
}
\usage{
ibar(var,\ldots)
}
\arguments{
\item{var}{Factor, vector of strings or variable to use.}
}
\value{
Resulting plot object.
}
\seealso{
\code{\link{iplot}}, \code{\link{ihist}}, \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
ibar(Species)
}
\keyword{interactive bar chart}
