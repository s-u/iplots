\name{iplot.data}
\alias{iplot.data}
\title{
  Retireve data from a plot.
}
\description{
  This function retrieves the associated data from the current plot.
}
\usage{
iplot.data(id=NULL)
}
\arguments{
  \item{id}{number of the variable to retrieve. If omitted a list of
    all associated varaibles (and their contents) is returned.}
}
\value{
  A vector representaing a variable or a list of variable contents. The
  first two vecctors in the list are traditionally named \code{x} and
  \code{y}. The number of variables depends on the plot used,
  e.g. scatter plot has two, histogram or bar chart return one.
}
\seealso{
  \code{\link{iplot}}, \code{\link{ihist}}, \code{\link{ibar}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
iplot(Sepal.Width/Sepal.Length, Species)
}
\keyword{iplot}
