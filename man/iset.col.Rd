\name{iset.col}
\alias{iset.col}
\alias{iset.brush}
\title{
  Set color of cases in an iSet.
}
\description{
  This function sets the colors of cases of an iSet. Every iPlot is
  free to use the corresponding representation individually. Color of a
  case is a iSet-global property, that is all iPlots associated with an
  iSet will reflect the change.
}
\usage{
iset.col(col)
}
\arguments{
  \item{col}{Vector of integers specifying colors or a factor. The
    values are re-cycled if necessary. 0 has a special meaning denoting
    "no color", i.e. the plot is free to use the default color. This is
    also the default state of a newly created iSet.}
}
\seealso{
  \code{\link{iset.new}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
ibar(Species)
iset.col(Species)
}
\keyword{iplot}
