\name{iplot}
\alias{iplot}
\title{
  Interactive Scatterplot
}
\description{
  This function creates a new interactive scatterplot from the given data.
}
\usage{
iplot(x, y, ...)
}
\arguments{
  \item{x}{Data for the X axis. It can be either a vector of values or a variable of an iset.}
  \item{y}{Data for the y axis. It can be either a vector of values or a variable of an iset.}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}. Many of the parameters used in
    \code{\link{plot}} are supported.
  }
}
\value{
  Resulting plot object.
}
\seealso{
  \code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
iplot(Sepal.Width/Sepal.Length, Species)
}
\keyword{hplot}
