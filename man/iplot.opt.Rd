\name{iplot.opt}
\alias{iplot.opt}
\title{
  Modify parameters of an interactive plot
}
\description{
  This function modifies parameters of an iPlot.
}
\usage{
iplot.opt(..., plot=iplot.cur())
}
\arguments{
  \item{xlim}{Range of the X axis (vector of two numbers).}
  \item{ylim}{Range of the Y axis (vector of two numbers).}
  \item{col}{Colors of the points. See \code{\link{iset.brush}} for
    details.}
  \item{...}{Plot specific parameters.}
  \item{plot}{Plot whose parameters are to be modified.}
}
\value{
  If no parameters (except for the plot) are specified, a list of the
  current parameters is returned.
}
\seealso{
  \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
iplot.opt(xlim=c(1.5,5),col=Species)
}
\keyword{hplot}
