\name{iplot}
\alias{iplot}
\title{
Interactive Scatterplot
}
\description{
This function creates a new interactive scatterplot from the given data.
}
\usage{
iplot(x,y,col=NULL,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)))
}
\arguments{
\item{x}{Data for the X axis. It can be either a vector of values or a variable of an iset.}
\item{y}{Data for the y axis. It can be either a vector of values or a variable of an iset.}
\item{col}{(alpha:unused) Vector of colors used for brushing. The contents are used in circular fashion.}
\item{xlim}{(alpha:unused) Range of the X axis.}
\item{ylim}{(alpha:unuseD) Range of the Y axis.}
}
\value{
Resulting plot object.
}
\seealso{
\code{\link{ihist}}, \code{\link{ibar}}, \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
iplot(Sepal.Width/Sepal.Length, Species)
}
\keyword{interactive scatter plot}
