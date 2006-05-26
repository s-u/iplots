\name{iplot}
\alias{iplot}
\title{
  Interactive Scatterplot
}
\description{
  This function creates a new interactive scatterplot from the given data.
}
\usage{
iplot(x, y=NULL, xlab=NULL, ylab=NULL, changePtDiamBy=2, customFieldBg=FALSE, COL_CUSTOMBG="white", minimalDiam=1, ptDiam=3, spaceprop=1.1, ...)
}
\arguments{
  \item{x}{Data for the x axis. It can be either a vector of values or a variable of an iset. If y is not given, this must be a list of one of these.}
  \item{y}{Data for the y axis. It can be either a vector of values or a variable of an iset.}
  \item{xlab}{Name for x variable.}
  \item{ylab}{Name for y variable.}
  \item{changePtDiamBy}{Number of pixels the point diameter should be changed when in-/decreasing it via keyboard or menu.}
  \item{customFieldBg}{Whether to use a custom background color.}
  \item{COL_CUSTOMBG}{The custom background color.}
  \item{minimalDiam}{Minimal point diameter.}
  \item{ptDiam}{Point diameter.}
  \item{spaceprop}{Gives the amount of space around the data points. 1.0 means no space, 1.5 means half as much space around the data as is used for the data itself. This resets zoom.}
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
