\name{itext}
\alias{itext}
\title{
  Add text to the current iPlot.
}
\description{
  \code{\link{text}} adds the strings given in the vector \code{labels} at the
  coordinates given by \code{x} and \code{y} to the current iPlot. \code{y} may be missing since
  \code{xy.coords(x,y)} is used for construction of the coordinates.
}
\usage{
itext(x, y=NULL, labels=seq(along=x), ax=NULL, ay=NULL, ..., plot = iplot.cur())
}
\arguments{
\item{x, y}{Coordinate vectors of the text. \code{\link{xy.coords}} is
  used to obtain the coordinates.}
\item{labels}{one or more character strings or expressions specifying the
  text to be written.}
\item{ax, ay}{anchor coordinates of the text. They specify where is the
  point specified by \code{x} and \code{y} relative to the text. 0
  means left resp. top and 1 means right/bottom. Therefore vertically
  and horizontally centered text can be obtained by using \code{ax=0.5}
  and \code{ay=0.5}.}
\item{...}{any additional options are passed to \code{\link{iobj.opt}}}
\item{plot}{parent plot}
}
\value{
  Resulting iObject.
}
\seealso{
  \code{\link{ilines}}, \code{\link{iobj.opt}}
}
\keyword{iplot}
