\name{imap}
\alias{imap}
\title{
  Interactive Map
}
\description{
  This function creates a new interactive map from the given data.
}
\usage{
imap(x, y=NULL, ...)
}
\arguments{
  \item{x}{either an object of the class \code{"map"} as created by the
    \code{map} function or a vector of the x-coordinates of the map
    polygons}
  \item{y}{y-coordinates of the map polygons}
  \item{...}{All additional parameters are passed to
    \code{\link{iplot.opt}}.}
}
\value{
  Resulting plot object.
}
\details{
  Creates an interactive map plot.

  The input can be either an object of the class \code{"map"} passed
  directly to the \code{x} parameter or two vectors of matching
  coordinates passed to \code{x} and \code{y}. The format for polygons
  in the same as used by the \code{map} function.

  Each polygon should correspond to a case and it will be linked
  correspondingly. See \code{\link{ivar.new.map}} for details on map
  variables.

  Note: this function is currently experimental and it may change in the
  future.
  
%  Additional parameters (also accessible via \code{\link{iplot.opt}}):
%  \describe{
%    \item{\code{drawTicks}}{Whether ticks should be drawn.}
%    \item{\code{isSpine}}{Whether a spineplot should be created instead of a normal barchart.}
%    \item{\code{borderColorSel}}{Here this color defaults to black. See
%      \code{\link{iplot.opt}} for details.}
%  }
}
\seealso{
  \code{\link{iplot}}, \code{\link{ivar.new.map}}
}
\examples{
library(maps)
m <- map('state', plot=FALSE)
imap(m)
}
\keyword{hplot}
