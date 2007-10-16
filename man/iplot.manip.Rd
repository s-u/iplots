\name{iplot.manip}
\alias{iplot.backend}
\alias{iplot.resetZoom}
\alias{iplot.rotate}
\alias{iplot.setExtendedQuery}
\alias{iplot.zoomIn}
\alias{iplot.zoomOut}
\alias{print.iobj}
\alias{print.iplot}
\alias{iplot.size}
\alias{iplot.location}
\title{
  iPlot manipulation functions
}
\description{
  The following functions are used to mainpulate iplots. They are NOT
  part of the official API and may disappear without warning. Most of
  them are legacy functions introduced before \code{iplot.opt} was
  available.

  \code{iplot.backend} retrieves or sets the iPlots back-end. 
  
  \code{iplot.resetZoom} reset zoom
  
  \code{iplot.rotate} set plot rotation
  
  \code{iplot.setExtendedQuery} set text for extended query
  
  \code{iplot.zoomIn} zoom into specified area
  
  \code{iplot.zoomOut} zoom out (the coordinates are ignored as the zoom
  is heararchical)

  \code{iplot.location} get or set the location of the iplot

  \code{iplot.size} get or set the size of an iplot plot
}
\usage{
iplot.backend(type = NULL)
iplot.resetZoom() 
iplot.rotate(i) 
iplot.setExtendedQuery(str, plotID=.iplot.curid) 
iplot.zoomIn(x1, y1, x2, y2) 
iplot.zoomOut(x, y)
iplot.location(x, y, relative=FALSE, plot=iplot.cur())
iplot.size(width, height, plot=iplot.cur())
print.iobj(x, ...) 
print.iplot(x, ...) 
}
\arguments{
  \item{i}{rotation orientation}
  \item{plotID}{plot ID (number)}
  \item{str}{string to show on extended query or
    \code{FALSE} or \code{NULL} to disable extended query}
  \item{type}{back-end type - one of \code{"awt"}, \code{"swing"} or
    \code{"opengl"} (or any unambiguous first part hereof) to set the
    type or \code{NULL} to retrieve the current back-end type}
  \item{x1}{basis coordinate for the \code{x} axis}
  \item{y1}{basis coordinate for the \code{y} axis}
  \item{x2}{edge coordinate for the \code{x} axis}
  \item{y2}{edge coordinate for the \code{y} axis}
  \item{x}{object to print or x-coordinate}
  \item{y}{y-coordinate}
  \item{plot}{plot to query or move (id or object)}
  \item{relative}{can be \code{TRUE}, \code{FALSE} or another plot}
  \item{width}{width of the plot (in pixels)}
  \item{height}{height of the plot (in pixels)}
  \item{...}{additional parameters}
}
\details{
  \code{iplot.location}, \code{iplot.size}: those functions either query
  or set the location or size of the plot. If either of the coordinates
  is missing, the size/location in that coordinate will not be changed.
  If both coordinates are missing, the functions have no side effect and
  just return the current size (\code{iplot.size}) or location and size
  (\code{iplot.location}) as named vectors (\code{x}, \code{y} for
  location and \code{width}, \code{height} for size).

  \code{iplot.backend} determines the back-end used by iplots. The
  choices are \code{awt}, \code{swing} an d\code{opengl}. Not all
  back-ends are available on all platforms. \code{AWT} is the most
  compatible back-end and available on all platforms, \code{Swing} has
  more modern look and feel and \code{OpenGL} sacrifices text rendering
  quality for speed (i.e. OpenGL backend is best for large data). When
  no backend is specified, the function has no side effect and just
  returns the currently requested back-end. Currently there is no
  indication whether the back-end request was honored or not. If a
  backend is not supported, the request is silently ignored.
}
\keyword{hplot}
