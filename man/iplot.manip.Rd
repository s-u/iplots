\name{iplot.manip}
\alias{iplot.backend}
\alias{iplot.resetZoom}
\alias{iplot.rotate}
\alias{iplot.setExtendedQuery}
\alias{iplot.zoomIn}
\alias{iplot.zoomOut}
\alias{print.iobj}
\alias{print.iplot}
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
}
\usage{
iplot.backend(type = NULL)
iplot.resetZoom() 
iplot.rotate(i) 
iplot.setExtendedQuery(str, plotID=.iplot.curid) 
iplot.zoomIn(x1, y1, x2, y2) 
iplot.zoomOut(x, y)
iplot.location(x, y, relative=FALSE, plot=iplot.cur())
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
  \item{...}{additional parameters}
}
\keyword{hplot}
