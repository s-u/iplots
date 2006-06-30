\name{iplot.manip}
\alias{iplot.backend}
\alias{iplot.new}
\alias{iplot.resetAxes}
\alias{iplot.resetXaxis}
\alias{iplot.resetYaxis}
\alias{iplot.resetZoom}
\alias{iplot.rotate}
\alias{iplot.setExtendedQuery}
\alias{iplot.showVars}
\alias{iplot.zoomIn}
\alias{iplot.zoomOut}
\alias{print.iobj}
\alias{print.iplot}
\title{
  iPlot manipulation functions
}
\description{
  The following functions are used to mainpulate iplots. They are NOT
  part of the official API and may disappear without warning.

  \code{iplot.backend} retrieves or sets the iPlots back-end. 
  
  \code{iplot.resetAxes} reset all axes to their defaults
  
  \code{iplot.resetXaxis} reset major X axis to its default
  
  \code{iplot.resetYaxis} reset major Y axis to its default
  
  \code{iplot.resetZoom} reset zoom
  
  \code{iplot.rotate} set plot rotation
  
  \code{iplot.setExtendedQuery} set text for extended query
  
  \code{iplot.showVars} show window with current variable (unimplemented
  unless KLIMT plugin is loaded)
  
  \code{iplot.zoomIn} zoom into specified area
  
  \code{iplot.zoomOut} zoom out (the coordinates are ignored as the zoom
  is heararchical)
}
\usage{
iplot.backend(type = NULL)
iplot.resetAxes(ipl = lastPlot) 
iplot.resetXaxis(ipl = lastPlot) 
iplot.resetYaxis(ipl = lastPlot) 
iplot.resetZoom() 
iplot.rotate(i) 
iplot.setExtendedQuery(str) 
iplot.showVars() 
iplot.zoomIn(x1, y1, x2, y2) 
iplot.zoomOut(x, y) 
print.iobj(x, ...) 
print.iplot(x, ...) 
}
\arguments{
  \item{ipl}{plot to operate on}
  \item{i}{rotation orientation}
  \item{str}{string to show on extended query}
  \item{type}{back-end type - one of \code{"awt"}, \code{"swing"} or
    \code{"opengl"} (or any unambiguous first part hereof) to set the
    type or \code{NULL} to retrieve the current back-end type}
  \item{x1}{basis coordinate for the \code{x} axis}
  \item{y1}{basis coordinate for the \code{y} axis}
  \item{x2}{edge coordinate for the \code{x} axis}
  \item{y2}{edge coordinate for the \code{y} axis}
  \item{x}{object to print}
  \item{y}{unused}
  \item{...}{additional parameters}
}
\keyword{hplot}
