\name{iobj.list}
\alias{iobj.list}
\alias{iobj.set}
\alias{iobj.next}
\alias{iobj.prev}
\alias{iobj.cur}
\alias{iobj.get}
\alias{iobj.rm}
\alias{==.iobj}
\alias{!=.iobj}
\title{
Interactive objects (iObjects) management functions.
}
\description{
  These functions are used to manage iObjects of an iPlot. Exactly one
  of the iObjects is the current one (for each iPlot). Every newly created iObject
  automatically becomes current.
  
  \code{\link{iobj.list}} returns all iObjects of the current iPlot.
    
  \code{\link{iobj.cur}} returns the current iObject.
  
  \code{\link{iobj.next}} and \code{\link{iobj.prev}} return the ID
  of the next resp. previous object in the list relative to the object
  specified by the argument.
  
  \code{\link{iobj.set}} makes the object with the specified ID current.
  
  \code{\link{iobj.get}} returns the object specified by its ID.
  
  \code{\link{iobj.rm}} removes the object

  \code{==} and \code{!=} operators can be used to compare two iObjects
}
\usage{
iobj.list()
iobj.cur()
iobj.next(which=iobj.cur())
iobj.prev(which=iobj.cur())
iobj.set(which=iobj.next())
iobj.get(pos, plot=iplot.cur())
iobj.rm(which=iobj.cur())
`==.iobj`(a,b)
`!=.iobj`(a,b)
}
\arguments{
  \item{which}{An object or an integer specifying the object number. }
  \item{pos}{ID of an object}
  \item{plot}{plot whose object is to be retrieved.}
  \item{a}{object to compare}
  \item{b}{object to compare}
}
\seealso{
  \code{\link{ilines}}, \code{\link{iabline}}
}
\keyword{iplot}
