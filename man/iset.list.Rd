\name{iset.list}
\alias{iset.list}
\alias{iset.new}
\alias{iset.set}
\alias{iset.next}
\alias{iset.prev}
\alias{iset.cur}
\alias{print.iset}
\alias{show,iset-method}
\title{
iSet management functions.
}
\description{
  These functions are used to manage iSets. An iSets groups all iPlots
  which use the same data as a basis. All iPlots belonging to an iSet
  are linked, that is selecting cases in one iPlot cause the same cases
  to be highlighted in all other iPlots based on the same iSet. The
  linking is done on case-level, therefore the variables should have the
  same number of cases.

  You can get the iset object by calling the \code{iset} function. An
  iset object can be used in a way similar to data frames, so subsetting
  and other operations work as expected. Subassignment is always done
  "live", so changeing parts of the data will cause plots to be updated
  accordingly.
  
  There is always exactly one current iSet. An initial iSet called
  "default" is created when the iplots library is loaded. All new
  iPlots are created using the current iSet. The set of functions
  described here allow the manipulation of iSets.
  
  Note that changing the iSet also has an effect on
  \code{\link{iplot.list}} and \code{\link{iobj.list}} since iPlots
  are linked to their iSets.
  
  \code{\link{iset.new}} creates a new iSet and makes it current
  
  \code{\link{iset.list}} returns all iSets
    
  \code{\link{iset.cur}} returns the ID of the current iSet
  
  \code{\link{iset.next}} and \code{\link{iset.prev}} return the ID
  of the next resp. previous iSet in the list relative to the iSet
  specified by the argument.
  
  \code{\link{iset.set}} makes the iSet with the specified ID current.

  \code{print.iset} prints an iSet object.
}
\usage{
iset.new(name=NULL, payload=NULL)
iset.list()
iset.cur()
iset.next(which=iset.cur())
iset.prev(which=iset.cur())
iset.set(which=iset.next())
\method{print}{iset}(x, ...)
}
\arguments{
  \item{name}{Name of the new iSet. If no name is specified, an
    automatic name of the form "data.X" is created where "X" is an
    integer. The name can be used instead on an ID in many cases.}
  \item{payload}{Initial contents of the iSet in the form of a
    list/data.frame or \code{NULL} if an empty iSet is desired.}
  \item{which}{An integer specifying an iSet ID or a string specifying a
    name of the iSet.}
  \item{x}{An iSet object to print}
  \item{...}{further parameters to be passed to \code{print}}
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "iset")}: is mapped to
      \code{print.iset}}
  }
}
\seealso{
  \code{\link{iset}}, \code{\link{iplot.list}}, \code{\link{iobj.list}}
}
\keyword{iplot}
