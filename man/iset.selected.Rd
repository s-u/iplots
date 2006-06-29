\name{iset.selected}
\alias{iset.selected}
\alias{iset.select}
\alias{iset.sel.changed}
\alias{iset.selectAll}
\alias{iset.selectNone}
\title{
  Selection / linked highlighting
}
\description{
  These functions modify the selection or return the currently selected
  (highlighted) cases.
  
  \code{\link{iset.selected}} returns a vector of IDs of all currently
  selected cases (in the current iSet)
  
  \code{\link{iset.select}} changes the selection of the current
  iSet. All plots are updated immediately to reflect the new selection.
  
  \code{\link{iset.sel.changed}} returns \code{TRUE} if the selection
  has changed since last call to this function.

  \code{iset.selectAll} selects everything

  \code{iset.selectNone} clears all selections (yes, not a good name)
}
\usage{
iset.selected()
iset.select(what, mode="replace", mark=TRUE)
iset.sel.changed(iset=iset.cur())
iset.selectAll()
iset.selectNone()
}
\arguments{
  \item{what}{specification of the new selection. This can be either a
    vector of case IDs or a logical vector.}
  \item{mode}{mode to be used when combining the previous selection and
    teh surrent one. Supported modes are "replace", "union" and "intersect".}
  \item{mark}{mark to be used.}
  \item{iset}{iSet to query}
}
\value{
  List of IDs of selected cases (\code{iset.selected}), boolean value
  (\code{iset.sel.changed}).
}
\seealso{
  \code{\link{iset.list}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Length,Petal.Length)
iset.select(Species=="virginica")
}
\keyword{iplot}
