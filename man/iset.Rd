\name{iset}
\alias{iset-class}
\alias{ivar-class}
\alias{iset}
\alias{isets}
\alias{iSet}
\alias{iVar}
\alias{dim.iset}
\alias{length.iset}
\alias{names.iset}
\alias{names.iset<-}
\alias{[<-.iset}
\alias{[[<-.iset}
\alias{[.iset}
\alias{[[.iset}
\alias{$.iset}
\alias{$<-.iset}
\alias{names<-.iset}
\title{
iSet and iVar - managing data in iPlots
}
\description{
  iPlots maintain a separate copy of all data that are displayed in the
  iPlots. This allows iPlots to opearate even after the underlying data
  has been deleted in R. It also allows iPlots to use hot linking in all
  aspects, including update of plots on data changes.
  
  This data management consists mainly of two classes: \code{iset} and
  \code{ivar}. The \code{iset} object encapsuletes an \emph{iSets} which
  can be thought of as a kind of special data frame that allows linking
  of all variables contained therein. Each variable (or column in
  data.frame-speech) is represented by an \code{ivar} object
  encapsulating an \emph{iVar}.

  An instance of a \code{ivar} class can be use to create new iplots or
  update data in the existing iplots. Convenience operators on both
  \code{ivar} and \code{iset} objects include those such as
  \code{length}, subsetting and subassignment and therefor from user's
  point of view it is possible to use them transparently a data frames
  (iSet) or vectors (iVar). In addition, iSet supports methods such as
  \code{names} or \code{dim}.

  \code{iset} returns the object corresponding to a given iSet.

  \code{isets} returs objects for all iSets
}
\usage{
iset(which=iset.cur())
isets()
\method{[[}{iset}(x, i)
\method{$}{iset}(x, name)
\method{$}{iset}(x, name) <- value
\method{[[}{iset}(x, i) <- value
\method{[}{iset}(x, i = 1:(dim(x)[1]), j = 1:length(x))
\method{[}{iset}(x, i = 1:(dim(x)[1]), j = 1:length(x)) <- value
\method{names}{iset}(x) <- value
\method{names}{iset}(x)
\method{length}{iset}(x)
\method{dim}{iset}(x)
}
\arguments{
  \item{x}{iset object}
  \item{i}{row (case) index}
  \item{j}{column (variable) index}
  \item{value}{replacement value}
  \item{which}{An integer specifying an iSet ID or a string specifying a
    name of the iSet.}
  \item{name}{name of the variable}
}
\seealso{
  \code{\link{ivar}}, \code{\link{iset.cur}}
}
\examples{
data(iris)
s <- iset.new("iris", iris)
rm(iris)
s[]
names(s)
dim(s)
length(s)
s[[1]]
s$Species
s[1:5,c("Sepal.Length","Petal.Length")]
rm(s)
# even if we get rid of the reference object,
# we can get it back
s <- iset()
s
# see ?ivar for more examples
}
\keyword{iplot}
