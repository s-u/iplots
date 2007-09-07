\name{ivar}
\alias{ivar}
\alias{ivar.data}
\alias{ivar.new}
\alias{ivar.new.map}
\alias{ivar.update}
\alias{iset.updateVars}
\alias{[.ivar}
\alias{[<-.ivar}
\alias{length.ivar}
\alias{print.ivar}
\alias{show,ivar-method}
\title{
  Data (ivar) manipulation functions
}
\description{
  The following functions are used to mainpulate variables for iplots:

  \code{ivar.data} returns the data associated with an \code{iVar}
  variable

  \code{ivar.new} creates a new \code{iVar} variable in the current
  \code{iSet} using the supplied data

  \code{ivar.new.map} same as \code{ivar.new} but for map (polygon) data

  \code{ivar.update} replaces the content of a variable

  \code{iVar} variables (deprecated - use \code{\link{iset.new}} instead)

  \code{[.ivar} allows data subsetting of ivar contents

  \code{[<-.ivar} allows partial replacement of ivar contents

  \code{length.ivar} returns the length of a variable
  
  \code{iset.updateVars} notifies all plots that variables have been
  changed. This function should not be normally used, because
  notification happens automatically, unless batch updates are
  performed.
}
\usage{
ivar.data(var)
ivar.new(name = deparse(substitute(cont)), cont)
ivar.new.map(name, x, y)
ivar.update(var, cont, batch = FALSE)
\method{length}{ivar}(x)
\method{[}{ivar}(x, i, ...)
\method{[}{ivar}(x, ...) <- value
\method{print}{ivar}(x, ...)

iset.updateVars()
}
\arguments{
  \item{var}{an existing \code{iVar} variable}
  \item{cont}{desired contents - usually a numeric vector or a factor}
  \item{name}{variable name in an iSet}
  \item{batch}{if set to \code{TRUE} then plots are not automatically
  notified about the change. This allows an update of multiple variables
  in batches without constant re-painting of the plots.}
  \item{x}{object to subset or x-coordinate of the map data}
  \item{y}{y-coordinate of the map data}
  \item{i}{index - if missing all data are returned}
  \item{...}{indices}
  \item{value}{replacement value}
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "ivar")}: is mapped to
      \code{print.ivar}}
  }
}
\details{
  All data that will are displayed in an iPlot are organized in
  \code{iSet}s. Each \code{iSet} contains variables, called
  \code{iVar}s, that represent the displayed data. Typically an
  \code{iSet} corresponds to a data frame and an \code{iVar} corresponds
  to a column in a data frame.  All variables in one \code{iSet} must
  have the same length and indexing order. This allows iPlots to perform
  proper linking of all plots.

  Before some data can be displayed in an iPlot, they are put into an
  \code{ivar} using the \code{ivar.new} function. Each variable has a
  name that is unique within an \code{iSet}. This process is automatic
  if you pass arbitrary vectors to the iPlots plotting
  functions. However, it is possible (and desirable) to register
  variables beforehand using \code{\link{iset.new}}. It is much faster
  to use \code{iVar}s in the plots instead of the raw data.

  Once an \code{iVar} is created, it is merely a reference to the data
  in the \code{iSet}. The regular subsetting operators \code{[} or
  \code{ivar.data} function can be used to retrieve the data back to R.

  In addition, it is possible to update the contents of an \code{iVar}
  using the \code{ivar.update} function. Once the variable is updated,
  all plots that use the variable will be updated, too, and reflect the
  change, unless \code{batch} was set to \code{TRUE}. The notification
  is performed using the \code{iset.updateVars} function.
}
\seealso{
  \code{\link{iset.new}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
\examples{
data(iris)
s <- iset.new("iris", iris)
rm(iris) # we don't need iris anymore
s[1:5,]
s$Species
s$Species[]
s[1:5,c("Sepal.Width","Sepal.Length")]
s$Test <- factor(c("yes","no")[runif(dim(s)[1])+1.5])
s[1:5,]
iplot(s$Sepal.Length, s$Petal.Length)
iset.select(s$Test[]=="yes")
}
\keyword{hplot}
