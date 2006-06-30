\name{ivar}
\alias{ivar}
\alias{ivar.data}
\alias{ivar.new}
\alias{ivar.update}
\alias{iset.df}
\alias{iset.updateVars}
\title{
  Data (ivar) manipulation functions
}
\description{
  The following functions are used to mainpulate variables for iplots:

  \code{ivar.data} returns the data associated with an \code{iVar}
  variable

  \code{ivar.new} creates a new \code{iVar} variable in the current
  \code{iSet} using the supplied data

  \code{ivar.update} replaces the content of a variable

  \code{iset.df} pushes an entire data frame into corresponding
  \code{iVar} variables

  \code{iset.updateVars} notifies all plots that variables have been
  changed. This function should not be normally used, because
  notification happens automatically, unless batch updates are
  performed.
}
\usage{
ivar.data(var)
ivar.new(name = deparse(substitute(cont)), cont)
ivar.update(var, cont, batch = FALSE)
iset.df(df)
iset.updateVars()
}
\arguments{
  \item{var}{an existing \code{iVar} variable}
  \item{cont}{desired contents - usually a numeric vector or a factor}
  \item{name}{name of the newly created variable as it will appear in
    the \code{iSet} and all plots}
  \item{batch}{if set to \code{TRUE} then plots are not automatically
  notified about the change. This allows an update of multiple variables
  in batches without constant re-painting of the plots.}
  \item{df}{a data frame whose columns will be converted into
    \code{iVar}s}
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
  variables beforehand. \code{iset.df} can be used to convert an entire
  data frame into \code{iVar}s. It is much faster to use \code{iVar}s in
  the plots instead of the raw data.

  Once an \code{iVar} is created, it is merely a reference to the data
  in the \code{iSer}. The \code{ivar.data} function can be used to
  retrieve that data.

  In addition, it is possible to update the contents of an \code{iVar}
  using the \code{ivar.update} function. Once the variable is updated,
  all plots that use the variable will be updated, too, and reflect the
  change, unless \code{batch} was set to \code{TRUE}. The notification
  is performed using the \code{iset.updateVars} function.
}
\seealso{
  \code{\link{iset.df}}, \code{\link{ibar}}, \code{\link{iplot.list}},
  \code{\link{iplot.opt}}
}
%\examples{
%}
\keyword{hplot}
