\name{iobj.opt}
\alias{iobj.opt}
\title{
  Modify parameters of an iObject
}
\description{
  This function modifies parameters of an iObject.
}
\usage{
iobj.opt(o=iobj.cur(),...)
}
\arguments{
  \item{o}{object whose options are to be set}
  \item{...}{options to be set}
}
\value{
  If no parameters (except for the object) are specified, a list of the
  current parameters is returned (if supported by the object).
  
  The following common parameters are used by most iObjects:
  
  \code{visible} visibility flag. The default is \code{TRUE}.
  
  \code{layer} layer in which the object is placed. Currently the
  following layers are supported: 0 - background, 1 - data points, 2 -
  selection, 3 - drag boxes. The value -1 has a special meaning by
  denoting the topmost layer (which is the default)
  
  \code{col} drawing color of the object
  
  \code{fill} filling color of the object (where applicable). If set to
  \code{NA} then no filling is performed.
  
  \code{coord} coordinates system to use. 0=graphical coordinates,
  1=data space coordinates (default), 2=relative coordinates.
  
  \code{update} if set to \code{FALSE} then no plot refresh is done
  after updating the options. When modifying multiple objects it is
  common practice to set \code{update} to \code{FALSE} for all but the last
  updated object. The default is \code{TRUE}.
}
\seealso{
  \code{\link{iobj.list}}
}
\keyword{iplot}
