\name{ievent.wait}
\alias{ievent.wait}
\title{
  Interactive events
}
\description{
  This function provides a way of building interactive event loops in
  R. Currently SJava interface has no stable callbacks, therefore this
  is the only way of defining new interactions in iPlots.
  
  \code{\link{ievent.wait}} waits until some iPlots event (iEvent)
  occurs. The most commonly used events include selection change and
  the "break" event. It is possible to generate user-definable events
  (such as events sent by custom buttons or menu entries) with low-level
  API (via SJava)
  
  Note that in the current implementation the Windows GUI "freezes" until the
  function returns.
}
\usage{
ievent.wait()
}
\value{
  \code{NULL} is the \code{break} event occured or a \code{ievent}
  object if some other event occured.
}
\examples{
  d<-iplot.data()

  iabline(lm(d$y ~ d$x), col = "red")
  ilines(lowess(d$x,d$y), col = "#0000c0")
  ilines(c(0,0),c(0,0), col = "marked", visible = FALSE)

  while (!is.null(ievent.wait())) {
    if (iset.sel.changed()) {
      s <- iset.selected()
      if (length(s) > 1) 
        iobj.opt(x=lowess(d$x[s],d$y[s]),visible = TRUE)
      else iobj.opt(visible = FALSE)
    }
  }
  for(i in 1:3) iobj.rm()
}
\keyword{hplot}
