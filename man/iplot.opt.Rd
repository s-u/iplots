\name{iplot.opt}
\alias{iplot.opt}
\title{
  Modify parameters of an interactive plot
}
\description{
  This function modifies parameters of an iPlot.
}
\usage{
iplot.opt(..., plot=iplot.cur())
}
\arguments{
  \item{...}{Parameters to modify}
  \item{plot}{Plot whose parameters are to be modified}
}

\details{
  The following parameters are common to all plots. See help pages of
  individual plots for plot-specific parameters.
  \describe{
    \item{\code{xlim}}{Range of the X axis (vector of two numbers).}
    \item{\code{ylim}}{Range of the Y axis (vector of two numbers).}
    \item{\code{col}}{Colors of the points. See \code{\link{iset.brush}} for
      details.}
    \item{\code{autoAdjustMargins}}{Whether iPlots should try to determine appropriate
      margin sizes. Set this to FALSE if you want to set the margins manually.}
    \item{\code{defaultMargins}}{Vector of default values for left, right, top and bottom margins.}
    \item{\code{mLeft}}{Size of left margin.}
    \item{\code{mRight}}{Size of right margin.}
    \item{\code{mTop}}{Size of top margin.}
    \item{\code{mBottom}}{Size of bottom margin.}
    \item{\code{fillColor}}{Fill color.}
    \item{\code{borderColor}}{Border color.}
    \item{\code{fillColorSel}}{Fill color when selected.}
    \item{\code{borderColorSel}}{Border color when selected.}
    \item{\code{fillColorDrag}}{Fill color of dragged bars.}
    \item{\code{COL_INVALID}}{Color of invalid elements.}
    \item{\code{COL_OUTLINE}}{Default line color.}
    \item{\code{COL_SELBG}}{Background color of selection rectangle. Defaults to selection color with alpha value 0.298.}
    \item{\code{COL_ZOOMBG}}{Background color of selection rectangle.}
    \item{\code{horizontalMedDist}}{Mean horizontal label distance.}
    \item{\code{horizontalMinDist}}{Minimal horizontal label distance.}
    \item{\code{verticalMedDist}}{Mean vertical label distance.}
    \item{\code{verticalMinDist}}{Minimal vertical label distance.}
    \item{\code{extQueryString}}{Extended query string.}
    \item{\code{rotateYLabels}}{Whether labels for the y axis should be rotated by the angle given by rotateYLabelsBy.}
    \item{\code{rotateYLabelsBy}}{The labels for the y axis are rotated by this amount of degrees if rotateYLabels is set to true.}
    \item{\code{title}}{Frame title.}
  }
}
\value{
  If no parameters (except for the plot) are specified, a list of the
  current parameters is returned.
}
\seealso{
  \code{\link{iplot.list}}
}
\examples{
data(iris)
attach(iris)
iplot(Sepal.Width,Petal.Width)
iplot.opt(xlim=c(1.5,5),col=Species)
}
\keyword{hplot}
