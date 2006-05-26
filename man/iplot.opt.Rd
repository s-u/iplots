\name{iplot.opt}
\alias{iplot.opt}
\title{
  Modify parameters of an interactive plot
}
\description{
  This function modifies parameters of an iPlot.
}
\usage{
iplot.opt(plot=iplot.cur(), autoAdjustMargins=TRUE,
 fillColorDrag=rgb(128,128,128,70,max=255), COL_INVALID="red,
 COL_OUTLINE="black", COL_ZOOMBG=rgb(0,0.3,1,0.25), horizontalMedDist=50,
 horizontalMinDist=35, verticalMedDist=30, verticalMinDist=18,
 extQueryString="", rotateYLabels=TRUE, rotateYLabelsBy=270, ...)
}
\arguments{
  \item{plot}{Plot whose parameters are to be modified.}
  \item{xlim}{Range of the X axis (vector of two numbers).}
  \item{ylim}{Range of the Y axis (vector of two numbers).}
  \item{col}{Colors of the points. See \code{\link{iset.brush}} for
    details.}
  \item{autoAdjustMargins}{Whether iPlots should try to determine appropriate
    margin sizes. Set this to FALSE if you want to set the margins manually.}
  \item{defaultMargins}{Vector of default values for left, right, top and bottom margins.}
  \item{mLeft}{Size of left margin.}
  \item{mRight}{Size of right margin.}
  \item{mTop}{Size of top margin.}
  \item{mBottom}{Size of bottom margin.}
  \item{fillColor}{Fill color.}
  \item{borderColor}{Border color.}
  \item{fillColorSel}{Fill color when selected.}
  \item{borderColorSel}{Border color when selected.}
  \item{fillColorDrag}{Fill color of dragged bars.}
  \item{COL_INVALID}{Color of invalid elements.}
  \item{COL_OUTLINE}{Default line color.}
  \item{COL_SELBG}{Background color of selection rectangle. Defaults to selection color with alpha value 0.298.}
  \item{COL_ZOOMBG}{Background color of selection rectangle.}
  \item{horizontalMedDist}{Mean horizontal label distance.}
  \item{horizontalMinDist}{Minimal horizontal label distance.}
  \item{verticalMedDist}{Mean vertical label distance.}
  \item{verticalMinDist}{Minimal vertical label distance.}
  \item{extQueryString}{Extended query string.}
  \item{rotateYLabels}{Whether labels for the y axis should be rotated by the angle given by rotateYLabelsBy.}
  \item{rotateYLabelsBy}{The labels for the y axis are rotated by this amount of degrees if rotateYLabels is set to true.}
  \item{title}{Frame title.}
  \item{...}{Plot specific parameters.}
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
