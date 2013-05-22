\name{iraster}
\alias{iraster}
\title{
  Add a bitmap (raster) image to the current iPlot.
}
\description{
  \code{iraster} adds a raster image as an iObj to the given iPlot.
  Position in the plot is specified by bottom-left and top-right points
  of the image.
}
\usage{
iraster(x1, y1, x2, y2, img, ..., plot = iplot.cur())
}
\arguments{
\item{x1, y1, x2, y2}{coordinates of the bottom-left (x1, y1) and
  top-right (x2, y2) corner. Alternatively, \code{x1} can be a vector of
  length 4 specifying the four values, or \code{x1} and \code{x2} can be
  vectors of length 2 specifying one point each}
\item{raster}{raster object to draw. Note that the current
  implementation simply calls \code{\link{writePNG}} on this object, so see
  the corresponding documentation for details}
\item{img}{image to draw. It can be either a raster, file name, binary
  connection or a raw vector containing an image in a common image
  format such as PNG or JPEG. If it is a raster, then the raster is first
  encoded into PNG format as passed as raw vector.}
\item{...}{additional arguments that will be passed to
  \code{\link{iobj.opt}} if present.}
\item{plot}{parent plot for the image}
}
\value{
  Resulting iObject.
}
\details{
  The current implementation uses Java's ImageIO API to read the
  image, so the supported formats will depend on your Java
  implemetation. Raster objects (i.e., of class \code{"raster"},
  \code{"nativeRaster"}), matrices and arrays) are simply passed to
  \code{png::writePNG(img)} so for anything other than computationally
  constructed objects it is more efficient to use the encoded image.
}
\seealso{
  \code{\link{iobj.opt}}
}
\examples{
## very silly example ...
iplot(0:20/20, 0:20/20)
## get a sample image (R logo) from the png package
fn <- system.file("img", "Rlogo.png", package="png")
## put this image behind all points
iraster(0, 0, 1, 1, fn, layer=-2)
## you can use a raster but it's less eficient
## this one goes to the top layer where iObjs reside normally
iraster(0, 0, 0.5, 0.5, png::readPNG(fn))
}
\keyword{hplot}
