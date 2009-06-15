\name{slider functions}
\title{interactive brushing functions}
\alias{slider.brush.pairs}
\alias{slider.brush.plot.xy}

\description{
  These functions compute a pairs plot or a simple xy-plot and
  open a slider control widget for brushing. 

  \code{slider.brush.pairs} computes a pairs plot; the user defines an
  interval for one of the variables and in effect all data points 
  in this interval will be recolored.

  \code{slider.brush.plot.xy} computes an xy-plot; the user defines a
  interval for a third variable \code{z} and all points 
  \code{(x,y)} will be recolored red if the \code{z} value is in the interval.
}
\usage{
  slider.brush.pairs(data, ...)
  slider.brush.plot.xy(x, y, z, ...)
}
\arguments{
  \item{data}{ matrix or data frame to be used for plotting }
  \item{\dots}{ new settings for global graphics parameters }
  \item{x}{ matrix or data frame or vector }
  \item{y}{ vector of y values if \code{x} is not a matrix }
  \item{z}{ vector of z values if \code{x} is not a matrix }
}
\details{
  \code{slider.brush.pairs} draws a pairs plot of the data set \code{data}.
  The first slider defines the lower limit of the interval and the
  second its width. By the third slider a variable is selected.
  All data points for which the selected variable is in the interval
  are recolored red.

  \code{slider.brush.plot.xy} draws an xy-plot of the data set \code{data}.
  The first slider defines the lower limit of the interval of z values 
  and the second one its width. All data points for which the variable z
  is in the interval are recolored red.
}
\value{
  a message about the usage
}
\references{ W. S. Cleveland, R. A. Becker, and G. Weil. The Use of
  Brushing and Rotation for Data Analysis. InW. S. Cleveland
  and M. E. McGill, editors, Dynamic Graphics for
  Statistics. Wadsworth and Brooks/Cole, Pacific Grove,
  CA, 1988. }
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{pairs}}, \code{\link{plot}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.brush.pairs(iris)
}
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.brush.plot.xy(iris[,1:3])
}
}
\keyword{ iplot }

