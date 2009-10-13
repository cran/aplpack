\name{slider.lowess.plot}
\title{interactive lowess smoothing}
\alias{slider.lowess.plot}
\description{
  \code{slider.lowess.plot} computes an xy-plot of the data and 
  adds LOWESS lines. The smoother
  span and the number of iterations are selected by sliders.
}
\usage{
  slider.lowess.plot(x, y, ...)
}
\arguments{
  \item{x}{ data set to be used for plotting or vector of x values }
  \item{y}{ vector of y values in case \code{x} is not a matrix }
  \item{\dots}{ additional (graphics) parameter settings }
}
\details{
  \code{slider.lowess.plot} computes a scatterplot of the data.
  Then a LOWESS smoother line is added to the plot.   
  For more details about the lowess parameters \code{f} and \code{iter}
  take a look at the help page of \code{lowess}.
  The parameters are set by moving sliders of the 
  control widget. The first slider defines the smoother span \code{f}
  and the second one the number of iterations.
}
\value{
  a message about the usage
}
\references{ for references see help file of \code{lowess} }
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{lowess}}, \code{slider} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.lowess.plot(cars)
}
}
\keyword{ iplot }


