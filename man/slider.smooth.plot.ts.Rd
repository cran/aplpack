\name{slider functions}
\title{interactive Tukey smoothing}
\alias{slider.smooth.plot.ts}

\description{
  \code{slider.smooth.plot.ts} computes smooth curves
  of a time series plot by Tukey's smoothers.
  The kind of smoothing is controlled by a Tcl/Tk widget. 
}
\usage{
  slider.smooth.plot.ts(x, ...)
}
\arguments{
  \item{x}{ time series }
  \item{\dots}{ additional graphical parameters }
}
\details{
  \code{slider.smooth.plot.ts} draws the time series \code{x}.
  The user selects a filter of the set 
  \code{c("3RS3R", "3RSS", "3RSR", "3R", "3", "S")}
  step by step and the resulting curve is added to the plot.
  The selection is performed by pressing a button of the control
  widget of \code{slider.smooth.plot.ts}. 
  The button \code{reset} restarts the smoothing process.
}
\value{
  a message about the usage
}
\references{  Tukey, J. W. (1977). Exploratory Data Analysis, Reading
     Massachusetts: Addison-Wesley.
}
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{plot}}, \code{\link{smooth}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.smooth.plot.ts(rnorm(100))
}
}
\keyword{ iplot }

