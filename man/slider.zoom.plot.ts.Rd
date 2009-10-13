\name{slider.zoom.plot.ts}
\title{interactive zooming of time series}
\alias{slider.zoom.plot.ts}

\description{
  This function shows one or two sections of a time series. The window(s) is 
  (are) controlled by sliders. 
}
\usage{
  slider.zoom.plot.ts(x, n.windows, ...)
}
\arguments{
  \item{x}{ time series }
  \item{n.windows}{ \code{if(n.windows>1} two sections are defined }
  \item{\dots}{ additional graphical parameters }
}
\details{
  \code{slider.zoom.plot.ts} plots the original time series and it lets you 
  select one or two sections of the series by fixing the width(s) and the 
  starting point(s) of the region(s). Then the section(s) of the series is (are)
  plotted separately one below the other.

  The first slider defines the width of the section(s). 
  The second (third) one sets the start of the first (second) section.
}
\value{
  a message about the usage
}
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{plot}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.zoom.plot.ts(co2,2)
}
}
\keyword{ iplot }

