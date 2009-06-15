\name{slider functions}
\title{interactive splitting of time series}
\alias{slider.split.plot.ts}

\description{
  \code{slider.split.plot.ts} plots linear fitted lines or 
  summary statistics in sections of a time series. 
  The sections are controlled by sliders. 
}
\usage{
  slider.split.plot.ts(x, ...)
}
\arguments{
  \item{x}{ time series or vector}
  \item{\dots}{ additional graphics parameters }
}
\details{
  \code{slider.split.plot.ts} draws a time series plot and let you define 
  sections of the series by fixing a limit on the time scale as well as 
  a window width.
  The whole range of the series is partitioned in pieces of the same 
  length in a way that the fixed limit will be one of the section limits. 
  Then linear models are fitted and plotted in the sections.
  Alternatively -- by pressing the button \code{fivenum summary} -- 
  summary statistics are drawn instead of the model lines.

  The first slider fixes the width of the sections and 
  the second one the limit between two of them.

  By clicking on button \code{linear model} or \code{fivenum summary}
  the user switches between drawing model curves and five number summary.
}
\value{
  a message about the usage
}
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{plot}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.split.plot.ts(as.vector(sunspots)[1:100])
}
}
\keyword{ iplot }

