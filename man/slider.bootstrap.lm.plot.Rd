\name{slider.bootstrap.lm.plot}
\title{interactive bootstapping for lm}
\alias{slider.bootstrap.lm.plot}

\description{
  \code{slider.bootstrap.lm.plot} computes a scatterplot and 
  adds regression curves of samples of the data points. 
  The number of samples and the degree of the model are 
  controlled by sliders. 
}
\usage{
  slider.bootstrap.lm.plot(x, y, ...)
}
\arguments{
  \item{x}{ two column matrix or vector of x values if y is used }
  \item{y}{ y values if x is not a matrix }
  \item{\dots}{ additional graphics parameters }
}
\details{
  \code{slider.bootstrap.lm.plot} draws a scatterplot of the data points
  and fits a linear model to the data set. Regression curves  
  of samples of the data are then added to the plot. Within a Tcl/Tk 
  control widget the degree of the model, the repetitions and the start
  of the random seed are set. After modification of a parameter
  the plot is updated.
}
\value{
  a message about the usage
}
\references{ ~~ }
\author{ Hans Peter Wolf }
 \seealso{ \code{\link{plot}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  daten<-iris[,2:3]
  slider.bootstrap.lm.plot(daten)
}
}
\keyword{ iplot }
