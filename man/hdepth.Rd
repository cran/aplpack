\name{hdepth}
\alias{hdepth}
\title{ hdepth of points }
\description{
  \code{hdepth()} computes the h-depths of points.
}
\usage{
hdepth(tp, data, number.of.directions=181)
}
\arguments{
  \item{tp}{ two column matrix of the coordinates of points which h-depths are needed } 
  \item{data}{ two column matrix of the coordinates of the points of a data set} 
  \item{number.of.directions}{ number of directions to be checked }
}
\details{
The function \code{hdepth} computes the h-depths of the points
\code{tp} relative to data set \code{data}. If \code{data} is 
missing \code{tp} will also be taken as data set.
}
\value{
the h-depths of the test points
}
\author{ Peter Wolf }
\note{
  Version of bagplot: 12/2012 }
 \seealso{ \code{\link[aplpack]{bagplot}} }
\examples{
  # computation of h-depths
  data <- cbind(rnorm(40), rnorm(40)) 
  xy <- cbind(runif(50,-2,2),runif(50,-2,2))
  bagplot(data); text(xy, as.character(hdepth(xy,data)))
}
