\name{spin3R}
\alias{spin3R}
\title{ spin3R }
\description{
  Simple spin function to rotate and to inspect 
  a 3-dimensional cloud of points
}
\usage{
  spin3R(x, alpha = 1, delay = 0.015)
}
\arguments{
  \item{x}{ \code{(nx3)}-matrix of points }
  \item{alpha}{ angle between successive projections }
  \item{delay}{ delay in seconds between two plots }
}
\details{
  \code{spin3R} computes two-dimensional projections 
  of \code{(nx3)}-matrix \code{x} and plots them
  on the graphics devise. The cloud of points is rotated
  step by step. The rotation is defined by a tcl/tk control 
  widget. \code{spin3R} requires tcl/tk package of R.
}
\references{ 
   Cleveland, W. S. / McGill, M. E. (1988): Dynamic Graphics
   for Statistics. Wadsworth & Brooks/Cole, Belmont, California.
}
\author{ Peter Wolf }
\note{  version 01/2003 }
\seealso{ \code{spin} of S-Plus }
\examples{
  xyz<-matrix(rnorm(300),100,3)
  # now start:     spin3R(xyz) 
}
\keyword{misc}

