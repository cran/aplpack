\name{plothulls}
\alias{plothulls}
\title{plothulls for data peeling }
\description{
  \code{plothulls plots convex hulls of a bivariate data set.}
}
\usage{
plothulls(x, y, fraction, n.hull = 1, main, add = FALSE, col.hull, 
    lty.hull, lwd.hull, density = 0, ...) 
}
\arguments{
  \item{x}{ two column matrix of the coordinates of points of x-values of a data set} 
  \item{y}{ if x is one dimensional then y contains the y-values of the data set} 
  \item{fraction}{ ... of points that lies inside the hull to be plotted} 
  \item{n.hull}{ number of directions sequential hulls to be plotted}
  \item{main}{ title for the graphics}
  \item{add}{ if TRUE no new plot is initialized}
  \item{col.hull}{ color(s) of the hull(s)}
  \item{lty.hull}{ line type(s) of the hull(s)}
  \item{lwd.hull}{ line width(s) of the hull(s)}
  \item{density}{ density argument of polygon() that draws the hulls}
  \item{...}{ further arguments used in the call of plot() or points()}
}
\details{
The function \code{plothulls} computes hulls of a bivariate data set using the 
function \code{chull}. After finding a hull the hull maybe plotted. 
Then the data points of the hull will be removed and 
the hull of the remaining points is computed. 
The style of plotting a hull depends on the setting of 
\code{col.hull}, \code{lty.hull}, \code{lwd.hull} and \code{density}.
\code{density=NA} has the effect that the regions of the hulls are filled by a color.
Using \code{fraction} you can plot a single hull. 
\code{n.hull} defines the number of hull that should be drawn one after the other.
}
\value{
The  hull(s) are stored as a list of matrices with two columns, 
the innermost first and so on. 
}
\references{ 
  Green, P.J. (1981): Peeling bivariate data. 
  In: Interpreting Multivariate Data, V. Barnett (ed.), pp 3-19, Wiley.
  Porzio, Giovanni C., Ragozini, Giancarlo (2000): 
  Peeling multvariate data sets: a new approach. Quanderni di Statistica, Vol. 2.
}
\author{ Peter Wolf }
\note{
  Version of plothulls: 10/2013 }
 \seealso{ \code{\link[aplpack]{bagplot}} }
\examples{
  # 10 hulls computed from the faithful data and plotted
  plothulls(faithful, n.hull=10, lty.hull=1)
  # plotting additionally a hull with 90 percent of points within the hull
  plot(faithful)
  plothulls(faithful, fraction=.90, add=TRUE, col.hull="red", lwd.hull=3)
  # hull with 10 percent of points within the hull 
  plothulls(faithful, fraction=.10, col.hull="red", lwd.hull=3)
  # first 3 hulls of the cars data set
  n <- 3
  plothulls(cars, n.hull=n, col.hull=1:n, lty.hull=1:n)
  # 5 hulls represented by colored regions
  n <- 5
  cols <- heat.colors(9)[3:(3+n-1)]
  plothulls(cars, n.hull=n, col.hull=cols, lty.hull=1:n, density=NA, col=0)
  points(cars, pch=17, cex=1)
  # 6 hulls: regions colored and boundaries shown
  n <- 6
  cols <- rainbow(n)
  plothulls(cars, n.hull=n, col.hull=cols, lty.hull=1:n, density=NA, col=0)
  plothulls(cars, n.hull=n, add=TRUE, col.hull=1, lwd.hull=2, lty=1, col=0)
}
