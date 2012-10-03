\name{bagplot.pairs}
\alias{bagplot.pairs}
\title{ \code{pairs} plot with bagplots }
\description{
  \code{bagplot.pairs} calls \code{pairs} and use bagplot() as panel function. 
  It can be used for the inspection of data matrices.
}
\usage{
  bagplot.pairs(dm, trim = 0.0, main, numeric.only = TRUE, 
                factor = 3, approx.limit = 300, pch = 16, 
                cex = 0.8, precision = 1, col.loophull = "#aaccff",
                col.looppoints = "#3355ff", col.baghull = "#7799ff",
                col.bagpoints = "#000088", ...)
}
\arguments{
  \item{dm}{ datamatrix, columns contain values of the variables }
  \item{trim}{ fraction or vector of fractions of data points 
               that should be removed from the variables before computing }
  \item{main}{ title of the plot } 
  \item{numeric.only}{ if TRUE only numerical variables will be used. Otherwise an 
                       transformation to numeric will be performed.} 
  \item{factor}{ see help of bagplot }
  \item{approx.limit}{ see help of bagplot }
  \item{pch}{ see help of bagplot }
  \item{cex}{ see help of bagplot }
  \item{precision}{ see help of bagplot }
  \item{col.loophull}{ see help of bagplot }
  \item{col.looppoints}{ see help of bagplot }
  \item{col.baghull}{ see help of bagplot }
  \item{col.bagpoints}{ see help of bagplot }
  \item{\dots}{ further arguments to be passed to \code{pairs} }
}
\details{
  \code{bagplot.pairs} is a cover function which calls \code{pairs} and uses 
  \code{bagplot} to display the data.
}
\value{
  The data which has been used for the plot.
}
\author{Peter Wolf }
\note{
  Feel free to have a look inside of bagplot.pairs and 
  to improve it according to your ideas.
}
 \seealso{   \code{\link{bagplot}}, \code{\link{pairs}} }
\examples{
 # bagplot.pairs(freeny)
  # bagplot.pairs(trees,col.baghull="green", col.loophull="lightgreen")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ hplot }
