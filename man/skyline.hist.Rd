\name{skyline.hist}
\alias{skyline.hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  \code{skyline.hist} computes a skyline plot which is special histogram.
}
\description{
  The function \code{skyline.hist} draws several histograms in one plot. The 
  resulting image may look like a skyline.
}
\usage{
   skyline.hist(x, n.class, n.hist = 1, main, ylab="density", 
                night = FALSE, col.bars = NA, col.border = 4, lwd.border = 2.5,
                n.shading = 6, lwd.shading = 2, col.shading = NA, lty.shading = 3,
                pcol.data = "green", cex.data = 0.3, pch.data = 16, col.data = 1,
                lwd.data = .2, permutation = FALSE, 
                xlab, xlim, ylim, new.plot=TRUE, bty="n", ...)
}
\arguments{
  \item{x}{ one dimensional data set.}
  \item{n.class}{
    number of classes that should be used to find the width of the bars 
    of the histogram(s).}
  \item{n.hist}{
    number of histograms that should be plotted.}
  \item{main}{
    used for call of \code{title}.}
  \item{ylab}{
    text for y axis.}
  \item{night}{
    If \code{TRUE} the background will be colored blue. 
    If \code{FALSE} there will be no colored background. Otherwise 
    \code{night} is used as background color.}
  \item{col.bars}{
    defines the color of the bars. If \code{is.na(col.bars)} and 
    \code{night==TRUE} the bars will be colored gray. }
  \item{col.border}{  color of the borders of the bars.}
  \item{lwd.border}{  line width of the borders of the bars.}
  \item{n.shading}{
    number of vertical lines for filling the bars of the histograms.}
  \item{lwd.shading}{
    line width of the vertical lines for shading the bars. }
  \item{col.shading}{
    color for the vertical lines for shading. If \code{NA} heat colors are used.}
  \item{lty.shading}{
    line type for the vertical lines for shading.}
  \item{pcol.data}{ color of data points.}
  \item{cex.data}{ character size of plotting character.}
  \item{pch.data}{ plotting character of data points.}
  \item{lwd.data}{ line width for segments between data points.}
  \item{col.data}{ color for segments between data points.}
  \item{permutation}{ if not \code{FALSE} a permutation of the data set is erformed.}
  \item{xlab}{     text for y axis. }
  \item{xlim}{     range of x. }
  \item{ylim}{     range of y. }
  \item{new.plot}{    logical. If \code{TRUE} a new plot is constructed.}
  \item{bty}{     box type, used by \code{plot}. }
  \item{\dots}{     further graphical parameters passed to plot. }
}
\details{
  \code{skyline.hist} computes several histograms and plots them one upon
  the other. The histograms differ in the positions of the first cells, 
  but all cells have the same width. The parameters \code{n.class} and
  \code{n.hist} have the greatest effect on the design of the result. 
  \code{col.border} allows to color the border of the rectangular boxes of the 
  histogram bars. \code{col.bars} defines the fill color of the bars. 
  \code{n.shading} defines the number of vertical lines of type
  \code{lty.shading} and width \code{lwd.shading} that are drawn within the boxes. 
  Another feature of \code{skyline.hist} is to represent the data points.
  The data points of a cell are plotted according their x-values and
  their ranks (within the points of the cell). The resulting points are connected
  by line segments and you will see a time series running from bottom to top
  in each cell. The points and lines can be specified by \code{pcol.data},
  \code{cex.data}, \code{pch.data}, \code{lwd.data}, \code{col.data}. To get rid 
  of the original order of the data you can permutated them (\code{permutation=1}).
  The "skyline" of the plot may be similar to the skyline of a town and the 
  vertical lines may look like small windows of buildings.
  In Young et. al. you find "shaded histograms". These histograms have triggered
  the idea of \code{skyline.hist} and the representation of a one dimensional 
  data set by laying histograms on top of otheroverlied histograms.
}
\value{
  The result of a call of hist is returned.
}
\references{
  F.W. Young, R.M. Valero-Mora, M. Friendly (2006): Visual Statistics. 
  Wiley, p207--208.
}
\author{
  Peter Wolf, pwolf@wiwi.uni-bielefeld.de
}
 \seealso{
   \code{\link{hist}}, \code{\link{density}}
}
\examples{
  # dev.off()
print(par())
  par(mfrow=c(1,1))
  for(n.c in c(2,4,8)){  # some values for n.class
    for(n.h in c(2,4,3)){# some values for number of n.hist
        n.s <- 9         # value for number of vertical lines
        skyline.hist(co2, n.shading = n.s, n.hist = n.h ,n.class = n.c, 
                     night = n.h==3, col.border = n.h!=4)
    }
  }
  par(mfrow = c(1,1))
  skyline.hist(x=rivers, n.class=4, n.hist=2, n.shading=0, main="rivers",
             cex.data=.5, lwd.data = .2, col.data = "green", pcol.data = "red",
             col.border=NA, night=FALSE, ylab="density")
  skyline.hist(x=rivers, n.class=4, n.hist=5, n.shading=0, main="rivers",
             cex.data=.5, lwd.data = 1, col.data = "green", pcol.data = "red",
             col.border=NA, night="blue" , ylab="density", col.bars =NA)
  skyline.hist(x=rivers, n.class=10, n.hist=2, n.shading=0, main="rivers",
             cex.data=.5, lwd.data = 1, col.data = "green", pcol.data = "red",
             col.border=NA, night=FALSE , ylab="density", col.bars = "lightblue")
  skyline.hist(x=rivers, n.class=10, n.hist=1, n.shading=0, main="rivers",
             cex.data=1, lwd.data = 0, col.data = "green", pcol.data = "red",
             col.border=NA, night=FALSE , ylab="density", col.bars = "lightblue" )
  skyline.hist(x=rivers, n.class=6, n.hist=1, n.shading=0, main="rivers",
             cex.data=0.1, lwd.data = 2, col.data = "red", pcol.data = "green",
             night="orange" , ylab="density", col.bars = "white", col.border=1 )
  skyline.hist(x=rivers, n.class=6, n.hist=1, n.shading=0, main="rivers",
             cex.data=0.1, lwd.data = 2, col.data = "red", pcol.data = "green",
             col.border=NA, night=FALSE , ylab="density", col.bars = "lightblue")
  skyline.hist(x=rivers, n.class=6, n.hist=1, n.shading=5, col.shading = "blue",
             main="rivers",
             cex.data=0.1, lwd.data = 1, col.data = "black", pcol.data = "green",
             col.border=NA, night=FALSE , ylab="density", col.bars = "green")
  skyline.hist(x=rivers, n.class=6, n.hist=3, n.shading=5, col.shading = "blue",
             main="rivers", col.bars = "green",
             cex.data=0.1, lwd.data = 1, col.data = "black", pcol.data = "green",
             col.border="white", night="magenta" , ylab="density")
  skyline.hist(x=rivers, n.class=6, n.hist=4, n.shading=5, col.shading = "blue",
             main="rivers",
             cex.data=0.8, lwd.data = 1, col.data = "blue", pcol.data = "red",
             col.border=NA, night=FALSE , ylab="density", col.bars = "green")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ hplot }
