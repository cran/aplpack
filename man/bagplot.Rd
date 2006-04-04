\name{bagplot}
\alias{bagplot}
\alias{compute.bagplot}
\alias{plot.bagplot}
\title{ bagplot, a bivariate boxplot }
\description{
  \code{compute.bagplot()} computes an object
  describing a bagplot of a bivariate data set. 
  \code{plot.bagplot()} plots a bagplot object. 
  \code{bagplot()} computes and plots a bagplot.
}
\usage{
bagplot(x, y, factor = 3, approx.limit = 300,  
       show.outlier = TRUE, show.whiskers = TRUE, 
       show.looppoints = TRUE, show.bagpoints = TRUE,
       show.loophull = TRUE, show.baghull = TRUE, 
       create.plot = TRUE, add = FALSE, pch = 16, cex = 0.4, 
       dkmethod = 2, precision = 1, verbose = FALSE, 
       debug.plots = "no",   col.loophull="#aaccff", 
       col.looppoints="#3355ff", col.baghull="#7799ff", 
       col.bagpoints="#000088", transparency=FALSE, ...
)

compute.bagplot(x, y, factor = 3, approx.limit = 300, 
       dkmethod = 2, precision = 1, verbose = FALSE, 
       debug.plots = "no")

plot.bagplot(x,  
       show.outlier = TRUE, show.whiskers = TRUE, 
       show.looppoints = TRUE, show.bagpoints = TRUE,
       show.loophull = TRUE, show.baghull = TRUE, 
       add = FALSE, pch = 16, cex = 0.4, verbose = FALSE, 
       col.loophull="#aaccff", col.looppoints="#3355ff", 
       col.baghull="#7799ff", col.bagpoints="#000088", 
       transparency=FALSE,...)
}

\arguments{
  \item{x}{ x values of a data set; 
     in \code{bagplot}: an object of class \code{bagplot}
     computed by \code{compute.bagplot} }
  \item{y}{ y values of the data set } 
  \item{factor}{ factor defining the loop }
  \item{approx.limit}{ precision of approximation, default: 300 }
  \item{show.outlier}{ if TRUE outlier are shown }
  \item{show.whiskers}{ if TRUE whiskers are shown }
  \item{show.looppoints}{ if TRUE loop points are plottet }
  \item{show.bagpoints}{ if TRUE bag points are plottet }
  \item{show.loophull}{ if TRUE the loop is plotted }
  \item{show.baghull}{ if TRUE the bag is plotted }
  \item{create.plot}{ if FALSE no plot is created }
  \item{add}{ if TRUE the bagplot is added to an existing plot }
  \item{pch}{ sets the plotting character }
  \item{cex}{ sets characters size}
  \item{dkmethod}{ 1 or 2, there are two method of  
     approximating the bag, 
currently under construction}
  \item{precision}{ precision of approximation, default: 1 }
  \item{verbose}{ automatic commenting of calculations 
 }
  \item{debug.plots}{ developers' tool for debugging }
  \item{col.loophull}{ color of loop hull }
  \item{col.looppoints}{ color of the points of the loop }
  \item{col.baghull}{ color of bag hull }
  \item{col.bagpoints}{ color of the points of the bag }
  \item{transparency}{ see section details }
  \item{\dots}{ additional graphical parameters }
}
\details{
A bagplot is a bivariate generalization of the well known
boxplot. It has been proposed by Rousseeuw, Ruts, and Tukey.
In the bivariate case the box of the boxplot changes to a 
convex hull, the bag of bagplot. In the bag are 50 percent
of all points. The fence separates points in the fence from 
points outside. It is computed by increasing the
the bag. The loop is defined as the convex polygon containing 
all points inside the fence. 
If all points are on a straight line you get a classical
boxplot.
\code{bagplot()} plots bagplots that are very similar 
to the one described in Rousseeuw et al. 
Remarks:
The two dimensional median is approximated.
There are known difficulties with small data sets 
(But I think it is not wise to make a (graphical) 
summary of e.g. 10 points.)

In case people want to plot multiple (overlappIng) bagplots, it is convenient if the plots are semi-transparent. For this reason
the \code{transparency} flag has been added to the bagplot
command. 
If \code{transparency==TRUE} the alpha layer is set to '99' (hex).
This causes the bagplots to appear semi-transparent, but ONLY if the output device is PDF and opened using:
\code{pdf(file="filename.pdf", version="1.4")}. 
For this reason, the default is \code{transparency==FALSE}. 
This feature as well as the arguments
to specify different colors has been proposed by Wouter Meuleman.
}
\value{
  \code{compute.bagplot} returns an object of class
  \code{bagplot} that could be plotted by 
  \code{plot.bagplot()}.
}
\references{ P. J. Rousseeuw, I. Ruts, J. W. Tukey (1999):
    The bagplot: a bivariate boxplot, The American
    Statistician, vol. 53, no. 4, 382--387 }
\author{ Peter Wolf }
\note{
  The development of the function has not been finished. 
  Version 02/2006 }
\seealso{ \code{\link[graphics]{boxplot}} }
\examples{
  # example: 100 random points and one outlier
  dat<-cbind(rnorm(100)+100,rnorm(100)+300)
  dat<-rbind(dat,c(105,295))
  bagplot(dat,factor=2.5,create.plot=TRUE,approx.limit=300,
     show.outlier=TRUE,show.looppoints=TRUE,
     show.bagpoints=TRUE,dkmethod=2,
     show.whiskers=TRUE,show.loophull=TRUE,
     show.baghull=TRUE,verbose=FALSE)
  # example of Rousseeuw et al., see R-package rpart
  cardata <- structure(as.integer(c(2560, 2345, 1845, 2260, 2440,
   2285, 2275, 2350, 2295, 1900, 2390, 2075, 2330, 3320, 2885,
   3310, 2695, 2170, 2710, 2775, 2840, 2485, 2670, 2640, 2655,
   3065, 2750, 2920, 2780, 2745, 3110, 2920, 2645, 2575, 2935,
   2920, 2985, 3265, 2880, 2975, 3450, 3145, 3190, 3610, 2885,
   3480, 3200, 2765, 3220, 3480, 3325, 3855, 3850, 3195, 3735,
   3665, 3735, 3415, 3185, 3690, 97, 114, 81, 91, 113, 97, 97,
   98, 109, 73, 97, 89, 109, 305, 153, 302, 133, 97, 125, 146,
   107, 109, 121, 151, 133, 181, 141, 132, 133, 122, 181, 146,
   151, 116, 135, 122, 141, 163, 151, 153, 202, 180, 182, 232,
   143, 180, 180, 151, 189, 180, 231, 305, 302, 151, 202, 182,
   181, 143, 146, 146)), .Dim = as.integer(c(60, 2)), 
   .Dimnames = list(NULL, c("Weight", "Disp.")))
  bagplot(cardata,factor=3,show.baghull=TRUE,
    show.loophull=TRUE,precision=1,dkmethod=2)
  title("car data Chambers/Hastie 1992")
  # points of y=x*x
  bagplot(x=1:30,y=(1:30)^2,verbose=FALSE,dkmethod=2)
  # one dimensional subspace
  bagplot(x=1:100,y=1:100)
}
\keyword{ misc }


