\name{slider functions}
\title{interactive histogram and density traces}
\alias{slider.hist}
\alias{slider.density}

\description{
  The functions \code{slider.hist} and \code{slider.density}
  compute histograms and density traces 
  whereas some parameter are controlled by sliders. 

  \code{slider.hist} computes a histogram; the number of classes is
  defined by a slider.

  \code{slider.density} computes a density trace; width and 
  type of the kernel are defined by sliders.
}
\usage{
  slider.hist(x, panel, ...)
  slider.density(x, panel, ...)
}
\arguments{
  \item{x}{ data set to be used for plotting }
  \item{panel}{ function constructing additional graphical elements to the plot}
  \item{\dots}{ additional (graphics) parameters which are passed to 
                the invoked high level plotting function }
}
\details{
  \code{slider.hist} draws a histogram of the data set \code{x} by
  calling \code{hist} and opens a Tcl/Tk widget with one slider. 
  The slider defines the number of classes of the histogram. Changing the
  slider results in redrawing of the plot. For further
  details see the help page of \code{hist}. \code{rug} is used as the
  default panel function.

  \code{slider.density} draws a density trace of the data set \code{x}
  by \code{plot(density(...))} and opens a Tcl/Tk-widget with two 
  sliders. The first slider defines the width of the density trace 
  and the second one the kernel function: 
   \code{"1-gaussian",  "2-epanechnikov", "3-rectangular", 
         "4-triangular","5-biweight",     "6-cosine",     "7-optcosine"}
  Changing one of the sliders results in a redrawing of the plot. 
  For further details see the help page of \code{density}. 
  \code{rug} is used as the default panel function.
}
\value{
  a message about the usage
}
\references{ ~~ }
\author{ Hans Peter Wolf }
  \seealso{ \code{\link{hist}}, \code{slider}} 
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  slider.hist(log(islands))
}
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
slider.density(rivers,xlab="rivers",col="red")
}
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
slider.density(log(rivers),xlab="rivers",col="red",
  panel=function(x){
    xx<-seq(min(x),max(x),length=100)
    yy<-dnorm(xx,mean(x),sd(x))
    lines(xx,yy)
    rug(x)
    print(summary(yy))
  }
)
}
}
\keyword{ univar }
\keyword{ iplot }

